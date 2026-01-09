library(simsalapar)
library(doParallel)
library(foreach)
library(parallel)


#IMPORTANT NOTE:
# " we will assume that we can perfectly observe the stock without error"

# Focusing on this example first in my project

# TODO: Struggling with compute time as I believe I can only run one simulation at once, even if I find eight points to evaluate next
# What I would need to do is find these eight points and then run the simualtions for them in eight different "places" at the same time
# and be able to get the results from all of these and plug them into my following code
# which I have a brief idea of how to do in Python but not in R
# TODO: I think this means I will need the Viking computers or to look at running multiple "places" in R

## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)
library(DiceKriging)


obj_func <- function(f_cod, f_had, mixedfishery_MixME_om, stk_oem) {
  
  # Create the main MixME input object
  # This bundles together all the components needed to run the MSE
  # Set management type to fixedF
  input <- makeMixME(om = mixedfishery_MixME_om, catch_obs = stk_oem, management_lag = 0, management_type = "fixedF", parallel = FALSE)
  
  # Set up the observation error model
  # Cod and haddock catches occur at the start of the year so timing=0
  # TODO: Not very realistic - could consider changing
  input$oem@args$catch_timing$cod <- 0
  input$oem@args$catch_timing$had <- 0
  
  # Define the age range for calculating average fishing mortality (Fbar) for both stocks
  input$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
  input$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)
  
  
  # Set the target fishing mortality for both stocks as the next point decided by the algorithm
  input$ctrl_obj$hcr@args$ftrg$cod <- f_cod
  input$ctrl_obj$hcr@args$ftrg$had <- f_had
  
  ## Update fbar ranges
  # TODO: From old script, may not be needed here
  input$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
  input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)
  
  
  #RUN MIXME SIMULATION
  res <- runMixME(om  = input$om, oem = input$oem, ctrl_obj = input$ctrl_obj, args = input$args)
  
  # TODO: Remove checks? Helpful but not needed
  
  ## Check for advice failure
  apply(res$tracking$iterfail, 1, mean)
  
  ## Check for effort optimisation failure
  res$tracking$optim
  
  ## Check for effort optimisation message
  res$tracking$message
  
  ## Check quota uptake for cod and haddock
  res$tracking$uptake["cod",,,1]
  res$tracking$uptake["had",,,1]
  
  ## Check maximum overshoot of the quota by fleets
  max(res$tracking$overquota, na.rm = TRUE)
  
  ## Define Blim for each stock - required for way calculating risk right now
  hcrpars <- list(cod = c(Blim = 107000), had = c(Blim = 9227))  # https://doi.org/10.17895/ices.advice.5897
  
  # Update the control object with the new HCR parameters
  res$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = hcrpars))
  
  return(res)
  
}

# Calculate how correlated two pairs of Fcod and Fhad are based on their distance in the grid
cov_exp <- function(X1, X2, theta, sigma2) {
  n1 <- nrow(X1)
  n2 <- nrow(X2)  
  # pairwise difference weighted by theta
  D <- array(0, dim = c(n1, n2)) 
  for (j in seq_along(theta)) {
    D <- D + theta[j] * abs(outer(X1[, j], X2[, j], "-"))
  } 
  sigma2 * exp(-D)
}

knowledge_gradient_sim <- function(mu, sigma, model, obs_noise_var = 0, nsim = 100, prisk_cod, prisk_had, eps = 1e-4) 
{ 
  X_pred <- dat[, c("Fcod","Fhad")]
  # Get correlation matrix for the whole design space of Fcod and Fhad
  cov_grid <- cov_exp(X_pred, X_pred, theta = model@covariance@range.val, sigma2 = model@covariance@sd2)
  m <- length(mu)
  var <- sigma^2
  kg <- numeric(m)
  
  # Loop over candidate points
  for (i in seq_len(m)) {
    # set knowledge gradient to 0 for any unsafe points - here has become prisks that are too high
    # TODO: Improve by removing any points in that are implausible when evaluating? or beforehand?
    if (prisk_cod[i] > eps || prisk_had[i] > eps) {
      kg[i] <- 0
      next
    }
    
    mu_i <- mu[i]
    sigma_i <- sqrt(var[i] + obs_noise_var)
    # nsim simulated observations - using rnorm to sample from a normal distribution is ok as we are running a GP
    # to model the catch, which is assumed to be normally distributed
    y_sim <- rnorm(nsim, mu_i, sigma_i)
    # the covariance between every point in the grid and the point x_i
    cov_xp_xi <- cov_grid[, i]
    # denominator term in the KG update formula
    denom <- var[i] + obs_noise_var
    # For each simulated y, compute updated max(mu)
    max_after <- numeric(nsim)
    for (s in seq_len(nsim)) {
      # says what would the new maximum mu be if we observed this simulated value at the point x_i 
      # by implementing the standard formula for this in GP posterior updating
      # This evaluates the posterior mean for every point in the space, updating other points based on closeness 
      # to the evaluated point by using the cov matrix
      mu_new <- mu + cov_xp_xi * (y_sim[s] - mu_i) / denom
      # takes this new maximum mu for later averaging
      max_after[s] <- max(mu_new)
    }
    # Computes expected increase in the maximum posterior mean, mu
    # higher values mean we're getting better knowledge as to where and what the maximum catch could be
    kg[i] <- mean(max_after - mu_best)
  }
  # Returns the vector of knowledge gradient values for each point in the design space
  kg
}

## load example data
data("mixedfishery_MixME_om")

# GENERATE OM AND OEM - repeating process from previous tutorials

# This determines what proportion of each stock's catch goes to each fleet based on the historic data
out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = TRUE)
# Update the stocks and fleets with the calculated quota shares
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

# Project the fishery forward in time using historical patterns allowing us to simulate results of the management advice into the future
# yearMeans uses recent historical averages as parameters for the projection
out <- stfMixME(mixedfishery_MixME_om, method = "yearMeans", nyears = 20, wts.nyears = 3, sel.nyears = 3, qs.nyears = 3, verbose = TRUE)

## Add new outputs from future years to the operating model
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

# initial projection year
iy = 2020

# set an arbitrary effort-based target for each fleet as we only want the values at the beginning of 2020
ctrlArgs <- lapply(1:length(mixedfishery_MixME_om$flts), function(x) {
  list(year = iy,quant = "effort",fishery = names(mixedfishery_MixME_om$flts)[x],value = 1)
})

# This matrix maps which fleets catch which stocks
ctrlArgs$FCB <- makeFCB(biols = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts)

# Generate effort-based FLasher::fwd forecast control
flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)

# Simulate one year of fishing to get starting conditions right
omfwd <- FLasher::fwd(object = mixedfishery_MixME_om$stks, fishery = mixedfishery_MixME_om$flts, control = flasher_ctrl)

#Update the operating model with projected population numbers
mixedfishery_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]
mixedfishery_MixME_om$stks$cod@n[,ac(iy)] <- omfwd$biols$cod@n[,ac(iy)]


# CREATE THE NOISY, REAL-LIFE DATA
#TODO: double check this and that this won't be an issue, as suggests we know the reality somewhere which we shouldn't

# Create a list of FLStock objects from the FLBiol objects in the operating model
stk_oem <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {
  
  # for each fleet, find which catch element corresponds to the current stock
  catch <- sapply(mixedfishery_MixME_om$flts, function(y) which(names(y) %in% name(x)))
  
  # get catches form each fleet and convert to FLStock
  xx <- as.FLStock(x, mixedfishery_MixME_om$flts, full = FALSE, catch = catch)
  
  #specifies fishing mortality is measured as instantaneous fishing mortality rate
  units(xx@harvest) <- "f"
  
  # remove excess data as we don't observe stock biomass or stock numbers-at-age directly in reality
  # forces us to simulate these using only the available data
  stock.n(xx)[] <- NA
  stock(xx)[]   <- NA
  
  # return the converted object
  return(xx)
}))





# Define Design Space as discrete with 0.05 increments
# Can change to 0.01 to be more granular once finished testing
dat <- data.frame(expand.grid(
  Fcod = seq(0, 1, by=0.05),
  Fhad = seq(0, 1, by=0.05)
))


# Set an intial F target for both stocks so the first loop has something to work with
f_cod_1 <- 0.28
f_had_1 <- 0.353
point_1 <- data.frame(Fcod = f_cod_1, Fhad = f_had_1)

# CHANGED: Pick 5 other RANDOM points from your grid to initialize the model with a set seed for reproducibility
# TODO: Could space apart equally as in Mike's code to explore the space more 
set.seed(123) 
warmup_indices <- sample(nrow(dat), 2)
warmup_points <- dat[warmup_indices, ]
next_points <- rbind(point_1, warmup_points)



# TESTING OUTSIDE OF THE LOOP FIRST

# CHANGED: Provide eight rows of points to run (due to nature of varlist function)
points_to_run <- varlist(
  # The "Grid" - counts from 1 to 3
  run_id = list(type = "grid", value = 1:nrow(next_points)),
  
  # The "Frozen" Data - the table of pairs available to all workers which we can pipck by run_id
  input_data = list(type = "frozen", value = next_points)
)

# This is the function that will eventually run on each core
doOne <- function(run_id,input_data) {
  
  #Fixing could not find functions error in runners by loading libraries - DO NOT REMOVE
  library(FLCore)
  library(FLFishery)
  library(mse)  
  library(stockassessment)
  library(MixME)
  library(DiceKriging)
  
  # Retrieve the specific inputs for this run ID
  # We use the 'frozen' input_data table and pick the row matching run_id
  this_Fcod <- input_data$Fcod[run_id]
  this_Fhad <- input_data$Fhad[run_id]
  
  # Run the simulation
  res <- obj_func(f_cod = this_Fcod, f_had = this_Fhad, mixedfishery_MixME_om = mixedfishery_MixME_om, stk_oem = stk_oem)
  
  # Get Catch directly from the 'tracking' object
  catch_cod <- sum(res$tracking$cod$stk["C.om", ac(2020:2039)], na.rm = TRUE)
  catch_had <- sum(res$tracking$had$stk["C.om", ac(2020:2039)], na.rm = TRUE)
  # Calculate total catch
  total_catch <- catch_cod + catch_had
  
  ## // Calculate the risk for both stocks in each year and at end of the projection//
  # TODO: Improve to a probability somehow, multiple iterations if needed
  # CONCLUSION: Need multiple iterations to generate a probabilistic risk as this data ("mixedfishery_MixME_om") is deterministic
  
  # Define Blims
  Blim_cod <- 107000
  Blim_had <- 9227
  
  # Extract SSB directly - still focusing on the end of the simualtion in 2039
  ssb_cod_data <- c(res$tracking$cod$stk["SB.om", ac(2039)])
  ssb_had_data <- c(res$tracking$had$stk["SB.om", ac(2039)])
  
  # Calculate risk for cod
  if (is.na(ssb_cod_data)) {
    # If it crashed, we treat SSB as effectively 0 so the penalty is the log of the full distance (Blim - 0)
    risk_cod <- -log(Blim_cod) 
  } else if(ssb_cod_data > Blim_cod) {
    # Safe zone: No penalty
    risk_cod <- 0
  } else {
    # Danger zone: The "risk" is how deep we are in the hole. We log it to make the GPs more stable
    risk_cod <- -log(Blim_cod - ssb_cod_data) 
  }
  
  # Calculate risk for haddock
  if (is.na(ssb_had_data)) {
    # If it crashed, we treat SSB as effectively 0 so the penalty is the log of the full distance (Blim - 0)
    risk_had <- -log(Blim_had) 
  } else if (ssb_had_data > Blim_had) {
    # Safe zone: No penalty
    risk_had <- 0
  } else {
    # Danger zone: The "risk" is how deep we are in the hole. We log it to make the GPs more stable
    risk_had <- -log(Blim_had - ssb_had_data) 
  }
  
  # Set up variable to return
  to_return <- c(Fcod = this_Fcod,Fhad = this_Fhad,RiskCod = risk_cod,RiskHad = risk_had,TotalCatch = total_catch)
  
  # Return the result
  return(to_return)
}

# 1. Check how many cores you have
n_cores <- parallel::detectCores() - 1 

result <- doMclapply(points_to_run,doOne = doOne,cores = n_cores)

# Now just need correct table mainpulaation
# look at names = TRUE, getArray and if type frozen or inner is good to get the results object into something I can work with
# use array2df to get a data frame
data_frame_initial <- as.data.frame(do.call(rbind, result))

# Extract the values which are currently a list
vals <- do.call(rbind, data_frame_initial$value)

# Bind them with the other columns (excluding the original 'value' column)
data_frame_result <- cbind(as.data.frame(vals), data_frame_initial[, c("error", "warning", "time")])

names(data_frame_result) <- c("Fcod","Fhad","RiskCod","RiskHad","TotalCatch","error","warning","time")


# Set up data frame to store data from current run - only keeping values that I want
dat_run <- data.frame(
  Fcod = c(data_frame_result$Fcod),
  Fhad = c(data_frame_result$Fhad),
  TotalCatch = c(data_frame_result$TotalCatch),
  RiskCod = c(data_frame_result$RiskCod),
  RiskHad = c(data_frame_result$RiskHad)
)

dat_run

# Adding names to reference in GPs and for readability
names(dat_run) <- c("Fcod","Fhad","TotalCatch","RiskCod","RiskHad")


# Runs variable to count all runs so far and all their associated data
runs <- rbind(runs, dat_run)

runs



## // Adjusting for GPs later on //

# Taking log of the catch so GP models are more stable
  log_total_catch <- log(runs$TotalCatch + 1e-12) # add small constant to avoid log(0)

  # --- FIX FOR FLAT DATA --- as we only have one iteration
  risk_cod_input <- runs$RiskCod
  risk_had_input <- runs$RiskHad
  
  # If variance is 0 (all zeros), add tiny random noise so Kriging doesn't crash - fix until risk calculated properly
  if(var(risk_cod_input, na.rm = TRUE) == 0) {
    risk_cod_input <- risk_cod_input + rnorm(length(risk_cod_input), mean=0, sd=1e-10)
  }
  
  if(var(risk_had_input, na.rm = TRUE) == 0) {
    risk_had_input <- risk_had_input + rnorm(length(risk_had_input), mean=0, sd=1e-10)
  }
