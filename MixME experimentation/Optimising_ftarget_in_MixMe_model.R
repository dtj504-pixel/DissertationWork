## PSEUDOCODE EXPANDED

#IMPORTANT NOTE:
# " we will assume that we can perfectly observe the stock without error"

# Focusing on this example first in my project

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
  input <- makeMixME(om = mixedfishery_MixME_om, catch_obs = stk_oem, management_lag = 0, management_type = "fixedF", parallel = FALSE)
  
  # Set up the observation error model
  # Cod and haddock catches occur at the start of the year so timing=0
  # Not very realistic - could consider changing
  input$oem@args$catch_timing$cod <- 0
  input$oem@args$catch_timing$had <- 0
  
  # Define the age range for calculating average fishing mortality (Fbar) for both stocks
  input$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
  input$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)
  
  
  # Set the target fishing mortality for both stocks - may be recommended by ICES
  
  #THIS IS WHAT I SHOULD BE CHANGING EACH TIME and running an algorithm to find the best one
  input$ctrl_obj$hcr@args$ftrg$cod <- f_cod  # use MSY f-target
  input$ctrl_obj$hcr@args$ftrg$had <- f_had # use MSY f-target
  
  ## Update fbar ranges
  input$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
  input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)
  
  
  #RUN MIXME SIMULATION
  res <- runMixME(om  = input$om, oem = input$oem, ctrl_obj = input$ctrl_obj, args = input$args)
  
  # UNDERSTANDING MIXME OUPUTS
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
  
  ## Define Blim for each stock
  hcrpars <- list(cod = c(Blim = 107000), had = c(Blim = 9227))  # https://doi.org/10.17895/ices.advice.5897
  
  # Update the control object with the new HCR parameters
  res$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = hcrpars))
  
  return(res)
  
}


# //DEFINE KG FUNCTION HERE//

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
  cov_grid <- cov_exp(X_pred, X_pred, theta = model@covariance@range.val, sigma2 = model@covariance@sd2)
  m <- length(mu)
  var <- sigma^2
  # Current best mean catch
  mu_best <- max(mu)   
  kg <- numeric(m)
  
  # Loop over candidate points
  for (i in seq_len(m)) {
    # set knowledge gradient to 0 for any points with a prisks taht are too low
    # TODO: Improve by removing any points in that are implausible when evaluating? or beforehand?
    if (prisk_cod[i] < eps || prisk_had[i] < eps) {
      kg[i] <- 0
      next
    }
    
    mu_i <- mu[i]
    sigma_i <- sqrt(var[i] + obs_noise_var)
    # nsim simulated observations - using rnorm to sample from a normal distribution is ok here as we are running a GP
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
      # takes this new maximum mu inot a vector for later averaging
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

# This projects the fishery forward in time using historical patterns as seen in previous tutorials, 
# allowing us to simulate results of the management advice into the future
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

# This simulates one year of fishing to get starting conditions right
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
  
  # get catches form eahc fleet and convert to FLStock
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





# Set an intial F target for both stocks so the first loop has something to work with
f_cod <- 0.28
f_had <- 0.353

# Define Design Space as discrete with 0.01 increments
dat <- data.frame(expand.grid(
  Fcod = seq(0, 1, by=0.01),
  Fhad = seq(0, 1, by=0.01)
))

#Intialise runs object
runs<- NULL

# Pick 5 RANDOM points from your grid to initialize the model
# (We use set.seed to make sure it's reproducible)
set.seed(123) 
warmup_indices <- sample(nrow(dat), 5)
warmup_points <- dat[warmup_indices, ]

print("--- STARTING WARM-UP PHASE (5 Iterations) ---")

for (i in 1:nrow(warmup_points)) {
    
    # Get the F values for this warm-up run
    f_cod_curr <- warmup_points$Fcod[i]
    f_had_curr <- warmup_points$Fhad[i]
    
    print(paste("Warm-up Run:", i, "| Testing Fcod:", f_cod_curr, "Fhad:", f_had_curr))
    
    # 1. RUN SIMULATION
    res <- obj_func(f_cod_curr, f_had_curr, mixedfishery_MixME_om, stk_oem)
    
    # 2. EXTRACT CATCH (Direct Lookup)
    catch_cod <- sum(res$tracking$cod$stk["C.om", ac(2020:2039)], na.rm = TRUE)
    catch_had <- sum(res$tracking$had$stk["C.om", ac(2020:2039)], na.rm = TRUE)
    total_catch <- catch_cod + catch_had
    
    # 3. EXTRACT RISK (Direct Lookup)
    Blim_cod <- 107000
    Blim_had <- 9227
    
    risk_cod_annual <- iterMeans(res$tracking$cod$stk["SB.om", ac(2020:2039)] < Blim_cod, na.rm = TRUE)
    risk_had_annual <- iterMeans(res$tracking$had$stk["SB.om", ac(2020:2039)] < Blim_had, na.rm = TRUE)
    
    risk_cod_val <- c(risk_cod_annual[, "2039"])
    risk_had_val <- c(risk_had_annual[, "2039"])
    
    # 4. STORE DATA
    dat_run <- data.frame(
        Fcod = f_cod_curr,
        Fhad = f_had_curr,
        TotalCatch = total_catch,
        RiskCod = risk_cod_val,
        RiskHad = risk_had_val
    )
    
    # Add to 'runs'
    if (is.null(runs)) {
        runs <- dat_run
    } else {
        runs <- rbind(runs, dat_run)
    }
}

print("--- WARM-UP COMPLETE. 'runs' now has 5 valid data points. ---")
print(runs)





# // LOOP STARTS HERE AS WE ARE CHANGING FTARGET //

max_rounds <- 20
round_num <- 1

#TODO: Can I rename the variables ion each round so I can clearly see the progression?

for (iteration in 1:max_rounds) {
  
  # Run the model with the new F targets (set at the end of the last run)
  res <- obj_func(f_cod,f_had,mixedfishery_MixME_om,stk_oem)
  
  # // WE HAVE NOW RUN THE FULL MODEL FOR A SINGLE SET OF F TARGETS //
  
  # Get Catch directly from the 'tracking' object
  # We use 'C.om' which we saw in your diagnostics
  catch_cod <- sum(res$tracking$cod$stk["C.om", ac(2020:2039)], na.rm = TRUE)
  catch_had <- sum(res$tracking$had$stk["C.om", ac(2020:2039)], na.rm = TRUE)
  
  total_catch <- catch_cod + catch_had
  
  print(total_catch)
  
  ## // Calculate the risk for both stocks in each year and at end of the projection//
  # TODO: Improve to a probability somehow, multiple iteratiosn if needed
  
  # Define Blims
  Blim_cod <- 107000
  Blim_had <- 9227
  
  # Extract SSB directly
  # This is safe because we verified "SB.om" exists
  ssb_cod_data <- res$tracking$cod$stk["SB.om", ac(2020:2039)]
  ssb_had_data <- res$tracking$had$stk["SB.om", ac(2020:2039)]
  
  # Calculate Risk Profiles (Annual)
  # CRITICAL FIX: Added 'na.rm = TRUE'. 
  # Without this, the 5.9% NAs in your data would make the Risk = NA.
  risk_cod_annual <- iterMeans(ssb_cod_data < Blim_cod, na.rm = TRUE)
  risk_had_annual <- iterMeans(ssb_had_data < Blim_had, na.rm = TRUE)
  
  # Extract the final year for the optimization
  risk_cod_2039 <- c(risk_cod_annual[, "2039"])
  risk_had_2039 <- c(risk_had_annual[, "2039"])
  
  # As we're focusing on the final risk, we set these as our risk metrics
  risk_cod <- risk_cod_2039
  risk_had <- risk_had_2039
  
  print(risk_cod)
  print(risk_had)
  
  
  # Set up data frame to store data from current run
  dat_run <- data.frame(
    Fcod = c(f_cod),
    Fhad = c(f_had),
    TotalCatch = c(total_catch),
    RiskCod = c(risk_cod),
    RiskHad = c(risk_had)
  )
  
  # Adding names to reference in GPs and for readability
  names(dat_run) <- c("Fcod","Fhad","TotalCatch","RiskCod","RiskHad")
  # Taking log of the catch so GP models are more stable
  log_total_catch <- log(dat_run$TotalCatch + 1e-12) # add small constant to avoid log(0)
  print(log_total_catch)
  
  # Runs variable to count all runs so far and all their associated data
  if (iteration == 1) {
    runs <- dat_run
  } 
  else {
    runs <- rbind(runs, dat_run)
  }
  
  print("runs is")
  print(runs)
  
  # SET UP THE GPS
  # Adding 1e-15 to nuggets to avoid 0 nugget variance
  gp_log_cat <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = runs$TotalCatch,nugget=1e-12*var(runs$TotalCatch)+1e-15,covtype = "exp")
  gp_cod_risk <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = runs$RiskCod,nugget=1e-12*var(runs$RiskCod)+1e-15,covtype = "exp")
  gp_had_risk <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = runs$RiskHad,nugget=1e-12*var(runs$RiskHad)+1e-15,covtype = "exp")
  
  
  # Find the remaining plausible points using BHM
  
  pred_risk_cod <- predict(gp_cod_risk, newdata = gridd, type = "SK")
  pred_risk_had <- predict(gp_had_risk, newdata = gridd, type = "SK")
  pred_log_cat <- predict(gp_log_cat, newdata = gridd, type = "SK")
  
  # Get probability that risk =< 0.05 for both cod and haddock
  prisk_cod <- pnorm(0.05, pred_risk_cod$mean, pred_risk_cod$sd + 1e-12)
  prisk_had <- pnorm(0.05, pred_risk_had$mean, pred_risk_had$sd + 1e-12)
  
  # Filter for valid runs
  valid_runs <- runs$TotalCatch[runs$RiskCod < 0.05 & runs$RiskHad < 0.05]
  
  print("valid_runs is")
  print(valid_runs)
  
  # Check if any valid runs exist before calculating current max
  if(length(valid_runs) > 0) {
    current_max <- max(valid_runs, na.rm = TRUE)
  } 
  else {
    current_max <- 0 
    print("Warning: No runs met the safety criteria (< 5% risk)")
  }
  
  # Probability catch is =< current max
  pcat <- pnorm(log(current_max), pred_log_cat$mean, pred_log_cat$sd + 1e-12)
  
  # Set epsilon for plausibility threshold
  eps <- 1e-4
  # Determine plausible points where min(1 - pcat, prisk) > eps
  possible <- (apply(cbind((1 - pcat), prisk_cod, prisk_had), 1, min) > eps)
  
  # Calculate KG
  mu <- pred_log_cat$mean
  sigma <- pred_log_cat$sd
  #TDOD: Put in correct X_pred
  kg <- knowledge_gradient_sim(mu, sigma, gp_log_cat, obs_noise_var = 0, nsim = 100, prisk_cod = prisk_cod, prisk_had = prisk_had, eps = 1e-4)
  
  
  # Calculate the remaining points - get loop to end when none left
  gridd_with_kg <- data.frame(expand.grid(Fcod,Fhad))
  gridd_with_kg$kg <- kg
  gridd_with_kg$possible <- possible
  names(gridd_with_kg) <- c("Fcod","Fhad", "kg","possible"  )
  
  # Filter to only plausible points with positive KG
  cand <- subset(gridd_with_kg, possible == TRUE & kg > 0)
  
  # Create keys for filtering
  cand$key <- paste(cand$Fcod, cand$Fhad, sep = "_")
  runs$key <- paste(runs$Fcod, runs$Fhad, sep = "_")
  
  # Remove already evaluated points
  cand <- cand[!(cand$key %in% runs$key), ]
  
  # Check if candidates exhausted
  if (nrow(cand) == 0) {
    cat("No unevaluated candidates with positive EI. Stopping at round", iteration, "\n")
    break
  }
  
  # Print how many points are left
  cat("Unevaluated candidates with EI > 0:", nrow(cand), "\n")
  
  # Get candidate point with highest KG
  next_point <- cand[which.max(cand$kg), c("Fcod","Fhad")]
  
  # Set the point with the highest KG as the new Ftargets to evaluate
  f_cod <- next_point$Fcod
  f_had <- next_point$Fhad
  
  # // LOOP ENDS //
  round_num <- iteration
  
}

