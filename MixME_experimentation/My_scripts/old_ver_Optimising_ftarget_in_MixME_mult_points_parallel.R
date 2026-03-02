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
library(simsalapar)
library(doParallel)
library(foreach)
library(parallel)
library(plot3D)

print(" This is Optimising_ftarget_in_MixME_mult_points_parallel")


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
  
  ## // Extract the min ssb at the last ten years of the simulation for both stocks //
    # Picking up long term SSB values to see if they dip below Blim at any point
    ssb_cod_data <- c(res$tracking$cod$stk["SB.om", ac(2030:2039)])
    ssb_had_data <- c(res$tracking$had$stk["SB.om", ac(2030:2039)])

    # Getting minimums to model with GPs
    ssb_cod_min <- min(ssb_cod_data, na.rm = TRUE)
    ssb_had_min <- min(ssb_had_data, na.rm = TRUE)

    # Safety Ctahc for if the simulation failed or produced all NAs
    if(is.infinite(ssb_cod_min) | is.na(ssb_cod_min)) ssb_cod_min <- 0
    if(is.infinite(ssb_had_min) | is.na(ssb_had_min)) ssb_had_min <- 0
  
  # free up memory now that we have the data we need from res
  rm(res)   
  
  # Set up variable to return
  to_return <- c(Fcod = this_Fcod,Fhad = this_Fhad,SSBCod = ssb_cod_min,SSBHad = ssb_had_min,TotalCatch = total_catch)
  
  # Return the result
  return(to_return)
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
  # Current best mean catch
  mu_best <- max(mu) 
  
  # Loop over candidate points
  for (i in seq_len(m)) {
    # set to 0 for any unsafe points - here has become probability that ssb is =< Blim is > 0.05 in the years 2030-2039
    # i.e. the run is not precautionary in these years
    if (pssb_cod[i] > 0.05 || pssb_had[i] > 0.05) {
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
  Fcod = seq(0.0, 0.6, by=0.02), # Starts at 0 to capture low F
  Fhad = seq(0.0, 0.6, by=0.02)  # Starts at 0 to capture low F
))


# // SETUP FOR LOOP //

# Define Blims
  Blim_cod <- 107000
  Blim_had <- 9227

# Set an intial F target for both stocks so the first loop has something to work with
# Sticking to Ftargets given in fixed fishing mortality example: https://github.com/CefasRepRes/MixME/wiki/Fixed-fishing-mortality-management-strategy
f_cod_1 <- 0.28
f_had_1 <- 0.353
point_1 <- data.frame(Fcod = f_cod_1, Fhad = f_had_1)

# CHANGED: Pick 5 other RANDOM points from your grid to initialize the model with a set seed for reproducibility
# TODO: Could space apart equally as in Mike's code to explore the space more 
set.seed(123) 

# To prep for running in parallel, check how many cores you have
n_cores <- parallel::detectCores() - 1 
n_warmup <- 2 * n_cores

warmup_indices <- sample(nrow(dat),n_warmup)
warmup_points <- dat[warmup_indices, ]
next_points <- rbind(point_1, warmup_points)
next_points



# Set max runs and initial round number
max_rounds <- 30
round_num <- 1

# Intiliase runs for safety
runs <- NULL

# // START OF LOOP //

for (iteration in 1:max_rounds) {
  
    # CHANGED: Provide eight rows of points to run (due to nature of varlist function)
    points_to_run <- varlist(
    # The "Grid" - counts from 1 to 3
    run_id = list(type = "grid", value = 1:nrow(next_points)),
    
    # The "Frozen" Data - the table of pairs available to all workers which we can pick by run_id
    input_data = list(type = "frozen", value = next_points)
    )

    # Run all the points in parallel, one at a time one each core
    result <- doMclapply(points_to_run,doOne = doOne,cores = n_cores)

    # Make a data frame with each result as a row
    data_frame_initial <- as.data.frame(do.call(rbind, result))

    # Extract the values which are currently a list
    vals <- do.call(rbind, data_frame_initial$value)

    # Bind them with the other columns (excluding the original 'value' column to avoid duplicating)
    data_frame_result <- cbind(as.data.frame(vals), data_frame_initial[, c("error", "warning", "time")])

    names(data_frame_result) <- c("Fcod","Fhad","SSBCod","SSBHad","TotalCatch","error","warning","time")


    # Set up data frame to store data from current run - only keeping values that I want
    # NOte: TotalCatch is in the middle now
    dat_run <- data.frame(
    Fcod = c(data_frame_result$Fcod),
    Fhad = c(data_frame_result$Fhad),
    SSBCod = c(data_frame_result$SSBCod),
    SSBHad = c(data_frame_result$SSBHad),
    TotalCatch = c(data_frame_result$TotalCatch)
    )

    dat_run

    # Adding names to reference in GPs and for readability
    names(dat_run) <- c("Fcod","Fhad","SSBCod","SSBHad","TotalCatch")


    # Runs variable to count all runs so far and all their associated data
    runs <- rbind(runs, dat_run)

    runs

    # Taking log of the catch so GP models are more stable
    log_total_catch <- log(runs$TotalCatch + 1e-12) # add small constant to avoid log(0)
    ssb_cod_min <- runs$SSBCod
    ssb_had_min <- runs$SSBHad
    
    
    # SET UP THE GPS
    # Adding 1e-15 to nuggets to avoid 0 nugget variance
    gp_log_cat <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = log_total_catch,nugget=1e-12*var(runs$TotalCatch)+1e-15,covtype = "exp")
    gp_cod_ssb <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = ssb_cod_min,nugget=1e-12*var(ssb_cod_min)+1e-15,covtype = "exp")
    gp_had_ssb <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = ssb_had_min,nugget=1e-12*var(ssb_had_min)+1e-15,covtype = "exp")
    
    print("GPs done")
    
    # Find the remaining plausible points using BHM
    pred_ssb_cod <- predict(gp_cod_ssb, newdata = dat, type = "SK")
    pred_ssb_had <- predict(gp_had_ssb, newdata = dat, type = "SK")
    pred_log_cat <- predict(gp_log_cat, newdata = dat, type = "SK")
    
    # Get the probability that sbb =< Blim for both cod and haddock - want this to be low!
    pssb_cod <- pnorm(Blim_cod, pred_ssb_cod$mean, pred_ssb_cod$sd + 1e-12)
    pssb_had <- pnorm(Blim_had, pred_ssb_had$mean, pred_ssb_had$sd + 1e-12)

    # Filter for valid runs
    valid_runs <- runs$TotalCatch[runs$SSBCod > Blim_cod & runs$SSBHad > Blim_had]
    
    print("valid_runs is")
    print(valid_runs)
    
    # Check if any valid runs exist before calculating current max
    # TODO: Put in due to an error but can't remember why needed
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
    # Determine plausible points where possibility of catch being greater than current max is > eps
    # and where probability that ssb<Blim for cod and haddock is less than 0.05
    possible <- (1 - pcat) > eps & pmax(pssb_cod, pssb_had) < 0.05

    # Visualises region that has risk < threshold and better catch than best evaluated so far
    # TODO: Check I am naming this the correct way around
    grid_dim <- sqrt(nrow(dat))
    image2D(matrix(possible * (1-pcat),nrow=grid_dim),y=sort(unique(dat$Fcod)),x=sort(unique(dat$Fhad)),xlab="Fhad",ylab="Fcod",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
    
    # Calculate KG
    mu <- pred_log_cat$mean
    sigma <- pred_log_cat$sd
    kg <- knowledge_gradient_sim(mu, sigma, gp_log_cat, obs_noise_var = 0, nsim = 100, prisk_cod = prisk_cod, prisk_had = prisk_had, eps = 1e-4)
    
    print("kg done")
    
    # Calculate the remaining points
    dat_with_kg <- dat
    dat_with_kg$kg <- kg
    dat_with_kg$possible <- possible
    names(dat_with_kg) <- c("Fcod","Fhad", "kg","possible"  )
    
    # Filter to only plausible points with positive KG
    cand <- subset(dat_with_kg, possible == TRUE & kg > 0)
    
    # Create keys for filtering
    cand$key <- paste(cand$Fcod, cand$Fhad, sep = "_")
    runs$key <- paste(runs$Fcod, runs$Fhad, sep = "_")
    
    # Remove already evaluated points
    cand <- cand[!(cand$key %in% runs$key), ]
    
    # Check if candidates exhausted - get loop to end when none left
    if (nrow(cand) == 0) {
        cat("No unevaluated candidates with positive KG. Stopping at round", iteration, "\n")
        pot_points_final <- dat[possible, ]
        cat("\nOptimal parameters:\n")
        print(pot_points_final)
        break
    }
    
    # Print how many points are left
    cat("Unevaluated candidates with KG > 0:", nrow(cand), "\n")
    
    # Select next points - keeping consistent with number of random points selected at the start
    if (nrow(cand) <= n_warmup) {
        next_points <- cand[order(-cand$kg), c("Fcod", "Fhad")]
    } else {
        top_candidates <- cand[order(-cand$kg), ][1:nrow(cand), ]
        set.seed(123)
        km_result <- kmeans(top_candidates[, c("Fcod", "Fhad")], centers = n_warmup)
        top_candidates$cluster <- km_result$cluster
        next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
        df[which.max(df$kg), c("Fcod", "Fhad", "kg")]
        }))
    }
    
    print("next points are")
    print(next_points)
    

    # CRITICAL MEMORY CLEANUP - added in due to RStudio running out of storage without this
    
    # Remove the heavy simulation result
    rm(result) 
    
    # Remove the Kriging models (they can be heavy too)
    rm(gp_log_cat, gp_cod_ssb, gp_had_ssb)
    
    # Remove the prediction vectors to be safe
    rm(pred_ssb_cod, pred_ssb_had, pred_log_cat)

    # KEEP ONLY WANTED COLUMNS IN runs
    # Define the columns we actually care about, otherwise we can't do rbind to runs properly next iteration
    important_cols <- c("Fcod","Fhad","SSBCod","SSBHad","TotalCatch")
    
    # Only keep those columns before binding
    runs <- runs[, important_cols]
    
    # // LOOP ENDS //
    round_num <- iteration

}