## // UNDER ADJUSTMENT SO MIGHT GET QUITE ODD //
# NO data to test on yet as no data for whiting in MixME tutorial

# // TODO: Currently DETERMINSTIC treatment of sampled points - issue if simulations or data are stochastic //


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

## === Define a custom harvest control rule ===
ICES_HCR <- function (stk, args, hcrpars, tracking) {
  
#' @param stk contains information about fish stocks, e.g. age
#' @param args contains useful arguments such as ay and mlag
#' @param hcrpars contains the parameters of the HCR, e.g. Ftrgt, Btrigger, Blim
#' @param tracking contains the current tracking object and is currently returned unchanged

  ## Extract year arguments

  # ay is the current assessment year
  ay <- args$ay
  # mlag is the management lag in years
  mlag <- args$management_lag
  # this is the number of iterations
  ni <- dims(stk)[["iter"]]
  

  # SHORT-TERM FORECAST
  ## Carry out short-term forecast to get ssb at the beginning of the advice year
  
  ## historical geomean to estimate recruitment for the stock (prevents big spikes in recruitment having a large effect)
  sr0 <- as.FLSR(stk, model = "geomean")
  # propagate sr0 params to match number of simulations
  sr0@params <- FLCore::propagate(sr0@params, iter = ni)
  
  # hardcoded using different starting years for different stocks when estimating recruitment
  if(stk@name == "cod") stk_n <- window(stk@stock.n, start = 2015)
  if(stk@name == "had") stk_n <- window(stk@stock.n, start = 1993)
  
  # We get the row of data for the youngest fish and calculate the geometric mean for that row
  # We do this by taking a log before calculating the average, as this reduces the effect of large values, 
  # and then exponentiating
  sr0@params[] <- exp(yearMeans(log(stk_n[1,])))
  

  ## find the first year with any data
  minyr <- dims(stk@stock[!is.na(stk@stock.n)])$minyear
  ## remove any years before this
  stk0  <- window(stk, start = minyr)
  
  ## extend stock object by one year
  ## automatically fills in parameters by averaging over the last 3 years
  stk0 <- FLasher::stf(stk0, 1)
  
  # FLasher::fwd will throw an error if there are NAs in weights in future years.
  # I need to zero these if associated numbers are zero
  FLCore::discards.wt(stk0)[FLCore::discards.n(stk0) == 0] <- 0
  FLCore::landings.wt(stk0)[FLCore::landings.n(stk0) == 0] <- 0
  

  ## find status quo F slightly differently for each stock
  # cod and whiting average last three years, haddock average last year only
  if(stk@name == "cod") fwd_yrs_fsq <- -2:0
  if(stk@name == "had") fwd_yrs_fsq <- 0
  
  # estimated fishing mortality for the forward year for each stock
  fsq <- yearMeans(fbar(stk)[,as.character(fwd_yrs_fsq+ay-mlag)])
  
  ## FLasher cannot handle fsq=0
  fsq <- ifelse(fsq==0, 0.001,fsq)

  ## Construct forward fishing mortality with same n umber fo rows a simualtions
  # Will overwrite the second row later
  targ <- matrix(0,nrow=2, ncol = ni)
  targ[1,] <- fsq
  targ[2,] <- fsq
  
  # Wrappper that constructs the fwdControl object, setting fbar to values in the targ matrix
  ctrl0 <- fwdControl(list(
    year  = c(ay,ay+mlag),
    quant = "fbar",
    value = c(targ)))
  
  ## project stock forward to get ssb in the advice year
  stk_fwd <- FLasher::fwd(stk0, sr = sr0, control = ctrl0)
  

  # HCR

  ## Extract the parameters and propogate to each simulation
  Ftrgt <- propagate(FLPar(hcrpars["Ftrgt"]), ni)
  Btrigger <- propagate(FLPar(hcrpars["Btrigger"]), ni)
  Blim <- propagate(FLPar(hcrpars["Blim"]), ni)
  
  ## calculate F multiplier

  # calculate what ratio of Btrigger we are at for each simulation - 1 means we're at Btrigger 
  status_Btrigger <- tail(ssb(stk_fwd), 1)/Btrigger
  # find which simulations are above Btrigger
  pos_Btrigger <- which(status_Btrigger > 1)
  # set Ftarget to the ratio of Ftarget that corresponds to status_Btrigger
  Fmult <- status_Btrigger
  # cap this ratio at 1
  Fmult[, , , , , pos_Btrigger] <- 1
  
  ## calculate new F target
  Ftrgt <- Ftrgt * Fmult
  # make most of the control object for output
  ctrl <- fwdControl(list(year = ay + mlag, quant = "fbar", value = Ftrgt))
  
  ## Attach Blim for use in implementation
  attr(ctrl, "Blim") <- Blim
  
  #return the control object and tracking object
  return(list(ctrl = ctrl, tracking = tracking))
}

#' Advice implementation using FLasher
#' 
#' Carry out a short-term forecast using Flasher
#' to transform advised fishing mortality into an advised catch target.
#' 
#' @param stk Object of class \code{FLStock} containing observed stock 
#'            information including commercial catch data, individual mean 
#'            weights and biological parameters.
#' @param tracking Tracking object
#' @param args Additional arguments
#' @param forecast Logical. Should a short-term forecast be carried out? Defaults
#'                 to \code{TRUE}
#' @param fwd_trgt Character. Catch or effort target to use during forecast. 
#'                 Defaults to 'fsq'.
#' @param fwd_yrs Integer. The number of years to forecast. Defaults to 1.
#' @param fwd_yrs_average Integer vector. The historical data years over which
#'                        biological parameter are averaged for use during 
#'                        forecast. Defaults to -3:-1.
#' @param fwd_yrs_rec_start Integer. Starting historical year from which to sample
#'                          projection period recruitment during forecast.
#' @param fwd_yrs_sel Integer vector. The historical data years over which
#'                    catch selection-at-age is averaged for use during forecast.
#'                    Defaults to -3:-1.
#' @param fwd_yrs_lf_remove Integer Vector. ... Defaults to -2:-1.
#' @param fwd_splitLD Logical. Defaults to \code{TRUE}


# === Creating forecast function ===
forecast_fun <- function(stk, tracking, ctrl,
                         args,                       # contains ay (assessment year) and management lag
                         fwd_trgt = c("fsq", "hcr"), # fish to status quo (fsq) in intermediate year and then apply hcr in final calculations
                         fwd_yrs = 2,                # number of years to add - have a space for the year inbetween to do intermediate calculations and then for the year we want a quota for
                         fwd_yrs_fsq = -2:0,         # years used to calculate fsq
                         fwd_yrs_average = -3:0,     # years used for averages
                         fwd_yrs_rec_start = NULL,   # recruitment - null uses the entire history to estimate
                         fwd_yrs_sel = -3:-1,        # selectivity of equipment - average form 3 years ago to 1 year ago 
                         fwd_yrs_lf_remove = -2:-1,  # calculate landings to discards ratio from 2 years ago to 1 year ago
                         fwd_splitLD = TRUE)         # Whetehr to calculate landings and discards separately
{  
  ## get current assessment year
  ay <- args$ay
  
  ## get management lag
  mlag <- args$management_lag
  
  ## checking number of iterations
  niter <- dim(stk)[6]  

# Potential issue with different ways of calculating fsq for different functions???
# The HCR function has a fixed year for each fsq but the forecast function uses the same year for every stock
# UNDERSTAND ABOVE: a list of variables is set for each stock and so  different values can be set for each one

  ## geomean to estimate recruitment for the stock
  sr0 <- as.FLSR(stk, model = "geomean")
  sr0@params <- FLCore::propagate(sr0@params, iter = niter)
  
  if (!is.null(fwd_yrs_rec_start)) {
    stk_n <- window(stk@stock.n, start = fwd_yrs_rec_start)
  } else {
    stk_n <- stk@stock.n
  }
  sr0@params[] <- exp(yearMeans(log(stk_n[1,])))
  
  ## remnove years before first data year
  minyr <- dims(stk@stock[!is.na(stk@stock.n)])$minyear # min year where data exists
  stk0  <- window(stk, start = minyr)
  
  ## extend stock object and fill with assumptions calculated based on variables defined in function call
  # use fwd_yrs+1 because we need to see if the stock crashes at the start of the year after the advice year
  stk0 <- FLasher::stf(stk0, fwd_yrs+1)
  
  # FLasher::fwd will throw an error if there are NAs in weights in future years.
  # I need to zero these if associated numbers are zero
  FLCore::discards.wt(stk0)[FLCore::discards.n(stk0) == 0] <- 0
  FLCore::landings.wt(stk0)[FLCore::landings.n(stk0) == 0] <- 0
  
  ## find status quo F for each stock
  fsq <- yearMeans(fbar(stk)[,as.character(fwd_yrs_fsq+ay-mlag)])
  
  ## FLasher cannot handle fsq=0
  fsq <- ifelse(fsq==0, 0.001,fsq)
  
  ## Construct matrix with rows as years and columns as iterations as before
  # and fill it with the Ftarget values
  targ <- matrix(0,nrow=fwd_yrs+1, ncol = niter)
  targ[1,] <- fsq
  targ[2,] <- ctrl@iters[,"value",]
  targ[3,] <- ctrl@iters[,"value",]
  
  # year sets the timeline, quant = fbar says we are using fishing mortality
  ctrl0 <- fwdControl(list(year  = c(ay,ctrl@target$year,ctrl@target$year+1), quant = "fbar", value = c(targ)))
  
  ## project stock forward
  stk_fwd <- FLasher::fwd(stk0, sr = sr0, control = ctrl0)
  
  ## Extract catch target for the advice year (3 decimal places) by looking at the result object
  TAC <- round(c(catch(stk_fwd)[,ac(ay+mlag)]),3)
  
  ## Get SSB at the end of the advice year (or at the start of the year after?)
  TACyr_ssb <- c(ssb(stk_fwd)[,ac(ay+mlag+1)])
  
  ## Find iterations where SSB at end of TAC year is < Blim
  belowBlim <- which(TACyr_ssb < attr(ctrl, "Blim"))
  
  ## Go through model fits - where SSB < Blim, forecast to target Blim
  if (length(belowBlim) > 0) {
    
    ## define forward control targetting Blim
    targ <- matrix(0,nrow=fwd_yrs+1, ncol = niter)
    targ[1,] <- fsq
    # target is now Blim
    targ[2,] <- c(attr(ctrl,"Blim"))
    targ[3,] <- ctrl@iters[,"value",]
    
    ctrl_blim <- fwdControl(list(
      year  = c(ay,ay+mlag,ay+mlag+1),
      #focus on biomass in the 2nd year to force us to end at Blim instead if we were below
      quant = c("fbar","ssb_end","fbar"),
      value = c(targ)))
    
    #rerun simulation focusing on Blim to find the Ftarget that is appropriate
    stk_blim <- FLasher::fwd(stk0, sr = sr0, control = ctrl_blim)
    
    ## Find iterations with zero TAC advice (some stocks may be in such bad health taht we can't catch any)
    zeroTAC <- ssb(stk_blim)[,ac(ay+mlag+1)] < attr(ctrl,"Blim")
    
    ## update TAC
    TAC[belowBlim] <- round(c(catch(stk_blim)[,ac(ay+mlag)])[belowBlim],3)
    # set any in zeroTAC to zero manually
    TAC[zeroTAC]   <- 0
  }
  
  ## Construct fwd control object, which we measurre using catch not fbar now
  ctrl0 <- fwdControl(list(year = ay + mlag, quant = "catch", value = TAC))
  
  # return the same objects as before
  return(list(ctrl = ctrl0, tracking = tracking))
}

# Defining objective function to run simulation 
obj_func <- function(f_cod, f_had, input) {
  
    # Check estimation methods
    input$ctrl_obj$est@args$estmethod$cod
    input$ctrl_obj$est@args$estmethod$had

    # we don't need any additional arguments for stock estimation
    # We are assuming there is no error in the observations
    # This means the stock will crash only if the HCR rule is bad
    input$ctrl_obj$est@args$fitList <- NULL
    input$ctrl_obj$est@args$fwdList <- NULL
    
    # INJECT CUSTOM FUNCTIONS into the model
    input$ctrl_obj$hcr@args$hcrmethod$cod <- ICES_HCR
    input$ctrl_obj$hcr@args$hcrmethod$had <- ICES_HCR
    
    # Change method of calculating TACs to use the forecast function defined above
    input$ctrl_obj$isys@args$isysmethod$cod <- forecast_fun
    input$ctrl_obj$isys@args$isysmethod$had <- forecast_fun
    
    # SET PARAMETERS for the HCR
    input$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = list(
        cod = c("Ftrgt" = f_cod, "Btrigger" = 5800,  "Blim" = 4200),
        had = c("Ftrgt" = f_had, "Btrigger" = 12822, "Blim" = 9227)
    )))
    
    # Set Forecast/ISYS Arguments
    # Using similar settings to Cod/Had and those defined in ICES_HCR function for whiting
    input$ctrl_obj$isys@args$isysList <- list(
        cod = list(fwd_trgt = c("fsq", "hcr"), fwd_yrs = 2, fwd_yrs_fsq = -2:0, fwd_yrs_average = -2:0, fwd_yrs_rec_start = 2015, fwd_yrs_sel = -3:-1),
        had = list(fwd_trgt = c("fsq", "hcr"), fwd_yrs = 2, fwd_yrs_fsq = 0,    fwd_yrs_average = -2:0, fwd_yrs_rec_start = 1993, fwd_yrs_sel = -3:-1)
    )
    
    # Initialize Advice
    ## Update simulation arguments
    input$args$management_lag <- 1
    # TODO: Change the below
    # arbitrarily set TAC to 1000 for intermediate year, instead of to previous year's TAC as advised
    input$args$adviceInit$cod[] <- 1000
    input$args$adviceInit$had[] <- 1000
    
    # RUN SIMULATION
    res <- runMixME(om = input$om, oem = input$oem, ctrl_obj = input$ctrl_obj, args = input$args)

    ## Check for effort optimisation failures
    res$tracking$optim

    ## Check for Management Procedure failures - effectively failure to return quotas
    res$tracking$iterfail

  return(res)
}

# Define funciton that will get risk and catch for each version of f_cod, f_had
doOne <- function(run_id,input_data){

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
    res <- obj_func(f_cod = this_Fcod, f_had = this_Fhad, input = input)
    
    # Get Catch directly from the 'tracking' object
    catch_cod <- sum(res$tracking$cod$stk["C.om", ac(2020:2039)], na.rm = TRUE)
    catch_had <- sum(res$tracking$had$stk["C.om", ac(2020:2039)], na.rm = TRUE)
    # Calculate total catch
    total_catch <- catch_cod + catch_had
    
    ## // Calculate the risk for all three stocks //
    # TODO: Improve to a probability somehow, multiple iterations if needed - I NOW HAVE THIS! YAY!

    ##############################################
    # APPARENTLY I DON'T HAVE MULTIPLE ITERATIONS AND SO THIS IS ONLY MARGINALLY DIFFERENT FROM PREV SIMULATION

    # Will need to chnage to same risk calculation as in Optimising_ftarget_in_MixME_mult_points_parallel.R
    # if I want GP models to work properly, as else eveyr risk is 0 or 1
    ###########################################
    
    # Define Blims here for safety
    Blim_cod <- 107000
    Blim_had <- 9227
    
    # Extract SSB directly - still focusing on the end of the simualtion in 2039
    ssb_cod_data <- c(res$tracking$cod$stk["SB.om", ac(2039)])
    ssb_had_data <- c(res$tracking$had$stk["SB.om", ac(2039)])

    # Remove large result object to preserve memory
    rm(res)   

    # Make sure NAs are calulatedas failures as they coudl be the stock crashing
    cod_is_failure <- is.na(ssb_cod_data) | (ssb_cod_data < Blim_cod)
    had_is_failure <- is.na(ssb_had_data) | (ssb_had_data < Blim_had)

    # Calculate Probability (Percentage of iterations below Blim)
    risk_cod <- mean(cod_is_failure)
    risk_had <- mean(had_is_failure)
    
    # Set up variable to return
    to_return <- c(Fcod = this_Fcod,Fhad = this_Fhad,RiskCod = risk_cod,RiskHad = risk_had,TotalCatch = total_catch)
    
    # Return the result
    return(to_return)
}

# Calculate how correlated two vectors of Fcod and Fhad are based on their distance in the grid
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

# Defining knowledge gradient acquisition fucntion so I can determine which points are best to run next
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
    # set knowledge gradient to 0 for any unsafe points - here has become prisks that are too high
    # TODO: Improve by removing any points in that are implausible when evaluating? or beforehand?
    if (prisk_cod[i] < eps || prisk_had[i] < eps) {
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

## SECOND PART

#' The objective of this script is to update the harvest control rule from constant catch 
#' to a hockey-stick harvest control rule with three parameters: ftarget, btrigger and blim.
#' 
#' # TODO: Consider changing Blim as well? Would give me a lot more dimensions but could be more in line with the goal above

## Load libraries
library(mse)  
library(FLCore)   
library(FLFishery)
library(FLasher) 
library(MixME)   

## Load tutorial data - I am using the correct data
data("mixedfishery_MixME_input")
input <- mixedfishery_MixME_input


# // SETUP FOR LOOP //

# Define Design Space as discrete with 0.02 increments
dat <- data.frame(expand.grid(
  Fcod = seq(0, 1, by=0.05),
  Fhad = seq(0, 1, by=0.05)
))

# Define Blims - sticking to ones given in fixed fishing mortality example: https://github.com/CefasRepRes/MixME/wiki/Fixed-fishing-mortality-management-strategy
Blim_cod <- 107000
Blim_had <- 9227

# Set an intial F target for both stocks so the first loop has something to work with
# Sticking to Ftargets given in fixed fishing mortality example: https://github.com/CefasRepRes/MixME/wiki/Fixed-fishing-mortality-management-strategy
f_cod_1 <- 0.28
f_had_1 <- 0.353
point_1 <- data.frame(Fcod = f_cod_1, Fhad = f_had_1)

# To prep for running in parallel, check how many cores you have
n_cores <- parallel::detectCores() - 1 
n_warmup <- 2 * n_cores - 1

# CHANGED: Pick 5 other RANDOM points from your grid to initialize the model with a set seed for reproducibility
# TODO: Could space apart equally as in Mike's code to explore the space more 
set.seed(123) 
warmup_indices <- sample(nrow(dat), n_warmup)
warmup_points <- dat[warmup_indices, ]
next_points <- rbind(point_1, warmup_points)

next_points


# Set max runs (kept low for testing) and initial round number
max_rounds <- 3
round_num <- 1

# Intiliase runs for safety
runs <- NULL


# START LOOP

for (iteration in 1:max_rounds) {
  
    # CHANGED: Provide six rows of points to run (due to nature of varlist function)
    points_to_run <- varlist(
    # The "Grid" - counts from 1 to 6
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

    names(data_frame_result) <- c("Fcod","Fhad","RiskCod","RiskHad","TotalCatch","error","warning","time")


    # Set up data frame to store data from current run - only keeping values that I want
    # NOte: TotalCatch is in the middle now
    dat_run <- data.frame(
    Fcod = c(data_frame_result$Fcod),
    Fhad = c(data_frame_result$Fhad),
    RiskCod = c(data_frame_result$RiskCod),
    RiskHad = c(data_frame_result$RiskHad),
    TotalCatch = c(data_frame_result$TotalCatch)
    )

    dat_run

    # Adding names to reference in GPs and for readability
    names(dat_run) <- c("Fcod","Fhad","RiskCod","RiskHad","TotalCatch")


    # Runs variable to count all runs so far and all their associated data
    runs <- rbind(runs, dat_run)

    runs

    # Taking log of the catch so GP models are more stable
    log_total_catch <- log(runs$TotalCatch + 1e-12) # add small constant to avoid log(0)
    risk_cod <- runs$RiskCod
    risk_had <- runs$RiskHad

    
    # SET UP THE GPS
    # Adding 1e-15 to nuggets to avoid 0 nugget variance
    # TODO: remove when risk calculations fixed
    gp_log_cat <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = log_total_catch,nugget=1e-12*var(runs$TotalCatch)+1e-15,covtype = "exp")
    gp_cod_risk <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = risk_cod,nugget=1e-12*var(risk_cod_input)+1e-15,covtype = "exp")
    gp_had_risk <- km(~.^2,design=runs[,c("Fcod","Fhad")],estim.method="MLE",response = risk_had,nugget=1e-12*var(risk_had_input)+1e-15,covtype = "exp")
    
    print("GPs done")
    
    # Find the remaining plausible points using BHM
    pred_risk_cod <- predict(gp_cod_risk, newdata = dat, type = "SK")
    pred_risk_had <- predict(gp_had_risk, newdata = dat, type = "SK")
    pred_log_cat <- predict(gp_log_cat, newdata = dat, type = "SK")
    
    # Get probability that risk =< -log(1000) for both cod and haddock
    prisk_cod <- pnorm(log(0.05), pred_risk_cod$mean, pred_risk_cod$sd + 1e-12)
    prisk_had <- pnorm(log(0.05), pred_risk_had$mean, pred_risk_had$sd + 1e-12)
    
    # Probability catch is =< current max
    pcat <- pnorm(log(current_max), pred_log_cat$mean, pred_log_cat$sd + 1e-12)
    
    # Set epsilon for plausibility threshold
    eps <- 1e-4
    # Determine plausible points where min(1 - pcat, prisk) > eps
    possible <- pmin((1 - pcat), (1 - prisk_cod), (1 - prisk_had), 1) > eps

    # Visualises region that has risk < threshold and better catch than best evaluated so far - NEEDS TO BE 3D NOW
    # TODO: Check I am naming this the correct way around
    image2D(matrix(possible * (1-pcat),nrow=21),y=sort(unique(dat$Fcod)),x=sort(unique(dat$Fhad)),xlab="Fhad",ylab="Fcod",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
    
    # Calculate KG
    mu <- pred_log_cat$mean
    sigma <- pred_log_cat$sd
    # TODO: May need ot change. Calculating this deterministically as obs_noise_var = 0.
    kg <- knowledge_gradient_sim(mu, sigma, gp_log_cat, obs_noise_var = 0, nsim = 100, prisk_cod = prisk_cod, prisk_had = prisk_had, eps = 1e-4)
    
    print("kg done")
    
    # Calculate the remaining points
    dat_with_kg <- dat
    dat_with_kg$kg <- kg
    dat_with_kg$possible <- possible
    names(dat_with_kg) <- c("Fcod","Fhad","kg","possible")
    
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
        break
    }
    
    # Print how many points are left
    cat("Unevaluated candidates with KG > 0:", nrow(cand), "\n")
    
    # Select next points - keeping consistent with number of random points selected at the start
    if (nrow(cand) <= 6) {
        next_points <- cand[order(-cand$kg), c("Fcod", "Fhad")]
    } else {
        top_candidates <- cand[order(-cand$kg), ][1:nrow(cand), ]
        set.seed(123)
        km_result <- kmeans(top_candidates[, c("Fcod", "Fhad")], centers = 6)
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
    rm(gp_log_cat, gp_cod_risk, gp_had_risk)
    
    # Remove the prediction vectors to be safe
    rm(pred_risk_cod, pred_risk_had, pred_log_cat)

    # KEEP ONLY WANTED COLUMNS IN runs
    # Define the columns we actually care about, otherwise we can't do rbind to runs properly next iteration
    important_cols <- c("Fcod","Fhad","RiskCod","RiskHad","TotalCatch")
    
    # Only keep those columns before binding
    runs <- runs[, important_cols]
    
    # // LOOP ENDS //
    round_num <- iteration

}

