# I should try out Matt's code that he has sent over in an email on 4th Decemeber
# to check that the code works on my machine and I can put it in the right order, etc.

## === Define a custom harvest control rule ===
ICES_HCR <- function (stk, args, hcrpars, tracking) {
  
# stk contains information about fish stocks, e.g. age
# args contains useful arguments such as ay and mlag
# hcrpars contains the parameters of the HCR, e.g. Ftrgt, Btrigger, Blim
# tracking contains the current tracking object and is currently returned unchanged

  ## Extract year arguments

  # ay is the current advice year
  ay <- args$ay
  # mlag is the management lag in years
  mlag <- args$management_lag
  # this is the number of iterations
  ni <- dims(stk)[["iter"]]
  
  # ------------------------------#
  # Short-term forecast
  # ------------------------------#
  
  ## Carry out short-term forecast to get ssb at the beginning of the advice year
  
  ## historical geomean to estimate recruitment for the stock (prevents big spikes in recruitment having a large effect)
  sr0 <- as.FLSR(stk, model = "geomean")
  # propagate sr0 params to match number of simulations
  sr0@params <- FLCore::propagate(sr0@params, iter = ni)
  
  # hardcoded using different starting years for different stocks when estimating recruitment
  if(stk@name == "cod") stk_n <- window(stk@stock.n, start = 2015)
  if(stk@name == "had") stk_n <- window(stk@stock.n, start = 1993)
  if(stk@name == "whg") stk_n <- window(stk@stock.n, start = 2010)
  
  # We get the row of data for the youngest fish and calculate the geometric mean for that row
  # We do this by taking a log before calculating the average
  # as this reduces the effect of large values, and then exponentiating
  sr0@params[] <- exp(yearMeans(log(stk_n[1,])))
  


  ## find the first year with any data
  minyr <- dims(stk@stock[!is.na(stk@stock.n)])$minyear
  ## remove any years before this
  stk0  <- window(stk, start = minyr)
  
  ## extend stock object by one year
  ## automatically fills in parameters TODO: How? AVerage of last 3 years?
  stk0 <- FLasher::stf(stk0, 1)
  
  # FLasher::fwd will throw an error if there are NAs in weights in future years.
  # I need to zero these if associated numbers are zero
  FLCore::discards.wt(stk0)[FLCore::discards.n(stk0) == 0] <- 0
  FLCore::landings.wt(stk0)[FLCore::landings.n(stk0) == 0] <- 0
  

  ## find status quo F slightly differently for each stock
  # cod and whiting average last three eyars, haddock average last year only
  if(stk@name == "cod") fwd_yrs_fsq <- -2:0
  if(stk@name == "had") fwd_yrs_fsq <- 0
  if(stk@name == "whg") fwd_yrs_fsq <- -2:0
  
  # estimated fishing mortality for the forward year for each stock
  fsq <- yearMeans(fbar(stk)[,as.character(fwd_yrs_fsq+ay-mlag)])
  
  ## FLasher cannot handle fsq=0
  fsq <- ifelse(fsq==0, 0.001,fsq)
  


  ## Construct forward fishing mortality
  # Will overwrite the second row later
  # Same number of columns as simulations
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
  
  # ------------------------------#
  # HCR
  # ------------------------------#
  
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

forecast_fun <- function(stk, tracking, ctrl,
                         args,                       # contains ay (assessment year)
                         fwd_trgt = c("fsq", "hcr"), # target in forecast
                         fwd_yrs = 2,                # number of years to add
                         fwd_yrs_fsq = -2:0,         # years used to calculate fsq
                         fwd_yrs_average = -3:0,     # years used for averages
                         fwd_yrs_rec_start = NULL,   # recruitment 
                         fwd_yrs_sel = -3:-1,        # selectivity
                         fwd_yrs_lf_remove = -2:-1,
                         fwd_splitLD = TRUE) {
  
  ## get current (assessment) year
  ay <- args$ay
  
  ## get management lag
  mlag <- args$management_lag
  
  ## number of iterations
  niter <- dim(stk)[6]
  
  ## geomean to estimate recruitment for the stock
  sr0 <- as.FLSR(stk, model = "geomean")
  sr0@params <- FLCore::propagate(sr0@params, iter = niter)
  
  if (!is.null(fwd_yrs_rec_start)) {
    stk_n <- window(stk@stock.n, start = fwd_yrs_rec_start)
  } else {
    stk_n <- stk@stock.n
  }
  sr0@params[] <- exp(yearMeans(log(stk_n[1,])))
  
  ## truncate to min data year
  minyr <- dims(stk@stock[!is.na(stk@stock.n)])$minyear # min year where data exists
  stk0  <- window(stk, start = minyr)
  
  ## extend stock object
  stk0 <- FLasher::stf(stk0, fwd_yrs+1)
  
  # FLasher::fwd will throw an error if there are NAs in weights in future years.
  # I need to zero these if associated numbers are zero
  FLCore::discards.wt(stk0)[FLCore::discards.n(stk0) == 0] <- 0
  FLCore::landings.wt(stk0)[FLCore::landings.n(stk0) == 0] <- 0
  
  ## find status quo F
  fsq <- yearMeans(fbar(stk)[,as.character(fwd_yrs_fsq+ay-mlag)])
  
  ## FLasher cannot handle fsq=0
  fsq <- ifelse(fsq==0, 0.001,fsq)
  
  ## Construct forward control
  targ <- matrix(0,nrow=fwd_yrs+1, ncol = niter)
  targ[1,] <- fsq
  targ[2,] <- ctrl@iters[,"value",]
  targ[3,] <- ctrl@iters[,"value",]
  
  ctrl0 <- fwdControl(list(
    year  = c(ay,ctrl@target$year,ctrl@target$year+1),
    quant = "fbar",
    value = c(targ)))
  
  ## project stock forward
  stk_fwd <- FLasher::fwd(stk0, sr = sr0, control = ctrl0)
  
  ## Extract catch target for the advice year (3 decimal places)
  TAC <- round(c(catch(stk_fwd)[,ac(ay+mlag)]),3)
  
  ## Get SSB at the end of the advice year
  TACyr_ssb <- c(ssb(stk_fwd)[,ac(ay+mlag+1)])
  
  ## Find iterations where SSB at end of TAC year is < Blim
  belowBlim <- which(TACyr_ssb < attr(ctrl, "Blim"))
  
  ## Go through model fits - where SSB < Blim, forecast to target Blim
  if (length(belowBlim) > 0) {
    
    ## define forward control targetting Blim
    targ <- matrix(0,nrow=fwd_yrs+1, ncol = niter)
    targ[1,] <- fsq
    targ[2,] <- c(attr(ctrl,"Blim"))
    targ[3,] <- ctrl@iters[,"value",]
    
    ctrl_blim <- fwdControl(list(
      year  = c(ay,ay+mlag,ay+mlag+1),
      quant = c("fbar","ssb_end","fbar"),
      value = c(targ)))
    
    stk_blim <- FLasher::fwd(stk0, sr = sr0, control = ctrl_blim)
    
    ## Find iterations with zero TAC advice
    zeroTAC <- ssb(stk_blim)[,ac(ay+mlag+1)] < attr(ctrl,"Blim")
    
    ## update TAC
    TAC[belowBlim] <- round(c(catch(stk_blim)[,ac(ay+mlag)])[belowBlim],3)
    TAC[zeroTAC]   <- 0
  }
  
  ## Construct fwd control object
  ctrl0 <- fwdControl(list(year = ay + mlag, quant = "catch", value = TAC))
  
  return(list(ctrl     = ctrl0,
              tracking = tracking))
}




## SECOND PART

#' The objective of this script is to update the harvest control rule from
#' constant catch to a hockey-stick harvest control rule with three parameters:
#' ftarget, btrigger and blim.

## Load libraries
library(mse)      # needed to create MSE control object
library(FLCore)   # basic FLR structures
library(FLFishery)# FLFishery structures
library(FLasher)  # MP short-term forecast
library(MixME)    # main simulation package

## Load tutorial data 
data("mixedfishery_MixME_input")
input <- mixedfishery_MixME_input

## set estimation method to perfect observations 
input$ctrl_obj$est@args$estmethod$cod
input$ctrl_obj$est@args$estmethod$had

## we don't need any additional arguments for stock estimation
input$ctrl_obj$est@args$fitList <- NULL
input$ctrl_obj$est@args$fwdList <- NULL


input$ctrl_obj$hcr@args$hcrmethod$cod <- ICES_HCR
input$ctrl_obj$hcr@args$hcrmethod$had <- ICES_HCR

## Define the HCR parameters
input$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = list(cod = c("Ftrgt"=0.29, "Btrigger"=5800, "Blim"=4200),
                                                          had = c("Ftrgt"=0.353, "Btrigger"=12822, "Blim"=9227))))

## Change implementation method
input$ctrl_obj$isys@args$isysmethod$cod <- forecast_fun
input$ctrl_obj$isys@args$isysmethod$had <- forecast_fun

## Define short-term forecast parameters forecast
input$ctrl_obj$isys@args$isysList <- list(cod = list(fwd_trgt = c("fsq", "hcr"), # what to target in forecast
                                                     fwd_yrs = 2,                # number of years to add
                                                     fwd_yrs_fsq = -2:0,         # years used to calculate fsq
                                                     fwd_yrs_average = -2:0,     # years used for averages
                                                     fwd_yrs_rec_start = 2015,   # start year for recruitment 
                                                     fwd_yrs_sel = -3:-1,        # years used for selectivity
                                                     fwd_yrs_lf_remove = NULL),   # years to overwrite landings fraction with most recent data year
                                          had = list(fwd_trgt = c("fsq", "hcr"),
                                                     fwd_yrs = 2,
                                                     fwd_yrs_fsq = 0,
                                                     fwd_yrs_average = -2:0,
                                                     fwd_yrs_rec_start = 1993,
                                                     fwd_yrs_sel = -3:-1,
                                                     fwd_yrs_lf_remove = NULL))

#' We will have a lag of 1 year between the generation and implementation of 
#' catch advice. We therefore need an initial catch target for each stock - this
#' would typically be last year's TAC

## Update simulation arguments
input$args$management_lag <- 1
input$args$adviceInit$cod[] <- 1000
input$args$adviceInit$had[] <- 1000

## Run simulation
system.time({res <- runMixME(input$om,
                             input$oem,
                             input$ctrl_obj,
                             input$args)})

## Check for effort optimisation failures
res$tracking$optim

## Check for MP failures
res$tracking$iterfail

## Check SSB
plot(res$tracking$cod$stk["SB.om"])
plot(res$tracking$had$stk["SB.om"])

## Check catches
plot(res$tracking$cod$stk["C.om"])
plot(res$tracking$had$stk["C.om"])

## Check quota uptake
summary_uptake_MixME(res)
