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