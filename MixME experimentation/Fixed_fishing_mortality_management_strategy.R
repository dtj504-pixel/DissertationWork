## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)

## load example data
data("mixedfishery_MixME_om")

# GENERATE OM AND OEM - repeating process form previous tutorials
out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = TRUE)
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

out <- stfMixME(mixedfishery_MixME_om, method = "yearMeans", nyears = 20,  wts.nyears = 3,  sel.nyears = 3, qs.nyears = 3, verbose = TRUE)

## Add new outputs
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

## initial projection year
iy = 2020

## arbitrary effort-based target for each fleet
ctrlArgs <- lapply(1:length(mixedfishery_MixME_om$flts), function(x) {
  list(year = iy,
       quant = "effort",
       fishery = names(mixedfishery_MixME_om$flts)[x],
       value = 1)
})
ctrlArgs$FCB <- makeFCB(biols = mixedfishery_MixME_om$stks, 
                        flts = mixedfishery_MixME_om$flts)

## Generate effort-based FLasher::fwd forecast control
flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)

omfwd <- FLasher::fwd(object    = mixedfishery_MixME_om$stks, 
                      fishery   = mixedfishery_MixME_om$flts, 
                      control   = flasher_ctrl)

mixedfishery_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]
mixedfishery_MixME_om$stks$cod@n[,ac(iy)] <- omfwd$biols$cod@n[,ac(iy)]

## convert FLBiol to FLStocks
stk_oem <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {
  
  ## identify where corresponding catch occurs in fleet structure
  catch <- sapply(mixedfishery_MixME_om$flts, function(y) which(names(y) %in% name(x)))
  
  ## coerce to FLStock
  xx <- as.FLStock(x, mixedfishery_MixME_om$flts, full = FALSE, catch = catch)

    
  ## SET UNITS FOR HARVEST SLOT (ADD THIS LINE)
  units(xx@harvest) <- "f"
  
  ## remove excess data
  stock.n(xx)[] <- NA
  stock(xx)[]   <- NA
  
  ## return result
  return(xx)
}))


#ASSEMBLE SIMULATION INPUTS

input <- makeMixME(om = mixedfishery_MixME_om, catch_obs = stk_oem, management_lag = 0, management_type = "fixedF", parallel = FALSE)

## observation error model
input$oem@args$catch_timing$cod <- 0
input$oem@args$catch_timing$had <- 0

input$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
input$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)

## check estimation module
input$ctrl_obj$est@args$estmethod$cod
input$ctrl_obj$est@args$estmethod$had

## update HCR
input$ctrl_obj$hcr@args$hcrmethod$cod
input$ctrl_obj$hcr@args$hcrmethod$had

input$ctrl_obj$hcr@args$ftrg$cod <- 0.28  # use MSY f-target
input$ctrl_obj$hcr@args$ftrg$had <- 0.353 # use MSY f-target

## Check initial projection year
input$args$iy

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

## Check quota uptake
res$tracking$uptake["cod",,,1]
res$tracking$uptake["had",,,1]

## Check maximum overshoot
max(res$tracking$overquota, na.rm = TRUE)


#EXPLORE STOCK AND FLEET DYNAMICS
## plot stock properties
plot_timeseries_MixME(res, quantity = "ssb")
plot_timeseries_MixME(res, quantity = "fbar", minyr = 2020)
plot_timeseries_MixME(res, quantity = "catch")
plot_timeseries_MixME(res, quantity = "uptake")

## Check choke stock
res$tracking$choke

## plot fleet properties
plot_timeseries_MixME(res, quantity = "effort", minyr = 2020, maxyr = 2038)

## Add reference points
hcrpars <- list(cod = c(Blim = 107000),# 
                had = c(Blim = 9227))  # https://doi.org/10.17895/ices.advice.5897
res$ctrl_obj$phcr <- mseCtrl(args   = list(hcrpars = hcrpars))

## Plot risk time-series
plot_timeseries_MixME(res, quantity = "risk")


#CONVERSION OF FISHING MORTALITY TARGET TO CATCH TARGET
## Target fishing mortality
res$tracking$cod$stk["hcr.adv",]
res$tracking$had$stk["hcr.adv",]

## Stock TAC
res$tracking$cod$advice[1,,]
res$tracking$had$advice[1,,]

## Fleet quota
res$tracking$quota["cod",,,]
res$tracking$quota["had",,,]

## Implemented fishing mortality
res$tracking$cod$stk["F.om",]
res$tracking$had$stk["F.om",]