## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)

## load example data
data("singlestock_MixME_om")

## A list of stks (FLBiols) and flts (FLFisheries)
summary(singlestock_MixME_om)

## Dimensions for each stock and fleet
summary(singlestock_MixME_om$stks)
summary(singlestock_MixME_om$flts)

## Check that stock and catch names match
names(singlestock_MixME_om$flts$fleet) %in% names(singlestock_MixME_om$stks)

## Check catchability dimensions
dim(catch.q(singlestock_MixME_om$flts$fleet$had))

## Check stock-recruit relationship
singlestock_MixME_om$stks$had@rec

## Calculate quota-share
out <- calculateQuotashare(stks = singlestock_MixME_om$stks, 
                           flts = singlestock_MixME_om$flts, verbose = TRUE)
singlestock_MixME_om$stks <- out$stks
singlestock_MixME_om$flts <- out$flts

## Check 
attr(singlestock_MixME_om$flts$fleet$had, "quotashare")


# Creating a projection 20 years ahead using parameter values averaged over the most recent three years
out <- stfMixME(singlestock_MixME_om,
                method = "yearMeans", 
                nyears = 20, 
                wts.nyears = 3, 
                sel.nyears = 3, 
                qs.nyears = 3, 
                verbose = TRUE)

## Check projection
plot(out$stks$had)
plot(out$flts$fleet)

## Add new outputs from the prediction to get an operating model stretching into the future
singlestock_MixME_om$stks <- out$stks
singlestock_MixME_om$flts <- out$flts

## Initial simulation year
iy <- 2020

## Create an arbitrary forecast target for each fishery, as we only need the population of the stock at the start of 2020
ctrlArgs <- lapply(1:length(singlestock_MixME_om$flts), function(x) {
  list(year = iy,
       quant = "effort",
       fishery = names(singlestock_MixME_om$flts)[x],
       value = 1)
})
## make an FCB matrix for which fleets affect which biological stocks
ctrlArgs$FCB <- makeFCB(biols = singlestock_MixME_om$stks, flts = singlestock_MixME_om$flts)

## Generate effort-based FLasher::fwd forecast control
flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)

## carry out the projection for 2020 stock
omfwd <- FLasher::fwd(object    = singlestock_MixME_om$stks, 
                      fishery   = singlestock_MixME_om$flts, 
                      control   = flasher_ctrl)

## assign the projected numbers
singlestock_MixME_om$stks$had@n[,ac(iy)] <- omfwd$biols$had@n[,ac(iy)]


## convert FLBiol to FLStocks and use a fixed catch rule
stk_oem <- FLStocks(lapply(singlestock_MixME_om$stks, function(x) {
  xx <- as.FLStock(x, singlestock_MixME_om$flts$fleet)
  stock.n(xx)[] <- NA
  stock(xx)[]   <- NA
  harvest(xx)[] <- NA
  return(xx)
}))

# MixME input object creation
singlestock_MixME_input <- makeMixME(om = singlestock_MixME_om, 
                                     catch_obs = stk_oem, 
                                     management_lag = 0, 
                                     management_type = "fixedC", 
                                     parallel = FALSE)


## Stock estimation
singlestock_MixME_input$ctrl_obj$est@args

## Harvest control rule parameterisation
singlestock_MixME_input$ctrl_obj$phcr

## Harvest control rule (update catch target)
singlestock_MixME_input$ctrl_obj$hcr@args

## Advice implementation
singlestock_MixME_input$ctrl_obj$isys@args

## Forward projection
singlestock_MixME_input$ctrl_obj$fwd@args

singlestock_MixME_input$ctrl_obj$hcr@args$ctrg$had <- 1000



#Examining argument to customise and check if sensible
## Full set of simulation arguments
singlestock_MixME_input$args

## Update fbar ranges
singlestock_MixME_input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)

# Arguments of the observation error model
singlestock_MixME_input$oem@args

## Update observation arguments
singlestock_MixME_input$oem@args$catch_timing$had <- 0



# Now I can run the simulation!
res <- runMixME(om  = singlestock_MixME_input$om, 
                oem = singlestock_MixME_input$oem,
                ctrl_obj = singlestock_MixME_input$ctrl_obj,
                args     = singlestock_MixME_input$args)

#Check what simulated dynamics look like
plot_timeseries_MixME(res, quantity = "ssb")
plot_timeseries_MixME(res, quantity = "catch")
plot_timeseries_MixME(res, quantity = "uptake")
plot_timeseries_MixME(res, quantity = "fbar")