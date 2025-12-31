#IMPORTANT NOTE:
# " we will assume that we can perfectly observe the stock without error"

# Focusing on this example first in my project

## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)


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
  
  # get catches from each fleet and convert to FLStock
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


#ASSEMBLE SIMULATION INPUTS

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

## check estimation method
input$ctrl_obj$est@args$estmethod$cod
input$ctrl_obj$est@args$estmethod$had

## check the HCR for both stocks
input$ctrl_obj$hcr@args$hcrmethod$cod
input$ctrl_obj$hcr@args$hcrmethod$had

# Set the target fishing mortality for both stocks - may be recommended by ICES

#THIS IS WHAT I SHOULD BE CHANGING EACH TIME and running an algorithm to find the best one
input$ctrl_obj$hcr@args$ftrg$cod <- 0.28  # use MSY f-target
input$ctrl_obj$hcr@args$ftrg$had <- 0.353 # use MSY f-target

# Check initial projection year is as we set it
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

## Check quota uptake for cod and haddock
res$tracking$uptake["cod",,,1]
res$tracking$uptake["had",,,1]

## Check maximum overshoot of the quota by fleets
max(res$tracking$overquota, na.rm = TRUE)


#EXPLORE STOCK AND FLEET DYNAMICS
## plot stock properties

#plot ssb trajectories for cod and haddock
plot_timeseries_MixME(res, quantity = "ssb")
# plot average fishing mortality rates for cod and haddock
plot_timeseries_MixME(res, quantity = "fbar", minyr = 2020)
# plot total catch trajectories for cod and haddock
plot_timeseries_MixME(res, quantity = "catch")
# plot quota uptake trajectories for cod and haddock
plot_timeseries_MixME(res, quantity = "uptake")

## Check choke stock in each year
res$tracking$choke

## plot fleet properties
plot_timeseries_MixME(res, quantity = "effort", minyr = 2020, maxyr = 2038)

## Define Blim for each stock
hcrpars <- list(cod = c(Blim = 107000), had = c(Blim = 9227))  # https://doi.org/10.17895/ices.advice.5897

# Update the control object with the new HCR parameters
res$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = hcrpars))

## Plot risk over time for each stock
plot_timeseries_MixME(res, quantity = "risk")


#CONVERSION OF FISHING MORTALITY TARGET TO CATCH TARGET

# Target fishing mortality advised by HCR - these could change each year
res$tracking$cod$stk["hcr.adv",]
res$tracking$had$stk["hcr.adv",]

# Stock TAC generated from the target fishing mortality above
res$tracking$cod$advice[1,,]
res$tracking$had$advice[1,,]

# Fleet quota - total TAC divided among fleets using quota shares from earlier
res$tracking$quota["cod",,,]
res$tracking$quota["had",,,]

# Implemented fishing mortality - what actually happened in the model compared to the above
res$tracking$cod$stk["F.om",]
res$tracking$had$stk["F.om",]
