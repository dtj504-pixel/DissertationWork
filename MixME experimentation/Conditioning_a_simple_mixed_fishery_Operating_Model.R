## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)

## load example data
data("mixedfishery_MixME_om")

## A list of stks (FLBiols) and flts (FLFisheries)
summary(mixedfishery_MixME_om)

## Dimensions for each stock and fleet
summary(mixedfishery_MixME_om$stks)
summary(mixedfishery_MixME_om$flts)

## Plot recruitment and total stock biomass for both stocks
plot(mixedfishery_MixME_om$stks)

## Plot effort for both fleets
plot(FLQuants(OTB_A = mixedfishery_MixME_om$flts$OTB_A@effort,
              OTB_B = mixedfishery_MixME_om$flts$OTB_B@effort))

## Plot catchability for cod by both fleets
plot(FLQuants(OTB_A = FLQuant(mixedfishery_MixME_om$flts$OTB_A$cod@catch.q["alpha",]),
              OTB_B = FLQuant(mixedfishery_MixME_om$flts$OTB_B$cod@catch.q["alpha",])))

## Plot catchability for haddock by both fleets
plot(FLQuants(OTB_A = FLQuant(mixedfishery_MixME_om$flts$OTB_A$had@catch.q["alpha",]),
              OTB_B = FLQuant(mixedfishery_MixME_om$flts$OTB_B$had@catch.q["alpha",])))

out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = TRUE)
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

## Check historic quota-share (landings-share) patterns
plot(FLQuants(OTB_A = attr(mixedfishery_MixME_om$flts$OTB_A$cod, "quotashare"),
              OTB_B = attr(mixedfishery_MixME_om$flts$OTB_B$cod, "quotashare")))
plot(FLQuants(OTB_A = attr(mixedfishery_MixME_om$flts$OTB_A$had, "quotashare"),
              OTB_B = attr(mixedfishery_MixME_om$flts$OTB_B$had, "quotashare")))

out <- stfMixME(mixedfishery_MixME_om,
                method = "yearMeans", 
                nyears = 20, 
                wts.nyears = 3, 
                sel.nyears = 3, 
                qs.nyears = 3, 
                verbose = TRUE)


## Check projection
plot(out$stks$had)
plot(out$flts$OTB_A)
plot(out$flts$OTB_B)

## Overwrite outputs
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

  ## SET UNITS FOR HARVEST SLOT
  units(xx@harvest) <- "f"
  
  ## remove excess data
  stock.n(xx)[] <- NA
  stock(xx)[]   <- NA
  
  ## return result
  return(xx)
}))

input <- makeMixME(om = mixedfishery_MixME_om, 
                   catch_obs = stk_oem, 
                   management_lag = 0, 
                   management_type = "fixedC", 
                   effort_type = "min",
                   parallel = FALSE)

## Update observation arguments
input$oem@args$catch_timing$cod <- 0
input$oem@args$catch_timing$had <- 0

## Update management arguments
input$ctrl_obj$hcr@args$ctrg$cod <- 1000
input$ctrl_obj$hcr@args$ctrg$had <- 1000

## Update simulation arguments
input$args$iy

## Update fbar ranges
input$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
input$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)

res <- runMixME(om  = input$om, 
                oem = input$oem,
                ctrl_obj = input$ctrl_obj,
                args     = input$args)