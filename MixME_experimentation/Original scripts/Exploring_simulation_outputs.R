## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)

##Load the results of the simple_mixed_fishery tutorial
data("mixedfishery_MixME_input")

res <- runMixME(om  = mixedfishery_MixME_input$om, 
                oem = mixedfishery_MixME_input$oem,
                ctrl_obj = mixedfishery_MixME_input$ctrl_obj,
                args     = mixedfishery_MixME_input$args)

# CHECKING SIMULATION QUALITY

## Check for advice failure
apply(res$tracking$iterfail, 1, mean)

## Check for effort optimisation failure
res$tracking$optim

## Check effort optimisation message - useful if we have had any optimisation failures
res$tracking$message

## Check over-quota catches - these could be high due to implementation error
round(res$tracking$overquota,3)["cod",,,1]
round(res$tracking$overquota,3)["had",,,1]

## What is the maximum overshoot?
max(res$tracking$overquota, na.rm = TRUE)

# EXPLORE STOCK AND FLEET DYNAMICS

plot_timeseries_MixME(res, quantity = "ssb")
plot_timeseries_MixME(res, quantity = "fbar")
plot_timeseries_MixME(res, quantity = "catch")
plot_timeseries_MixME(res, quantity = "uptake")

## Check choke stock
res$tracking$choke

plot_timeseries_MixME(res, quantity = "effort", minyr = 2020)

## Add reference points - Blim is not Btrigger
hcrpars <- list(cod = c(Blim = 107000), had = c(Blim = 9227))
res$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = hcrpars))

## Plot risk time-series
plot_timeseries_MixME(res, quantity = "risk")

summary_catch_MixME(res, minyr = 2020, byfleet = TRUE)
summary_uptake_MixME(res, minyr = 2020, byfleet = TRUE)

#CHANGING MANAGEMENT PARAMETERS

input <- mixedfishery_MixME_input
input$ctrl_obj$hcr@args$ctrg$cod <- 10000
input$ctrl_obj$hcr@args$ctrg$had <- 1000

res2 <- runMixME(om  = input$om, 
                oem = input$oem,
                ctrl_obj = input$ctrl_obj,
                args     = input$args)

## Check for advice failure
apply(res2$tracking$iterfail, 1, mean)

## Check for effort optimisation failure
res2$tracking$optim[2,,]

## Check for effort optimisation message
res2$tracking$message

## Check maximum overshoot
max(res2$tracking$overquota, na.rm = TRUE)

## Check choke stock
res2$tracking$choke

## plot stock properties
plot_timeseries_MixME(res2, quantity = "ssb")
plot_timeseries_MixME(res2, quantity = "fbar")
plot_timeseries_MixME(res2, quantity = "catch")
plot_timeseries_MixME(res2, quantity = "uptake")

# Trying fishing the stocks even more aggressively
input$ctrl_obj$hcr@args$ctrg$cod <- 5000000
input$ctrl_obj$hcr@args$ctrg$had <- 1000000

res3 <- runMixME(om  = input$om, 
                 oem = input$oem,
                 ctrl_obj = input$ctrl_obj,
                 args     = input$args)

## Check for advice failure
apply(res3$tracking$iterfail, 1, mean)

## Check for effort optimisation failure
res3$tracking$optim[2,,]

## Check for effort optimisation message
res3$tracking$message

## Check maximum overshoot
max(res3$tracking$overquota, na.rm = TRUE)

## Check choke stock
res3$tracking$choke

## plot stock properties
plot_timeseries_MixME(res3, quantity = "ssb")
plot_timeseries_MixME(res3, quantity = "fbar")
plot_timeseries_MixME(res3, quantity = "catch")
plot_timeseries_MixME(res3, quantity = "uptake")
plot_timeseries_MixME(res3, quantity = "effort", minyr = 2020, maxyr = 2038)