#IMPORTANT NOTE:
# " we will assume that we can perfectly observe the stock without error"

# Focusing on this example first in my project

## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)

# //DEFINE KG FUNCTION HERE//

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


#ASSEMBLE SIMULATION INPUTS

# // Set an intial F target for both stocks so the first loop has something to work with //

# // LOOP STARTS HERE AS WE ARE CHANGING FTARGET //

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

# // WE HAVE NOW RUN THE FULL MODEL FOR A SINGLE SET OF F TARGETS //

# Now, we get the total catch of both cod and haddock

# Add this to a variable similar to runs with the Ftargets for cod and haddock

# Find the remaining plausible points using BHM

# Calculate KG for each plausible point

# Set the point with the highest KG as the one to evaluate next time

# // LOOP ENDS //





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

# //DEFINE KG FUNCTION HERE//

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

# Set up the observation error model
# Cod and haddock catches occur at the start of the year so timing=0
# Not very realistic - could consider changing
input$oem@args$catch_timing$cod <- 0
input$oem@args$catch_timing$had <- 0

# Define the age range for calculating average fishing mortality (Fbar) for both stocks
input$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
input$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)

# SET UP THE GPS






#ASSEMBLE SIMULATION INPUTS

# Set an intial F target for both stocks so the first loop has something to work with

f_cod = 0.28
f_had = 0.353

# Define Design Space
lower_bounds <- c(f_cod = 0.0, f_had = 0.0)
upper_bounds <- c(f_cod = 1.0, f_had = 1.0)

# // LOOP STARTS HERE AS WE ARE CHANGING FTARGET //

obj_func <- function(f_cod, f_had){

    # Create the main MixME input object
    # This bundles together all the components needed to run the MSE
    input <- makeMixME(om = mixedfishery_MixME_om, catch_obs = stk_oem, management_lag = 0, management_type = "fixedF", parallel = FALSE)

    ## check estimation method
    input$ctrl_obj$est@args$estmethod$cod
    input$ctrl_obj$est@args$estmethod$had

    ## check the HCR for both stocks
    input$ctrl_obj$hcr@args$hcrmethod$cod
    input$ctrl_obj$hcr@args$hcrmethod$had

    # Set the target fishing mortality for both stocks - may be recommended by ICES

    #THIS IS WHAT I SHOULD BE CHANGING EACH TIME and running an algorithm to find the best one
    input$ctrl_obj$hcr@args$ftrg$cod <- f_cod  # use MSY f-target
    input$ctrl_obj$hcr@args$ftrg$had <- f_had # use MSY f-target

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

    # // WE HAVE NOW RUN THE FULL MODEL FOR A SINGLE SET OF F TARGETS //

    # Now, we get the total catch of both cod and haddock

    # We iterate through all fleets, check if they catch 'cod', and sum up the catch
    catch_cod <- Reduce("+", lapply(res$om$flts, function(f) {
    if("cod" %in% names(f)) {
        return(catch(f[["cod"]])) 
    } else {
        return(FLQuant(0))
    }
    }))

    # We iterate through all fleets, check if they catch 'had', and sum up the catch
    catch_had <- Reduce("+", lapply(res$om$flts, function(f) {
    if("had" %in% names(f)) {
        return(catch(f[["had"]])) 
    } else {
        return(FLQuant(0))
    }
    }))

    # Calculate total catch over the projection period (2020-2039)
    total_catch_cod <- sum(catch_cod[,ac(2020:2039)])
    total_catch_had <- sum(catch_had[,ac(2020:2039)])
    total_catch <- total_catch_cod + total_catch_had

    # Add this to a variable similar to runs with the Ftargets for cod and haddock
    runs <- runs + new datat with catch and two ftargets and the two risks

    # Find the remaining plausible points using BHM

    # Calculate KG for each plausible point

    # Set the point with the highest KG as the one to evaluate next time

    # // LOOP ENDS //

}














## CURRENT CODE FOR THIS:::::

library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)
library(DiceKriging) # For the GP later
library(dplyr)


# 1. SETUP: PREPARE THE OPERATING MODEL (Run Once)
data("mixedfishery_MixME_om")

# Calculate Quota Shares based on history
out <- calculateQuotashare(stks = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts, verbose = FALSE)

# Update the stocks and fleets with the calculated quota shares
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

# Project forward (STF) to initialize future years
out <- stfMixME(mixedfishery_MixME_om, method = "yearMeans", 
                nyears = 20, wts.nyears = 3, sel.nyears = 3, 
                qs.nyears = 3, verbose = FALSE)

# Add new outputs from future years to the operating model
mixedfishery_MixME_om$stks <- out$stks
mixedfishery_MixME_om$flts <- out$flts

# Initialize year 2020 with arbitrary effort levels to set starting conditions
iy <- 2020

ctrlArgs <- lapply(1:length(mixedfishery_MixME_om$flts), function(x) {
  list(year = iy, quant = "effort", fishery = names(mixedfishery_MixME_om$flts)[x], value = 1)
})

ctrlArgs$FCB <- makeFCB(biols = mixedfishery_MixME_om$stks, flts = mixedfishery_MixME_om$flts)

flasher_ctrl <- do.call(FLasher::fwdControl, ctrlArgs)


# 1. Run the forward projection for 2020
omfwd <- FLasher::fwd(object = mixedfishery_MixME_om$stks, 
                      fishery = mixedfishery_MixME_om$flts, 
                      control = flasher_ctrl)

# 2. UPDATE BOTH STOCKS AND FLEETS
# This ensures the 2020 catch in the fleet object matches the 2020 mortality in the stock object.
mixedfishery_MixME_om$stks <- omfwd$biols
mixedfishery_MixME_om$flts <- omfwd$fisheries 

# 3. Generate Observation Error Model (OEM)
# "We assume we can perfectly observe the stock"
stk_oem <- FLStocks(lapply(mixedfishery_MixME_om$stks, function(x) {

  # Identify which fleet catches this stock
  catch <- sapply(mixedfishery_MixME_om$flts, function(y) which(names(y) %in% name(x)))

  # Create the FLStock object
  # (This sums up the catch from the updated fleets)
  xx <- as.FLStock(x, mixedfishery_MixME_om$flts, full = FALSE, catch = catch)

  # Set units
  units(xx@harvest) <- "f"

  # === THE CRITICAL FIX FOR THE ERROR ===
  # Use NA_real_ (numeric) instead of NA (logical)
  # This prevents the "missing value where TRUE/FALSE needed" crash
  stock.n(xx)[] <- NA_real_
  stock(xx)[]   <- NA_real_

  return(xx)
}))







# 2. CREATE THE SIMULATION INPUT OBJECT
# We create a "template" input object. Inside the loop, we will copy this 
# and only change the F-targets.
input_template <- makeMixME(om = mixedfishery_MixME_om, catch_obs = stk_oem, 
                            management_lag = 0, management_type = "fixedF", 
                            parallel = FALSE)

# Fix basic settings
input_template$oem@args$catch_timing$cod <- 0
input_template$oem@args$catch_timing$had <- 0
input_template$oem@observations$stk$cod@range[c("minfbar","maxfbar")] <- c(2,4)
input_template$oem@observations$stk$had@range[c("minfbar","maxfbar")] <- c(3,5)
input_template$args$frange$cod <- c("minfbar" = 2, "maxfbar" = 4)
input_template$args$frange$had <- c("minfbar" = 3, "maxfbar" = 5)


#Define Blim for Risk Calculation
Blim_cod <- 107000
Blim_had <- 9227

# Add HCR parameters (Blim) to the template so risk is tracked
hcrpars <- list(cod = c(Blim = Blim_cod), had = c(Blim = Blim_had))
input_template$ctrl_obj$phcr <- mseCtrl(args = list(hcrpars = hcrpars))


# 3. DEFINE THE OBJECTIVE FUNCTION
objective_func_mixme <- function(f_cod, f_had, input_obj) {
  
  # A. Update F-targets
  local_input <- input_obj
  local_input$ctrl_obj$hcr@args$ftrg$cod <- f_cod
  local_input$ctrl_obj$hcr@args$ftrg$had <- f_had
  
  # B. Run Simulation
  out <- tryCatch(
    runMixME(om = local_input$om, oem = local_input$oem, 
             ctrl_obj = local_input$ctrl_obj, args = local_input$args),
    error = function(e) NULL
  )
  if(is.null(out)) return(data.frame(F_cod=f_cod, F_had=f_had, Catch_Total=NA, Risk_Cod=NA, Risk_Had=NA))

  # C. Extract Results using YOUR Method
  # -------------------------------------------------------
  # 1. SAFETY LOOP: Fix NAs to prevents as.FLStock crash
  # -------------------------------------------------------
  # We copy the fleets so we don't mess up the original simulation object
  cleaned_flts <- out$om$flts
  
  # We iterate through every fleet and every stock component
  for(i in seq_along(cleaned_flts)) {
    for(j in seq_along(cleaned_flts[[i]])) {
      
      # We use standard accessors (landings.n, landings.wt, etc.)
      # If any data is NA, we force it to 0 so as.FLStock validates successfully
      
      # Landings Numbers
      ln <- landings.n(cleaned_flts[[i]][[j]])
      if(any(is.na(ln))) {
        ln[is.na(ln)] <- 0
        landings.n(cleaned_flts[[i]][[j]]) <- ln
      }

      # Landings Weight
      lw <- landings.wt(cleaned_flts[[i]][[j]])
      if(any(is.na(lw))) {
        lw[is.na(lw)] <- 0
        landings.wt(cleaned_flts[[i]][[j]]) <- lw
      }

      # Discards Numbers
      dn <- discards.n(cleaned_flts[[i]][[j]])
      if(any(is.na(dn))) {
        dn[is.na(dn)] <- 0
        discards.n(cleaned_flts[[i]][[j]]) <- dn
      }
      
      # Discards Weight
      dw <- discards.wt(cleaned_flts[[i]][[j]])
      if(any(is.na(dw))) {
        dw[is.na(dw)] <- 0
        discards.wt(cleaned_flts[[i]][[j]]) <- dw
      }
    }
  }

  # -------------------------------------------------------
  # 2. CONVERT: Exact code from your setup
  # -------------------------------------------------------
  # We use 'cleaned_flts' instead of 'mixedfishery_MixME_om$flts'
  stks_res <- FLStocks(lapply(out$om$stks, function(x) {
    
    # for each fleet, find which catch element corresponds to the current stock
    catch_idx <- sapply(cleaned_flts, function(y) which(names(y) %in% name(x)))
    
    # get catches from each fleet and convert to FLStock
    # This automatically calculates Total Catch = Landings + Discards
    xx <- as.FLStock(x, cleaned_flts, full = FALSE, catch = catch_idx)


    # specifies fishing mortality is measured as instantaneous fishing mortality rate
    units(xx@harvest) <- "f"
    
    return(xx)
  }))

  # D. Calculate Metrics (2030-2039)
  eval_years <- ac(2030:2039)
  
  # Total Catch (Now we can read the catch slot directly)
  c_cod <- mean(catch(stks_res$cod)[, eval_years], na.rm=TRUE)
  c_had <- mean(catch(stks_res$had)[, eval_years], na.rm=TRUE)
  
  # Risk (SSB < Blim)
  r_cod <- mean(ssb(stks_res$cod)[, eval_years] < Blim_cod, na.rm=TRUE)
  r_had <- mean(ssb(stks_res$had)[, eval_years] < Blim_had, na.rm=TRUE)

  return(data.frame(F_cod=f_cod, F_had=f_had, Catch_Total=c_cod+c_had, Risk_Cod=r_cod, Risk_Had=r_had))
}

# RE-RUN INITIALIZATION

# Define Design Space
lower_bounds <- c(F_cod = 0.0, F_had = 0.0)
upper_bounds <- c(F_cod = 1.0, F_had = 1.0)

# Select Initial Point
start_f_cod <- 0.28
start_f_had <- 0.353

print(paste("Running Initial Simulation... F_cod:", start_f_cod, "F_had:", start_f_had))

# Run the Corrected Function
run1 <- objective_func_mixme(f_cod = start_f_cod, 
                             f_had = start_f_had, 
                             input_obj = input_template)

runs <- run1

print("Initial run complete. Current database:")
print(runs)