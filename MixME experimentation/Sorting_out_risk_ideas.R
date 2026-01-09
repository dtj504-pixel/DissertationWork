# // In Exploring_simualtion_outputs.R they start with data tha has 100 iterations and
# // so perhaps I need to test the risk calculations out of this kind of data
# // and do a basic version first and then integrate it with my model

# Focusing on inspecting the risk object from running the model in Optimising_ftarget_in_MixMe_model.R once
# (where we only have one iteration and so only have one entry for risk in each year for each stock)
# and thus trying to decide how to calculate risk in this situation


## load libraries
library(FLCore)
library(FLFishery)
library(mse)
library(stockassessment)
library(MixME)
library(DiceKriging)

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



# Set an F target for both stocks 
f_cod <- 0.28
f_had <- 0.353


# Run the model
res <- obj_func(f_cod,f_had,mixedfishery_MixME_om,stk_oem)

# There does not seem to be an industrial standard for calculating risk over one iteration, as it is a probability

# Found support for penalty approach in https://watermark02.silverchair.com/fsm085.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA1YwggNSBgkqhkiG9w0BBwagggNDMIIDPwIBADCCAzgGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMFZjiZVKP5a-uQJfAAgEQgIIDCbzZ-UOo6fvBPJ9VVqP4n8yFBUaAC8XKvZ07odtz90KTvqskMPA-xDUZaWrVuwurcFMCwK9tOfUtXkU88_ENNwiVT4ransobSR0haIGLVxzmZEAfzxqZ0AsIcDZFwVwSXK_J1DfK6LZULTIqJhKfNWAKukM8S5YpcLi8ax8D04Y430e1Jn7Fs7z1I9nzFYca16VkglWOZM3G_t5QJk7j6OxWpgag3FKo_76dI8VETlNkr6ntJyIWAEox4L1c5iOZnOZAZ8rG_Vi2M7d6edhxwG8ZoNdGWlt3D81j4hOmxw0KuFiKM5Kv07X0xhANLJauO9QGtEaEbJ-DL7B_8tgNT3EJwMQrn5zmXGjl7jOEe7rvBHQU3r0B7z46WMfBOjZAnkVpIdaktldtwofxuNCUc-7Mz4BCUVydmiyVaa-__Ib3bSKffi7344j5MdByfS-jipw4mTP4AkVFVFQ4JqsgfiTVbNV2-KLVKD-oTNeE9FEDHKMBftUIsAEHUPl57RijFxCHgJtvurRV3pg_l7EyZ1ENB_iT6NCGyg_EJXBXyrBhgPQLYKOznZDqDNxJkUZb9-c49BnIwxNX-yLDKN2e9anfFMMh26wAuV5Cp2iOCWSbqJe0b0xv6EtuJQc1XR7mpLCkEI4HiGniOGj6If33f3QtfAg27YTEGlpFyMx13shNTlbRw6H9uOaVJyBAU8clc_9Na0IQuKXPWrYZ_7mEuZ_2zAwpXQkud4QEdVWrf3oQUbOogAakLRTa-enrJZ_VdBg9anxoACGDpqBM8wDExDzvP8U9QNjKrH0fBU4BCepP3GL7HpXWtiUIE8_KC5oOWQeN2zfz9UsLGdkUwh9bI-LoSYFZuyNQv3E6mj-JHGbezE6bdilxUY0PHFvDVxsswdetvXPzh-duFjRLXiJaycSAPkLyUA-2-BNcfc4ysDiJNZVUnnX-UWzpr-YEnduI6tbiJc2_9EyLHa6MhkTmIPJ1D6SOfnJhnk314Q2JYhiXTNfdurZCSFiB3yTri1oZLfhdHuRC5knT_Q 
# We will need to change this for the shortcut version later on anyway, but I want to be able to check that
# the GPs are optimising well first - TODO: I will need a correct answer from somewhere for this

# TODO: Will then need to change to only calculate kg for possible points as equivalent threshold not obvious?


## // Calculate the risk for both stocks in each year and at end of the projection//
# TODO: Improve to a probability somehow, multiple iterations if needed
# CONCLUSION: Need multiple iterations to generate a probabilistic risk as this data ("mixedfishery_MixME_om") is deterministic

  # Define Blims
  Blim_cod <- 107000
  Blim_had <- 9227

  # Extract SSB directly - still focusing on the end fo the simualtion in 2039
  ssb_cod_data <- c(res$tracking$cod$stk["SB.om", ac(2039)])
  ssb_had_data <- c(res$tracking$had$stk["SB.om", ac(2039)])

  # Calculate risk for cod
  if (is.na(ssb_cod_data)) {
      # If it crashed, we treat SSB as effectively 0 so the penalty is the log of the full distance (Blim - 0)
      risk_cod <- -log(Blim_cod) 
  } else if(ssb_cod_data > Blim_cod) {
      # Safe zone: No penalty
      risk_cod <- 0
  } else {
      # Danger zone: The "risk" is how deep we are in the hole. We log it to make the GPs more stable
      risk_cod <- -log(Blim_cod - ssb_cod_data) 
  }

  # Calculate risk for haddock
  if (is.na(ssb_had_data)) {
      # If it crashed, we treat SSB as effectively 0 so the penalty is the log of the full distance (Blim - 0)
      risk_had <- -log(Blim_had) 
  } else if (ssb_had_data > Blim_had) {
      # Safe zone: No penalty
      risk_had <- 0
  } else {
      # Danger zone: The "risk" is how deep we are in the hole. We log it to make the GPs more stable
      risk_had <- -log(Blim_had - ssb_had_data) 
  }


## // NEW IDEA //

# Remove logs but divide by Blim to stabilise GPs

# Calculate Safety Score (Higher is better, 0 is the limit)
# We normalize by Blim so Cod and Haddock are on the same scale (-1 to +1)
risk_cod <- (ssb_cod_data - Blim_cod) / Blim_cod
risk_had <- (ssb_had_data - Blim_had) / Blim_had