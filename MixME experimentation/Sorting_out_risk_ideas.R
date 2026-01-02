# // In Exploring _simualtion_outputs.R they start with data tha has 100 iterations and
# // so perhaps I need to tets the risk calculations out of this kind fo data
# // and od a basic version first and then integrate it with my model






# 1. SET THE NUMBER OF ITERATIONS
n_iter <- 50  # Start with 50 to keep it fast for testing (100 is standard)

# 2. EXPAND THE OEM OBJECTS
# This copies the data 'n_iter' times
stk_oem$cod <- propagate(stk_oem$cod, n_iter)
stk_oem$had <- propagate(stk_oem$had, n_iter)

# 3. EXPAND THE OPERATING MODEL (OM) OBJECTS
# We need to be careful here because 'mixedfishery_MixME_om$cod' gave NULL dims.
# It is likely a list or a special FLFishery object. 
# Try propagating the internal FLStocks if they exist:

if(inherits(mixedfishery_MixME_om$cod, "FLStock") || inherits(mixedfishery_MixME_om$cod, "FLBiol")) {
    mixedfishery_MixME_om$cod <- propagate(mixedfishery_MixME_om$cod, n_iter)
    mixedfishery_MixME_om$had <- propagate(mixedfishery_MixME_om$had, n_iter)
} else {
    # If it's a simple list, we might need to propagate the specific slots manually.
    # Check if 'stk' exists inside it (common in FLR lists)
    if(!is.null(mixedfishery_MixME_om$cod$stk)) {
        mixedfishery_MixME_om$cod$stk <- propagate(mixedfishery_MixME_om$cod$stk, n_iter)
        mixedfishery_MixME_om$had$stk <- propagate(mixedfishery_MixME_om$had$stk, n_iter)
    }
}

# 4. CRITICAL: ADD RANDOMNESS (Recruitment Residuals)
# If we don't do this, all 50 iterations will be identical!

# Generate random recruitment deviations (mean 1, log-normal errors)
# Adjust 'sd = 0.3' to match the actual variability of your stock (0.3 is typical for North Sea)
rec_noise_cod <- rlnorm(n_iter, meanlog = 0, sdlog = 0.3)
rec_noise_had <- rlnorm(n_iter, meanlog = 0, sdlog = 0.3)

# Apply this noise to the Stock-Recruitment residuals of the OM
# (Assuming your OM object has an 'sr' slot or similar for recruitment)

# Note: Depending on how your 'mixedfishery' object is built, 
# you might need to apply this to the internal stock object.
# Example for a standard FLStock:
# stock(mixedfishery_MixME_om$cod) <- propagate(stock(mixedfishery_MixME_om$cod), n_iter)