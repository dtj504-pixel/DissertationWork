library(mlrMBO)
library(DiceKriging)
library(ParamHelpers)
library(smoof)
library(ggplot2)

################################################################################
# FINAL BATCH BAYESIAN OPTIMIZATION EXAMPLE
# Using AEI (Augmented Expected Improvement) + moimbo
# 
# This is the recommended approach for your MSE problem:
# - Handles noisy observations (AEI)
# - Proposes diverse batches (moimbo)
# - Handles constraints (penalty method)
# - Compatible with DiceKriging
################################################################################

# ==============================================================================
# STEP 1: DEFINE YOUR PROBLEM
# ==============================================================================

# OBJECTIVE FUNCTION: What you want to minimize
# In your MSE: this would return log(risk) from your simulation
toy_objective <- function(x1, x2) {
  # A non-convex function with multiple local minima
  bowl <- (x1 - 2)^2 + (x2 - 1)^2
  wiggles <- sin(3*x1) * cos(3*x2)
  return(bowl + wiggles)
}

# CONSTRAINT FUNCTION: Should be below threshold
# In your MSE: this would return catastrophe probability
toy_constraint <- function(x1, x2) {
  # Gaussian bump centered at (1, 2) - we want to AVOID this region
  0.5 * exp(-((x1-1)^2 + (x2-2)^2) / 2)
}

# COMBINED PENALIZED OBJECTIVE
# This is what the optimizer actually minimizes
penalized_objective <- function(x1, x2, 
                                constraint_threshold = 0.3, 
                                penalty_weight = 50) {
  obj_value <- toy_objective(x1, x2)
  constraint_value <- toy_constraint(x1, x2)
  
  # Apply quadratic penalty if constraint violated
  if (constraint_value > constraint_threshold) {
    violation <- constraint_value - constraint_threshold
    obj_value <- obj_value + penalty_weight * violation^2
  }
  
  return(obj_value)
}

# ==============================================================================
# STEP 2: SET UP PARAMETER SPACE
# ==============================================================================

# Define bounds for decision variables
# In your MSE: x1 = Ftarget, x2 = Btrigger (with appropriate bounds)
param_space <- makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 4),
  makeNumericParam("x2", lower = 0, upper = 4)
)

# Wrap objective for mlrMBO
objective_function <- makeSingleObjectiveFunction(
  name = "constrained_mse_toy",
  fn = function(x) penalized_objective(x$x1, x$x2),
  par.set = param_space,
  has.simple.signature = FALSE,
  noisy = TRUE  # Important: tells mlrMBO this is a noisy function
)

# ==============================================================================
# STEP 3: CREATE INITIAL DESIGN
# ==============================================================================

# Start with a space-filling design (16 points in a 4x4 grid)
# For your MSE: consider using Latin Hypercube Sampling (lhs package)
# or fewer initial points if MSE simulations are very expensive

cat("Creating initial design...\n")
initial_design <- expand.grid(
  x1 = seq(0.5, 3.5, length.out = 4),
  x2 = seq(0.5, 3.5, length.out = 4)
)

# Evaluate objective at all initial points
initial_design$y <- apply(initial_design, 1, function(row) {
  penalized_objective(row[1], row[2])
})

cat(sprintf("Initial design: %d points evaluated\n", nrow(initial_design)))
cat(sprintf("Initial best value: %.4f\n", min(initial_design$y)))

# ==============================================================================
# STEP 4: CONFIGURE BAYESIAN OPTIMIZATION
# ==============================================================================

# Create control object
control <- makeMBOControl()

# Set number of BO iterations
# Total evaluations = initial_design + (iters × propose.points)
#                   = 16 + (5 × 4) = 36 evaluations
control <- setMBOControlTermination(control, iters = 5)

# Set acquisition function: AEI (Augmented Expected Improvement)
# AEI is specifically designed for NOISY optimization
# It prevents over-exploitation of regions with artificially low uncertainty
control <- setMBOControlInfill(
  control,
  crit = makeMBOInfillCritAEI(
    aei.use.nugget = FALSE  # Estimate noise from data (more robust)
  )
)

# Configure BATCH selection using moimbo
# This ensures we get DIVERSE points in each batch, not duplicates
control <- setMBOControlMultiPoint(
  control,
  method = "moimbo",           # Multi-objective infill method
  moimbo.objective = "ei.dist", # Balance acquisition value + diversity
  moimbo.selection = "hypervolume",  # Selection criterion
  moimbo.maxit = 100           # EA generations (increase for better diversity)
)

# Set batch size
# For 2D problems, 4 points is often better than 8
# Adjust based on your parallel computing resources
control$propose.points <- 4

cat("\n=== Optimization Configuration ===\n")
cat(sprintf("Acquisition function: AEI (Augmented Expected Improvement)\n"))
cat(sprintf("Batch method: moimbo (Multi-Objective Infill MBO)\n"))
cat(sprintf("Batch size: %d points per iteration\n", control$propose.points))
cat(sprintf("Number of iterations: %d\n", control$stop.iters))
cat(sprintf("Total new evaluations: %d\n", control$stop.iters * control$propose.points))

# ==============================================================================
# STEP 5: CONFIGURE GAUSSIAN PROCESS MODEL
# ==============================================================================

# This is the "surrogate model" that approximates your expensive function
learner <- makeLearner(
  "regr.km",              # Kriging model (Gaussian Process)
  predict.type = "se",    # Predict mean AND standard error
  
  # Covariance function: Matérn 5/2
  # More flexible than exponential, less smooth than Gaussian
  covtype = "matern5_2",
  
  # CRITICAL SETTING: Estimate nugget from data
  # This prevents the zero-uncertainty problem
  # The nugget represents observation noise
  nugget.estim = TRUE,
  
  # Optimization method for hyperparameters
  optim.method = "BFGS",
  
  # Suppress optimization output
  control = list(trace = FALSE)
)

cat(sprintf("\nGP Model: Matérn 5/2 covariance with estimated nugget\n"))

# ==============================================================================
# STEP 6: RUN BAYESIAN OPTIMIZATION
# ==============================================================================

set.seed(42)  # For reproducibility
cat("\n=== Running Bayesian Optimization ===\n")

# Run the optimization
result <- mbo(
  fun = objective_function,
  design = initial_design,
  learner = learner,
  control = control,
  show.info = TRUE
)

# ==============================================================================
# STEP 7: ANALYZE RESULTS
# ==============================================================================

opt_path <- as.data.frame(result$opt.path)

cat("\n=== OPTIMIZATION RESULTS ===\n")
cat(sprintf("Best penalized objective value: %.4f\n", result$y))
cat(sprintf("Best parameters found:\n"))
cat(sprintf("  x1 = %.4f\n", result$x$x1))
cat(sprintf("  x2 = %.4f\n", result$x$x2))

# Check constraint satisfaction at best point
best_constraint_value <- toy_constraint(result$x$x1, result$x$x2)
best_true_objective <- toy_objective(result$x$x1, result$x$x2)

cat(sprintf("\nConstraint analysis at best point:\n"))
cat(sprintf("  Constraint value: %.4f (threshold: 0.30)\n", best_constraint_value))
cat(sprintf("  Constraint satisfied: %s\n", 
            ifelse(best_constraint_value < 0.3, "YES ✓", "NO ✗")))
cat(sprintf("  True objective (unpenalized): %.4f\n", best_true_objective))

# ==============================================================================
# STEP 8: EVALUATE BATCH QUALITY
# ==============================================================================

cat("\n=== BATCH DIVERSITY ANALYSIS ===\n")
cat("(Larger distances = better diversity, avoiding duplicate points)\n\n")

for (iter in 1:max(opt_path$dob)) {
  batch <- opt_path[opt_path$dob == iter, ]
  
  if (nrow(batch) > 1) {
    coords <- as.matrix(batch[, c("x1", "x2")])
    dists <- as.matrix(dist(coords))
    
    # Get all pairwise distances (excluding diagonal)
    pairwise_dists <- dists[lower.tri(dists)]
    
    cat(sprintf("Iteration %d (%d points):\n", iter, nrow(batch)))
    cat(sprintf("  Min distance:  %.3f %s\n", 
                min(pairwise_dists),
                ifelse(min(pairwise_dists) > 0.1, "✓", "⚠ (too close!)")))
    cat(sprintf("  Mean distance: %.3f\n", mean(pairwise_dists)))
    cat(sprintf("  Max distance:  %.3f\n", max(pairwise_dists)))
  }
}

# ==============================================================================
# STEP 9: VISUALIZE OPTIMIZATION PROGRESS
# ==============================================================================

cat("\n=== Creating visualizations ===\n")

# Plot 1: Objective value over time
p1 <- ggplot(opt_path, aes(x = 1:nrow(opt_path), y = y)) +
  geom_line(color = "gray50") +
  geom_point(aes(color = factor(dob)), size = 3, alpha = 0.8) +
  geom_hline(yintercept = min(opt_path$y), linetype = "dashed", color = "red") +
  labs(
    x = "Evaluation Number",
    y = "Penalized Objective Value",
    title = "Optimization Progress: Objective Value Over Time",
    subtitle = sprintf("Best value: %.3f", min(opt_path$y)),
    color = "Iteration"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p1)

# Plot 2: Parameter space exploration
p2 <- ggplot(opt_path, aes(x = x1, y = x2)) +
  geom_point(aes(color = y, size = -dob), alpha = 0.7) +
  geom_point(data = opt_path[which.min(opt_path$y), ],
             aes(x = x1, y = x2), 
             color = "red", size = 5, shape = 17) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_size_continuous(range = c(2, 5)) +
  labs(
    x = "x1 (e.g., Ftarget)",
    y = "x2 (e.g., Btrigger)",
    color = "Objective\nValue",
    size = "Iteration\n(larger = earlier)",
    title = "Parameter Space Exploration",
    subtitle = "Red triangle = best point found"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p2)

# Plot 3: Batch structure visualization
p3 <- ggplot(opt_path[opt_path$dob > 0, ], aes(x = x1, y = x2)) +
  geom_point(aes(color = factor(dob)), size = 4, alpha = 0.8) +
  geom_path(aes(group = dob), alpha = 0.3) +
  facet_wrap(~dob, ncol = 3, labeller = label_both) +
  labs(
    x = "x1",
    y = "x2",
    color = "Iteration",
    title = "Batch Composition: Points Selected in Each Iteration",
    subtitle = "Each panel shows one batch of proposed points"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p3)

# ==============================================================================
# STEP 10: SUMMARY FOR ADAPTATION TO YOUR MSE PROBLEM
# ==============================================================================

cat("\n=== ADAPTATION GUIDE FOR YOUR MSE PROBLEM ===\n")
cat("\n1. Replace toy functions with your MSE simulation:\n")
cat("   - toy_objective() → returns log(risk) from MSE\n")
cat("   - toy_constraint() → returns catastrophe probability\n")
cat("   - Both should call your actual MSE code\n")

cat("\n2. Update parameter space:\n")
cat("   - x1 → Ftarget with appropriate bounds\n")
cat("   - x2 → Btrigger with appropriate bounds\n")

cat("\n3. Adjust computational settings:\n")
cat("   - Reduce batch size if MSE is very expensive\n")
cat("   - Use Latin Hypercube for initial design\n")
cat("   - Consider fewer iterations but larger batches\n")

cat("\n4. Tune penalty weight:\n")
cat("   - Current: 50 (arbitrary)\n")
cat("   - Increase if constraints are frequently violated\n")
cat("   - Decrease if optimizer is too conservative\n")

cat("\n5. Save intermediate results:\n")
cat("   - MSE simulations are expensive!\n")
cat("   - Save opt_path after each iteration\n")
cat("   - Consider checkpointing in case of crashes\n")

cat("\n=== DONE ===\n")

# ==============================================================================
# STEP 11: COMPARE WITH TRUE OPTIMUM
# ==============================================================================

# For this toy problem, we can find the true optimum by grid search
# In your MSE problem, you won't know the true optimum (that's why we're optimizing!)
cat("\n=== COMPARISON WITH TRUE OPTIMUM ===\n")
cat("(For toy problem only - you won't have this for real MSE)\n\n")

# Find true optimum via fine grid search
cat("Computing true optimum via grid search...\n")
grid_size <- 100
x1_grid <- seq(0, 4, length.out = grid_size)
x2_grid <- seq(0, 4, length.out = grid_size)
grid_search <- expand.grid(x1 = x1_grid, x2 = x2_grid)

# Evaluate on entire grid
grid_search$y <- apply(grid_search, 1, function(row) {
  penalized_objective(row[1], row[2])
})

# Find minimum
true_optimum_idx <- which.min(grid_search$y)
true_optimum <- grid_search[true_optimum_idx, ]

cat("\n--- TRUE OPTIMUM (via grid search) ---\n")
cat(sprintf("x1 = %.4f\n", true_optimum$x1))
cat(sprintf("x2 = %.4f\n", true_optimum$x2))
cat(sprintf("Objective value = %.4f\n", true_optimum$y))

# Check if true optimum satisfies constraint
true_opt_constraint <- toy_constraint(true_optimum$x1, true_optimum$x2)
cat(sprintf("Constraint value = %.4f (threshold: 0.30)\n", true_opt_constraint))
cat(sprintf("Constraint satisfied: %s\n", 
            ifelse(true_opt_constraint < 0.3, "YES ✓", "NO ✗")))

cat("\n--- BEST POINT FOUND BY BO ---\n")
cat(sprintf("x1 = %.4f\n", result$x$x1))
cat(sprintf("x2 = %.4f\n", result$x$x2))
cat(sprintf("Objective value = %.4f\n", result$y))
cat(sprintf("Constraint value = %.4f\n", best_constraint_value))
cat(sprintf("Constraint satisfied: %s\n", 
            ifelse(best_constraint_value < 0.3, "YES ✓", "NO ✗")))

cat("\n--- PERFORMANCE METRICS ---\n")

# Euclidean distance in parameter space
param_distance <- sqrt((result$x$x1 - true_optimum$x1)^2 + 
                       (result$x$x2 - true_optimum$x2)^2)
cat(sprintf("Distance to true optimum: %.4f\n", param_distance))

# Objective value gap
obj_gap <- result$y - true_optimum$y
obj_gap_percent <- (obj_gap / abs(true_optimum$y)) * 100
cat(sprintf("Objective value gap: %.4f (%.2f%%)\n", obj_gap, obj_gap_percent))

# Simple regret: how much worse is our solution than the best possible?
cat(sprintf("Simple regret: %.4f\n", obj_gap))

# Performance assessment
cat("\n--- ASSESSMENT ---\n")
if (param_distance < 0.1) {
  cat("✓ EXCELLENT: Found solution very close to true optimum\n")
} else if (param_distance < 0.3) {
  cat("✓ GOOD: Found solution reasonably close to true optimum\n")
} else if (param_distance < 0.5) {
  cat("⚠ FAIR: Found solution in neighborhood of true optimum\n")
} else {
  cat("✗ POOR: Solution far from true optimum - may need more iterations\n")
}

if (obj_gap_percent < 1) {
  cat("✓ Objective value within 1% of optimum\n")
} else if (obj_gap_percent < 5) {
  cat("⚠ Objective value within 5% of optimum\n")
} else {
  cat("✗ Objective value gap > 5% - consider more iterations\n")
}

# Both satisfy or violate constraint?
both_feasible <- (best_constraint_value < 0.3) && (true_opt_constraint < 0.3)
both_infeasible <- (best_constraint_value >= 0.3) && (true_opt_constraint >= 0.3)
if (both_feasible) {
  cat("✓ Both solutions are feasible (satisfy constraint)\n")
} else if (both_infeasible) {
  cat("⚠ Both solutions violate constraint - problem may be infeasible\n")
} else if (best_constraint_value < 0.3) {
  cat("✓ BO solution is feasible (true optimum is not)\n")
} else {
  cat("✗ BO solution violates constraint (should increase penalty weight)\n")
}

cat("\n=== OPTIMIZATION COMPLETE ===\n")
cat(sprintf("Total function evaluations: %d\n", nrow(opt_path)))
cat(sprintf("Best objective value found: %.4f\n", result$y))
cat(sprintf("True optimum value: %.4f\n", true_optimum$y))
cat(sprintf("Performance: %.2f%% of optimal\n", 100 - obj_gap_percent))