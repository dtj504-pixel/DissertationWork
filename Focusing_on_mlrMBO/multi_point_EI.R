library(mlrMBO)
library(DiceKriging)
library(ParamHelpers)
library(smoof)

# ============================================================================
# SIMPLE TOY EXAMPLE: Constrained Optimization with Batch Evaluation
# ============================================================================

# OBJECTIVE FUNCTION: What we want to minimize
# In your MSE problem, this would be log(risk)
# Here: a bowl-shaped function with some wiggles
toy_objective <- function(x1, x2) {
  # Bowl shape centered at (2, 1)
  bowl <- (x1 - 2)^2 + (x2 - 1)^2
  # Add wiggles to make it interesting (non-convex)
  wiggles <- sin(3*x1) * cos(3*x2)
  return(bowl + wiggles)
}

# CONSTRAINT FUNCTION: Should be < threshold
# In your MSE problem, this would be catastrophe probability
# Here: a Gaussian bump centered at (1, 2) - we want to AVOID this region
toy_constraint <- function(x1, x2) {
  # Returns high value near (1,2), low value far away
  0.5 * exp(-((x1-1)^2 + (x2-2)^2) / 2)
}

# PENALIZED OBJECTIVE: Combines objective + constraint via penalty method
# This is the function mlrMBO will actually optimize
penalized_toy <- function(x1, x2, constraint_threshold = 0.3, penalty = 50) {
  # First, evaluate the true objective
  obj_value <- toy_objective(x1, x2)
  
  # Then, check the constraint
  constraint_value <- toy_constraint(x1, x2)
  
  # If constraint is VIOLATED (value too high), add a penalty
  # This makes the optimizer avoid constraint-violating regions
  if (constraint_value > constraint_threshold) {
    violation <- constraint_value - constraint_threshold  # How much over threshold
    obj_value <- obj_value + penalty * violation^2  # Quadratic penalty
  }
  
  # Return penalized objective value
  return(obj_value)
}

# ============================================================================
# SETUP
# ============================================================================

# PARAMETER SPACE: Define the bounds for our decision variables
# In your MSE: x1 = Ftarget, x2 = Btrigger
ps <- makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 4),  # x1 can be anywhere in [0, 4]
  makeNumericParam("x2", lower = 0, upper = 4)   # x2 can be anywhere in [0, 4]
)

# WRAP FOR mlrMBO: Convert to smoof function (mlrMBO's expected format)
obj <- makeSingleObjectiveFunction(
  name = "toy_constrained",
  fn = function(x) penalized_toy(x$x1, x$x2),  # x is a list with x1 and x2
  par.set = ps,
  has.simple.signature = FALSE  # FALSE because we use named parameters
)

# INITIAL DESIGN: Where to start the optimization
# These are the FIRST points we evaluate (before BO starts)
# Here: 4x4 grid = 16 points evenly spaced across the space
initial_design <- expand.grid(
  x1 = seq(0.5, 3.5, length.out = 4),  # 4 values: 0.5, 1.5, 2.5, 3.5
  x2 = seq(0.5, 3.5, length.out = 4)   # 4 values: 0.5, 1.5, 2.5, 3.5
)

# Evaluate the objective at all initial design points
initial_design$y <- apply(initial_design, 1, function(row) {
  penalized_toy(row[1], row[2])
})

cat(sprintf("Initial design: %d points\n", nrow(initial_design)))

# ============================================================================
# CONFIGURE MBO WITH BATCH EVALUATION
# ============================================================================

# CREATE CONTROL OBJECT: This stores all optimization settings
control <- makeMBOControl()

# TERMINATION: How many iterations of BO to run
# Each iteration proposes a BATCH of new points
control <- setMBOControlTermination(control, iters = 5)  # 5 batches

# ACQUISITION FUNCTION: How to decide which points are "promising"
# AEI = Augmented Expected Improvement
# - Regular EI measures: "how much improvement can we expect?"
# - AEI adds: "but account for observation noise"
# - This prevents over-exploiting noisy regions
control <- setMBOControlInfill(
  control, 
  crit = makeMBOInfillCritAEI(
    aei.use.nugget = FALSE  # FALSE = estimate noise from data (more robust)
                             # TRUE = use kriging nugget parameter
  )
)

# BATCH SELECTION: How to choose MULTIPLE points per iteration
# Problem: If we just maximize AEI 4 times, we might get 4 identical points!
# Solution: Use "moimbo" = Multi-Objective Infill MBO
control <- setMBOControlMultiPoint(
  control, 
  method = "moimbo",  # Use multi-objective evolutionary algorithm
  
  # Objectives to optimize simultaneously:
  # - "ei" = Maximize Expected Improvement (exploitation)
  # - "dist" = Maximize distance from existing points (exploration/diversity)
  moimbo.objective = "ei.dist",
  
  # Run evolutionary algorithm for 50 generations
  moimbo.maxit = 50
)

# BATCH SIZE: How many points to propose per iteration
# Total evaluations = initial_design + (iters × propose.points)
#                   = 16 + (5 × 4) = 36 points
control$propose.points <- 4

# ============================================================================
# LEARNER CONFIGURATION
# ============================================================================

# GAUSSIAN PROCESS (GP) MODEL: The "surrogate model"
# This approximates our expensive objective function
# After evaluating N points, we fit a GP to predict function values elsewhere

learner <- makeLearner(
  "regr.km",  # Kriging model (= Gaussian Process)
  
  # Predict both mean AND standard error (uncertainty)
  predict.type = "se",
  
  # COVARIANCE FUNCTION: How points are correlated
  # "matern5_2" = Matérn 5/2 kernel (smooth but not infinitely differentiable)
  # More flexible than "exp" (exponential) or "gauss" (Gaussian)
  covtype = "matern5_2",
  
  # NUGGET: Observation noise parameter
  # nugget.estim = TRUE means:
  # - Fit GP assuming observations are noisy: y = f(x) + ε, where ε ~ N(0, nugget)
  # - Estimate nugget from the data via maximum likelihood
  # 
  # This is CRITICAL for preventing zero-uncertainty problem!
  # If nugget.estim = FALSE and nugget = 1e-12:
  #   → GP interpolates data almost perfectly
  #   → Predicts zero uncertainty at/near observed points
  #   → All acquisition functions collapse to same point
  nugget.estim = TRUE  # KEY SETTING!
)

# ============================================================================
# RUN OPTIMIZATION
# ============================================================================

set.seed(123)  # For reproducibility
cat("\nRunning Bayesian Optimization...\n")

# THE MAIN OPTIMIZATION LOOP
# What happens inside mbo():
# 
# FOR each iteration (1 to 5):
#   1. Fit GP model to all data seen so far
#   2. Use acquisition function (AEI) to score every possible point
#   3. Use moimbo to select 4 diverse high-AEI points
#   4. Evaluate objective at those 4 points
#   5. Add results to dataset
# END FOR
# 
# Return: Best point found

result <- mbo(
  fun = obj,              # The function to optimize
  design = initial_design, # Starting data (16 points)
  learner = learner,       # GP model specification
  control = control,       # All the settings we configured above
  show.info = TRUE         # Print progress during optimization
)

# ============================================================================
# ANALYZE RESULTS
# ============================================================================

opt_path <- as.data.frame(result$opt.path)

cat("\n=== Results ===\n")
cat(sprintf("Best value: %.4f\n", result$y))
cat(sprintf("Best parameters: x1=%.3f, x2=%.3f\n", 
            result$x$x1, result$x$x2))

# Check if best point satisfies constraint
best_constraint <- toy_constraint(result$x$x1, result$x$x2)
cat(sprintf("Constraint value at best: %.4f (threshold: 0.3)\n", best_constraint))
cat(sprintf("Constraint satisfied: %s\n", 
            ifelse(best_constraint < 0.3, "YES", "NO")))

# Show batch diversity
cat("\n=== Batch Diversity ===\n")
for (iter in 1:max(opt_path$dob)) {
  batch <- opt_path[opt_path$dob == iter, ]
  if (nrow(batch) > 1) {
    coords <- as.matrix(batch[, c("x1", "x2")])
    dists <- dist(coords)
    cat(sprintf("Iter %d: %d points, min_dist=%.3f, mean_dist=%.3f\n",
                iter, nrow(batch), min(dists), mean(dists)))
  }
}

# Show each iteration's points
cat("\n=== Points Evaluated Each Iteration ===\n")
for (iter in 1:max(opt_path$dob)) {
  batch <- opt_path[opt_path$dob == iter, c("x1", "x2", "y")]
  if (nrow(batch) > 0) {
    cat(sprintf("\n--- Iteration %d ---\n", iter))
    print(round(batch, 3))
  }
}

# ============================================================================
# SIMPLE VISUALIZATION
# ============================================================================

library(ggplot2)

# Plot all evaluated points
p <- ggplot(opt_path, aes(x = x1, y = x2, color = y, shape = factor(dob))) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  labs(x = "x1", y = "x2", 
       color = "Objective\nValue",
       shape = "Iteration",
       title = "Batch BO: Parameter Space Exploration") +
  theme_minimal() +
  theme(legend.position = "right")

print(p)

# ============================================================================
# KEY TAKEAWAYS
# ============================================================================

cat("\n=== Key Points ===\n")
cat("1. AEI acquisition function handles noise and uncertainty\n")
cat("2. moimbo method ensures batch diversity (see distances above)\n")
cat("3. nugget.estim=TRUE prevents zero-uncertainty problem\n")
cat("4. Penalty method handles constraints simply\n")
cat("5. Each iteration proposes 4 diverse points\n")