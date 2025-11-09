library(mlrMBO)
library(DiceKriging)
library(ParamHelpers)
library(smoof)

# ============================================================================
# SIMPLE TOY EXAMPLE: Constrained Optimization with Batch Evaluation
# ============================================================================

# Simple 2D objective: minimize this function
toy_objective <- function(x1, x2) {
  (x1 - 2)^2 + (x2 - 1)^2 + sin(3*x1) * cos(3*x2)
}

# Constraint: this should be < 0.3 (mimics catastrophe probability)
toy_constraint <- function(x1, x2) {
  0.5 * exp(-((x1-1)^2 + (x2-2)^2) / 2)
}

# Combined penalized objective
penalized_toy <- function(x1, x2, constraint_threshold = 0.3, penalty = 50) {
  obj_value <- toy_objective(x1, x2)
  constraint_value <- toy_constraint(x1, x2)
  
  # If constraint violated, add penalty
  if (constraint_value > constraint_threshold) {
    violation <- constraint_value - constraint_threshold
    obj_value <- obj_value + penalty * violation^2
  }
  
  return(obj_value)
}

# ============================================================================
# SETUP
# ============================================================================

# Define parameter space
ps <- makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 4),
  makeNumericParam("x2", lower = 0, upper = 4)
)

# Wrap for mlrMBO
obj <- makeSingleObjectiveFunction(
  name = "toy_constrained",
  fn = function(x) penalized_toy(x$x1, x$x2),
  par.set = ps,
  has.simple.signature = FALSE
)

# Initial design: simple grid
initial_design <- expand.grid(
  x1 = seq(0.5, 3.5, length.out = 4),
  x2 = seq(0.5, 3.5, length.out = 4)
)
initial_design$y <- apply(initial_design, 1, function(row) {
  penalized_toy(row[1], row[2])
})

cat(sprintf("Initial design: %d points\n", nrow(initial_design)))

# ============================================================================
# CONFIGURE MBO WITH BATCH EVALUATION
# ============================================================================

control <- makeMBOControl()
control <- setMBOControlTermination(control, iters = 5)  # Just 5 iterations

# Use AEI acquisition function (handles noise well)
control <- setMBOControlInfill(
  control, 
  crit = makeMBOInfillCritAEI(aei.use.nugget = FALSE)
)

# Configure batch selection with moimbo
control <- setMBOControlMultiPoint(
  control, 
  method = "moimbo",
  moimbo.objective = "ei.dist",  # Balance EI and diversity
  moimbo.maxit = 50  # Fewer generations for speed
)

control$propose.points <- 4  # Propose 4 points per iteration

# ============================================================================
# LEARNER CONFIGURATION
# ============================================================================

learner <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern5_2",
  nugget.estim = TRUE  # KEY: Estimate noise from data
)

# ============================================================================
# RUN OPTIMIZATION
# ============================================================================

set.seed(123)
cat("\nRunning Bayesian Optimization...\n")

result <- mbo(
  fun = obj,
  design = initial_design,
  learner = learner,
  control = control,
  show.info = TRUE
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