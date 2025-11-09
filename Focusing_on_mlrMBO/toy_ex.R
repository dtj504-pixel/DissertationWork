# Currently deterministic and so always giving the same answer, which turns out to be a not very good answer. 

library(mlrMBO)
library(DiceKriging)
library(ParamHelpers)
library(smoof)

# Define the objective function (toy black‑box)
objective_fun <- function(x) {
  (x - 2)^2 + sin(5 * x)
}

# Parameter space
ps <- makeParamSet(
  makeNumericParam("x", lower = 0, upper = 4)
)

# Wrap objective in smoof function
obj <- makeSingleObjectiveFunction(
  name = "toy_batch_example",
  fn = function(x) objective_fun(x),
  par.set = ps,
  has.simple.signature = TRUE
)

# Initial design
design <- data.frame(x = seq(0, 4, length.out = 8))
design$y <- apply(design, 1, function(row) objective_fun(row[1]))

# Configure MBO control for batch size = 8
control <- makeMBOControl()
control <- setMBOControlTermination(control, iters = 7)        # 5 rounds of batches
control <- setMBOControlInfill(control, crit = makeMBOInfillCritCB(cb.lambda = 3))
control <- setMBOControlMultiPoint(control, method = "cb")     # using confidence bound strategy
control$propose.points <- 8                                   # batch size = 8

# Fit initial surrogate model
model <- km(
  formula = ~1,
  design = data.frame(x = design$x),
  response = design$y,
  covtype = "matern5_2",
  nugget = 1e-6
)

# Wrap learner for mlrMBO
learner <- makeLearner("regr.km", predict.type = "se",
                       covtype = model@covariance@name,
                       nugget = model@covariance@nugget,
                       nugget.estim = FALSE)

# Run MBO
result <- mbo(
  fun = obj,
  design = design,
  learner = learner,
  control = control,
  show.info = TRUE
)

# Convert optimization path to data frame
opt.path <- as.data.frame(result$opt.path)

# Trace each iteration’s proposed points
# Trace each iteration’s proposed points with updated surrogate
cat("=== Batch Bayesian Optimization Trace (8 points per iteration) ===\n")

for (iter in 1:max(opt.path$dob)) {
  batch_points <- opt.path[opt.path$dob == iter, ]
  if (nrow(batch_points) == 0) next
  
  # Refit surrogate with all points evaluated so far (up to this iteration)
  current_data <- opt.path[opt.path$dob <= iter, c("x","y")]
  model_iter <- km(
    formula = ~1,
    design = data.frame(x = current_data$x),
    response = current_data$y,
    covtype = "matern5_2",
    nugget = 1e-6
  )
  
  cat(sprintf("\n--- Iteration %d (batch size: %d) ---\n",
              iter, nrow(batch_points)))
  for (i in seq_len(nrow(batch_points))) {
    x_val <- batch_points$x[i]
    y_val <- batch_points$y[i]
    pred <- predict(model_iter, newdata = data.frame(x = x_val), type = "UK")
    cat(sprintf("Point %d: x = %.4f | observed y = %.4f | predicted mean = %.4f | predicted sd = %.4f\n",
                i, x_val, y_val, pred$mean, pred$sd))
  }
}

# Final summary
cat("\n=== Summary ===\n")
best_idx <- which.min(opt.path$y)
best_point <- opt.path[best_idx, ]
cat(sprintf("Best observed point so far: x = %.4f, y = %.4f\n", best_point$x, best_point$y))
