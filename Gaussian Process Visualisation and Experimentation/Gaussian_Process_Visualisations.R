# GP VISUALISATION EXPERIMENTS use DiceView

# 2D Visualisation whilst keeping Btrigger fixed
# Can be easily adjusted to fix Ftarget instead if desired
# Can also be easily adjusted to do this for the catch GP instead

# load libraries
library(mvtnorm)

# Fix Btrigger at its median value but also one that is in the gridd
unique_B <- sort(unique(gridd$Btrigger))
B_0 <- unique_B[which.min(abs(unique_B - median(unique_B)))]

# Take a slice from scaled gridd such that Btrigger = B0
slice_idx <- gridd$Btrigger == B_0
# Make this slice our new grid for prediction
Xnew <- gridd[slice_idx, c("Ftarget","Btrigger")]


# Use GP to predict functions
pred_risk_fixed <- predict(
  gp_risk,
  newdata = Xnew,
  type = "UK",
  cov.compute = TRUE
)

# mean and variance in our new grid
mu <- pred_risk_fixed$mean        
Sigma <- pred_risk_fixed$cov

# get 10 sample functions
nsim <- 10
fsim <- rmvnorm(nsim, mean = mu, sigma = Sigma)

#Get initial plot with axes and mean line
plot(
  Xnew$Ftarget, mu,
  type = "l",
  lwd = 3,
  col = "black",
  xlab = "Ftrgt",
  ylab = "log-risk",
  main = "Posterior GP samples (Btrigger fixed)"
)

# Add on sampled functions with slight transparency
for (i in 1:nsim) {
  lines(Xnew$Ftarget, fsim[i, ], col = rgb(0, 0, 1, 0.3))
}


# 3D view of Risk GP mean and sd plot

#load appropriate libraries
library(DiceView)
library(rgl)

#setting limits of the plot based on the points we have already evaluated
#here this is ok because we set up a design that covers the space well
#and this is applied in our four of the files we are focusing on
Xlim <- rbind(
  apply(gp_risk@X, 2, min),
  apply(gp_risk@X, 2, max)
)


sectionview3d(
  #tells sectionview3d which GP to plot - here it is the risk GP
  gp_risk,
  #tells sectionview3d we have 2 input dimensions
  dim = 2,
  #sets the limits of the polot along these two input dimensions
  Xlim = Xlim,
  #labelling the axes
  Xname = c("Ftrgt", "Btrigger"),
  yname = "GP mean (log-risk)",
  #we are predicitng the GP at 100x100 points to get a smooth surface, even though our space is discrete
  npoints = c(100, 100),
  #set the colour
  col = "lightblue",
  #scale teh axis proportionally to gte good visualisation
  scale = TRUE,
  #colour points we have already evaluated
  col_points = "red",      
  #making sure we create this plot in its own window
  add = FALSE
)

#gives each axis this same scale
rgl::aspect3d("iso")
#sets default camera view
rgl::view3d(theta = 40, phi = 25, zoom = 0.8)

#converting to a html widget so we can render it through a Quarto document
rglwidget()


# 3D view of Catch GP mean and sd plot
# repeat of the above, except with the GP changed to the catch GP
library(DiceView)
library(rgl)

Xlim <- rbind(
  apply(gp_cat@X, 2, min),
  apply(gp_cat@X, 2, max)
)


sectionview3d(
  gp_cat,
  dim = 2,
  Xlim = Xlim,
  Xname = c("Ftrgt", "Btrigger"),
  yname = "GP mean (log-risk)",
  npoints = c(100, 100),
  col = "lightblue",
  scale = TRUE,
  col_points = "red",      # color for design points
  add = FALSE
)

rgl::aspect3d("iso")
rgl::view3d(theta = 40, phi = 25, zoom = 0.8)

rglwidget()

# GP VISUALISATION EXPERIMENTS using persp

# Risk GP mean surface plot
mean_mat <- matrix(
  data = pred_risk1_g$mean,
  nrow = length(Ftarget),
  ncol = length(Btrigger),
  byrow = FALSE
)

persp(
  x = Ftarget,
  y = Btrigger,
  z = mean_mat,
  xlim = range(Ftarget),
  ylim = range(Btrigger),
  xlab = "Btrigger",
  ylab = "Ftarget",
  zlab = "GP mean (log-risk)",
  theta = 40,
  phi = 25,
  expand = 0.6,
  col = "lightblue",
  ticktype = "detailed"
)

# Catch GP mean surface plot
mean_mat <- matrix(
  data = pred_cat1_g$mean,
  nrow = length(Ftarget),
  ncol = length(Btrigger),
  byrow = FALSE
)

persp(
  x = Ftarget,
  y = Btrigger,
  z = mean_mat,
  xlim = range(Ftarget),
  ylim = range(Btrigger),
  xlab = "Btrigger",
  ylab = "Ftarget",
  zlab = "GP mean (log-catch)",
  theta = 40,
  phi = 25,
  expand = 1.2,
  col = "lightpink",
  ticktype = "detailed"
)

# Risk GP mean and standard deviation surface plot

zcol <- (pred_risk1_g$mean + 2*pred_risk1_g$sd) - (pred_risk1_g$mean - 2*pred_risk1_g$sd)
zcut <- cut(zcol, breaks = 10)  # divide into 10 bins
col_palette <- colorRampPalette(c("lightblue", rgb(0.678, 0.847, 0.902, 0.3)))  # blue
risk_facet_colours <- col_palette(10)[zcut]

mean_mat <- matrix(
  data = pred_risk1_g$mean,
  nrow = length(Ftarget),
  ncol = length(Btrigger),
  byrow = FALSE
)

persp(
  x = Ftarget,
  y = Btrigger,
  z = mean_mat,
  xlim = range(Ftarget),
  ylim = range(Btrigger),
  xlab = "Btrigger",
  ylab = "Ftarget",
  zlab = "GP mean (log-risk)",
  theta = 40,
  phi = 25,
  expand = 0.6,
  col = risk_facet_colours,
  ticktype = "detailed"
)

# Catch GP mean and standard deviation surface plot

zcol <- (pred_cat1_g$mean + 2*pred_cat1_g$sd) - (pred_cat1_g$mean - 2*pred_cat1_g$sd)
zcut <- cut(zcol, breaks = 10)  # divide into 10 bins
col_palette <- colorRampPalette(c("lightpink", rgb(1, 0.8, 0.7, 0.3)))  # pink
catch_facet_colours <- col_palette(10)[zcut]

mean_mat <- matrix(
  data = pred_risk1_g$mean,
  nrow = length(Ftarget),
  ncol = length(Btrigger),
  byrow = FALSE
)

mean_mat <- matrix(
  data = pred_cat1_g$mean,
  nrow = length(Ftarget),
  ncol = length(Btrigger),
  byrow = FALSE
)

persp(
  x = Ftarget,
  y = Btrigger,
  z = mean_mat,
  xlim = range(Ftarget),
  ylim = range(Btrigger),
  xlab = "Btrigger",
  ylab = "Ftarget",
  zlab = "GP mean (log-catch)",
  theta = 40,
  phi = 25,
  expand = 1.2,
  col = catch_facet_colours,
  ticktype = "detailed"
)