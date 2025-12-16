# GP VISUALISATION EXPERIMENTS use DiceView

library(DiceView)
library(rgl)

Xlim <- rbind(
  apply(gp_risk@X, 2, min),
  apply(gp_risk@X, 2, max)
)


sectionview3d(
  gp_risk,
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