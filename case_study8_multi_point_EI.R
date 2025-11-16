dat <- data.frame(
  Ftrgt =c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
  Btrigger =c(110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05),
  catch_median_long =c(34965,34973,34989.5,34990.5,35005,35005.5,35015,35019,35047.5,35046,35026.5,36976.5,36973.5,36984.5,36974,37000.5,37015,37015,37018.5,37004.5,37020.5,37014.5,38775.5,38786.5,38799,38804.5,38803.5,38826.5,38822.5,38824.5,38848,38839,38847.5,40406,40387,40424.5,40424.5,40437.5,40453.5,40465.5,40473.5,40481.5,40466.5,40485.5,41900,41916.5,41901.5,41904,41918.5,41927,41924,41929.5,41937.5,41944,41965.5,43249,43262,43263,43258,43276.5,43278.5,43293.5,43288.5,43284,43296,43303,44441,44448,44468.5,44474,44469,44476.5,44489.5,44499,44511.5,44514,44509.5,45502,45492.5,45539,45532,45532,45559,45577,45602.5,45602,45638.5,45621.5,46449,46475.5,46498.5,46495.5,46502.5,46521.5,46534,46570.5,46592.5,46616,46614,47329.5,47330,47353.5,47361,47406.5,47401,47413,47450.5,47454.5,47504,47477,48110.5,48139.5,48121,48138.5,48166.5,48168.5,48189,48197.5,48232,48294,48251.5,48816,48826,48827,48851.5,48864.5,48885.5,48899.5,48928,48943,49010.5,48981.5,49424,49426,49475.5,49480,49508.5,49512.5,49551,49563.5,49591,49707.5,49621.5,50010.5,50026.5,50053,50045,50074.5,50077,50104.5,50127,50168.5,50294,50241.5,50516,50512,50524,50521.5,50552.5,50586.5,50605,50648,50675,50884.5,50764.5,50907.5,50928.5,50923,50945,50961.5,51000.5,51039.5,51067,51160,51422,51264,51248.5,51245.5,51281.5,51322,51304,51358,51409,51470.5,51608,51973.5,51746,51582.5,51585,51600,51604.5,51607,51650.5,51765.5,51868,52010.5,52472.5,52272.5,51851,51821,51837.5,51857.5,51896.5,51927.5,52033.5,52219.5,52497,52888.5,52702.5,52057,52051.5,52052.5,52111,52119,52156,52348.5,52657.5,52891,53250,53097.5,52231.5,52247,52263,52306,52357,52515,52734,53027,53254,53558,53496,52389,52414,52462.5,52540.5,52609.5,52813,53066.5,53354,53671.5,53717.5,53801.5,52523.5,52585,52619,52691,52820.5,53134.5,53399,53699,53919,53687,53941,52642.5,52705,52737.5,52871,53118.5,53363,53668,54009.5,54090,53593.5,53977,52744,52803,52877.5,53062.5,53300.5,53619.5,53987.5,54232,54228.5,53419,53952,52793.5,52875.5,53004,53258.5,53534,53909.5,54241.5,54371,54177,53122,53795,52888.5,52972,53227.5,53430.5,53789.5,54167.5,54434.5,54372.5,54124.5,52846,53447.5,52927.5,53096.5,53306,53642,54036,54411,54563,54344,53819.5,52607,53170.5,52978.5,53158,53496.5,53883.5,54324.5,54572.5,54596.5,54219,53546.5,52452.5,52930,53011.5,53303.5,53629,54083.5,54523,54736,54448.5,54037,53236,52258,52705,53086.5,53440,53809,54358.5,54728,54774,54374.5,53781.5,53087.5,52109,52514.5,53224,53548,54034,54553.5,54865,54638,54099,53511,52836.5,52007,52371.5,53258.5,53672,54240.5,54704.5,54831,54494.5,53881,53206,52633,51814.5,52251.5,53336,53804.5,54406,54778.5,54735.5,54321.5,53657.5,52993,52475,51750,52086,53374,53947.5,54517.5,54838,54621.5,54069.5,53362.5,52779,52283.5,51672,51870,53457.5,54079,54628,54775,54426,53850,53073.5,52559,52126,51534.5,51776.5,53527.5,54255.5,54685,54695.5,54206.5,53605.5,52829,52348,51928.5,51483.5,51650.5,53691,54390.5,54735.5,54504,54035,53248,52578.5,52115,51779.5,51388.5,51488.5,53901,54507.5,54640,54320,53624.5,52873,52265.5,51886,51583,51219,51340,53935,54551,54509,54191.5,53215,52642,52084,51839,51514,51077.5,51204.5,54004,54454,54412,53796.5,52951,52420.5,51899.5,51653.5,51298,50914.5,51074),
  risk1_full =c(0.01035,0.01025,0.01015,0.01015,0.0101,0.01005,0.01005,0.01,0.00995,0.0099,0.0099,0.0104,0.01035,0.01035,0.01015,0.01015,0.0101,0.0101,0.01005,0.01005,0.00995,0.01,0.01065,0.0105,0.01035,0.01035,0.0102,0.01015,0.0101,0.0101,0.01005,0.01005,0.01005,0.01075,0.01065,0.0105,0.01035,0.01035,0.01025,0.01015,0.01015,0.0101,0.01005,0.0101,0.01095,0.0108,0.01065,0.0105,0.01035,0.01035,0.01025,0.01015,0.01015,0.0101,0.0101,0.01125,0.01105,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.0102,0.01015,0.01015,0.01145,0.0113,0.01095,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.01015,0.0102,0.01165,0.0115,0.0112,0.0109,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.01035,0.0118,0.0117,0.01135,0.0111,0.0108,0.01075,0.01065,0.0105,0.0104,0.01035,0.01035,0.012,0.0118,0.0117,0.01135,0.01105,0.0108,0.0107,0.01065,0.0105,0.01035,0.0104,0.0123,0.012,0.0117,0.0116,0.0113,0.01105,0.0108,0.0107,0.01065,0.0104,0.0105,0.0125,0.0123,0.01185,0.0117,0.0114,0.0112,0.01095,0.0108,0.01065,0.0105,0.01065,0.0128,0.0124,0.012,0.0118,0.0117,0.01135,0.01115,0.01095,0.0108,0.01065,0.01065,0.01335,0.0128,0.0124,0.01195,0.01175,0.01165,0.0114,0.0111,0.01095,0.0107,0.0108,0.01375,0.01305,0.0126,0.01215,0.0119,0.01175,0.01155,0.01135,0.0111,0.0108,0.01085,0.01435,0.01365,0.013,0.01255,0.0121,0.0118,0.01175,0.01145,0.0113,0.01085,0.0111,0.01505,0.01435,0.01345,0.0129,0.0126,0.0122,0.01195,0.0118,0.01145,0.0111,0.0112,0.01595,0.01535,0.0144,0.01365,0.01315,0.01265,0.01225,0.0121,0.01185,0.0113,0.01155,0.01825,0.01695,0.0157,0.0146,0.01395,0.01345,0.013,0.01245,0.0122,0.0115,0.01175,0.02055,0.0187,0.0174,0.0162,0.01515,0.0142,0.0136,0.01325,0.0126,0.01175,0.01225,0.023,0.02145,0.0195,0.0183,0.0166,0.01555,0.0147,0.01365,0.0132,0.0122,0.0126,0.02655,0.0243,0.02165,0.02035,0.01905,0.0173,0.0159,0.0149,0.01365,0.0127,0.0131,0.03135,0.02835,0.0257,0.0232,0.02135,0.01945,0.0173,0.0162,0.01445,0.01305,0.01375,0.03595,0.03275,0.02965,0.0266,0.02455,0.0224,0.02,0.01775,0.01615,0.01385,0.0143,0.04305,0.0394,0.0356,0.03165,0.0288,0.02615,0.02275,0.02035,0.01795,0.0144,0.01565,0.05125,0.04695,0.04195,0.03715,0.0325,0.02935,0.0263,0.02325,0.02055,0.01565,0.0178,0.06055,0.0556,0.0498,0.04385,0.0387,0.0333,0.0299,0.02595,0.0229,0.0176,0.0201,0.0693,0.0646,0.05805,0.05135,0.04465,0.0387,0.03355,0.03,0.02545,0.0194,0.0221,0.0838,0.07655,0.06795,0.0602,0.05215,0.0439,0.03785,0.03375,0.02885,0.02175,0.0249,0.098,0.08945,0.07985,0.06965,0.061,0.05115,0.0439,0.0381,0.0328,0.0244,0.02835,0.1138,0.10325,0.0922,0.08155,0.0709,0.0597,0.05085,0.0434,0.0374,0.02765,0.03245,0.1311,0.11815,0.1061,0.09305,0.08055,0.0686,0.05815,0.0497,0.04275,0.03125,0.03625,0.1511,0.1359,0.12035,0.10545,0.0924,0.07835,0.06595,0.05555,0.0471,0.0347,0.0406,0.1708,0.15385,0.1352,0.1193,0.1038,0.0897,0.07535,0.0632,0.0526,0.0394,0.04605,0.1919,0.17235,0.1528,0.13285,0.11585,0.1,0.08535,0.07135,0.0599,0.0438,0.05095,0.21485,0.19175,0.16985,0.1475,0.1277,0.11085,0.09605,0.08165,0.068,0.049,0.05735,0.23525,0.21115,0.1869,0.1634,0.1423,0.12235,0.10575,0.09195,0.07625,0.05485,0.06385,0.25645,0.22975,0.2028,0.1787,0.1558,0.13495,0.1151,0.1004,0.08545,0.0613,0.072,0.2779,0.25085,0.22095,0.19525,0.17,0.1477,0.12705,0.1107,0.09435,0.06765,0.0799,0.30175,0.27355,0.24105,0.2126,0.18495,0.1607,0.1399,0.1203,0.10375,0.0743,0.088,0.3241,0.2949,0.26,0.2288,0.2007,0.1747,0.1507,0.1303,0.1143,0.08145,0.0981)
)

rescale_Her <- function(runs,dat){
  # Rescale Ftarget and Btrigger to [0,1] using ranges from `dat`.
  # This makes optimisation / GP fitting numerically more stable.
  ret <- runs
  minF <- min(dat$Ftarget)
  rangeF <- diff(range(dat$Ftarget))
  ret$Ftarget <- (ret$Ftarget - minF)/rangeF
  minB <- min(dat$Btrigger)
  rangeB <- diff(range(dat$Btrigger))
  ret$Btrigger <- (ret$Btrigger - minB)/rangeB
  return(ret)
}

unrescale_Her <- function(runs,dat){
  # Undo the rescaling performed by `rescale_Her` to recover original units.
  ret <- runs
  minF <- min(dat$Ftarget)
  rangeF <- diff(range(dat$Ftarget))
  ret$Ftarget <- ret$Ftarget*rangeF + minF
  minB <- min(dat$Btrigger)
  rangeB <- diff(range(dat$Btrigger))
  ret$Btrigger <- ret$Btrigger*rangeB + minB
  return(ret)
}

equal_tol <- function(x,y,tol=1e-12){
  # Numeric equality check with a tiny tolerance to avoid floating point issues.
  abs(x-y) < tol
}


#' @param xi is a scalar for exploration/exploitation trade off
expected_improvement <- function(mu, sigma, y_best, xi = 0.05, task = "max", pred_risk, eps = 1e-4) 
{
  if (task == "min") imp <- y_best - mu - xi
  if (task == "max") imp <- mu - y_best - xi
  if (is.null(imp)) stop('task must be "min" or "max"')
  Z <- imp / sigma
  ei <- imp * pnorm(Z) + sigma * dnorm(Z)
  ei[sigma == 0.0] <- 0.0
# Giving points with prob(risk <= 0.05) < 0.01 an expected improvement of zero so we avoid them
  ei <- ifelse(pred_risk < eps, 0, ei)
  return(ei)
}

library(DiceKriging)
library(dplyr)
library(plot3D)
library(stats)  

# Use a logarithmic y-axis for the plot, i.e. log="y" tells the plot to display the y-axis on a log scale.
#CoPilot suggested this could result in a double transformation in lines 62 and 63, which may not be what is intended.
plot(dat$Ftrgt,log(dat$catch_median_long),log="y")
plot(dat$Btrigger,log(dat$catch_median_long),log="y")
plot(dat$Ftrgt,dat$risk1_full,log="y")
abline(h=0.05,col="red")
plot(dat$Btrigger,dat$risk1_full,log="y")
abline(h=0.05,col="red")


# Checks length of Ftrgt and Btrigger sets
length(unique(dat$Ftrgt))
length(unique(dat$Btrigger))

# Set Random Number Generator seed for reproducible random sampling.
# Copilot says I could change this to explore sensitivity of the process to the intial sample
set.seed(18)




#Round 1

# A space filling algorithm (seems simply chooses evenly spaced points) is used in the first round below to select points,
# as detailed in Section 3 Case Study in the paper
round1 <- data.frame(Ftrgt=sample(unique(dat$Ftrgt)[floor(seq(2,40,l=8))]),Btrigger=sample(unique(dat$Btrigger),size = 8))
plot(round1[,2:1])


# Choose an initial small set of design points and fit GP emulators for log-catch and log-risk using these initial runs.
Ftarget <- sort(unique(dat$Ftrgt))
Btrigger <- sort(unique(dat$Btrigger))
dat1 <- data.frame(expand.grid(Ftarget,Btrigger))
names(dat1) <- c("Ftarget","Btrigger")

gridd <- dat1[,c("Ftarget","Btrigger")]
gridd<- rescale_Her(gridd[order(gridd$Ftarget,gridd$Btrigger),],dat=dat1)

dat_run <- left_join(round1,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

# build the emulator for median catch

# res_cat is the log of observed median catch at the design runs. The GP
# on line 109 models this response using polynomial terms in the formula.
res_cat <- log(runs$C_long)

# Looks like it is using maximum likelihood estimation below and mentions this nugget thing which Mike was talking about
# CoPilot says "nugget: a tiny diagonal noise term for numerical stability."

# CoPilot also said "covtype = "exp": exponential correlation function (gives a less smooth prior than squared‑exponential)."
# which could maybe be something to follow up on

# Also has covariance type which is important for Gaussian Processes

# Below carries out the Kriging process for the two separate emulators, as detailed in the paper section 3.1
gp_cat <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")
res_risk <- log(runs$risk3_long)

gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

# Gets the Gaussian Process to produce a prediciton for risk at every point in gridd 
# gridd is the rescaled grid of Ftrgt and Btrigger
#This has a mean and standard deviation as we are unsure of the exact risk
pred_risk1_g <- predict(gp_risk,newdata=gridd,type="SK")


# Estimate median predicted risk
med_risk1 <- exp(pred_risk1_g$mean)
# plot it
image2D(matrix(med_risk1,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")

# now lets look at the probability that the risk is less than or equal to 0.05
prisk1 <- pnorm(log(0.05),pred_risk1_g$mean,pred_risk1_g$sd+1e-12)
# plot it
image2D(matrix(prisk1,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# Predicts mean and uncertainty for log(catch) at each parameter combination in gridd
# SK means simple kriging
pred_cat1_g <- predict(gp_cat,newdata=gridd,type="SK")

# Best catch observed so far where risk is below 0.05
max1 <- max(runs$C_long[runs$risk3_long < 0.05])

#Convert to max1 log scale as emulator trained on log(catch)
# pcat1 is the probability that the catch is less than or equal to max1 (the best safe catch observed) at each point in gridd
pcat1 <- pnorm(log(max1),pred_cat1_g$mean,pred_cat1_g$sd+1e-12)

# mark which points in gridd are still good candidates
eps <- 1e-4
possible1 <- (apply(cbind((1-pcat1) , prisk1),1,min) >  eps)

# collect final points - this is so we know what to do in round 2
pot_points1 <- gridd[possible1,]

#Removes already evaluated points from those that are still possible so don't evalute again
tmp <-setdiff(pot_points1,runs[,1:2])
pot_points1 <- tmp

# If no plausible points remain, stop the process
iteration <- 1
if (sum(possible1) == 0) {
  stop("No plausible points remaining at round", iteration, "\n")
  ans<-unrescale_Her(pot_points1[1,],dat1)
  ans
}

# getting catch out of log(catch) for each point in gridd
med_cat1 <- exp(pred_cat1_g$mean)


# Below produces heat maps for round 1 
image2D(matrix(med_cat1,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat1,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
# Visualises region that has risk <0.05 and better catch than best evaluated so far
image2D(matrix(possible1 * (1-pcat1),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# TODO: I might have a different way of evaluating points but I should change it to this for general applicability????
# Last time I tried, it turned out terribly and broke lots of things whilst also seeming to break the convergence to one point
# Defining a deterministic objective function for later acquisition function calculation
 #       objective <- function (Fval, Bval, dat) {
  #        row <- subset(data, Ftarget == Fval & Btrigger == Bval)
  #      # Says if number of rows in row is 0, i.e. row wasn't found, then stop with the warning
   #       if (nrow(row) == 0) stop("Combination not found in lookup table.")
    #      catch <- row$catch
     #     risk <- row$risk
      #    if (risk>0.05) return(-1e6)
       #   return(catch)
      #  }


# Round 2

# pot_points1 is the points that are still plausible as the optimum from the first round
# we're selecting 8 of these to evaluate next


#NEW SELECTION PROCESS USING ACQUISITION FUNCTION
mu1 <- pred_cat1_g$mean
sigma1 <- pred_cat1_g$sd

# Would like to compute only for possible1 to save time as only possible1 are looked at in our selection process
# for the next points, but seems to cause vector issues - TODO: Double check by thinking on some more and running
ei1 <- expected_improvement(mu1, sigma1, log(max1), xi = 0.05, pred_risk = prisk1, eps = 1e-4)
gridd1_with_ei <- gridd
gridd1_with_ei$ei1<- ei1
gridd1_with_ei$possible1 <- possible1


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand1 <- subset(gridd1_with_ei, possible1 == TRUE & ei1 > 0)

# Create composite keys to avoid floating point errors
cand1$key <- paste(cand1$Ftarget, cand1$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand1 <- cand1[!(cand1$key %in% runs$key), ]


# If there are fewer than 8 plausible points left to sample, take them all
if (nrow(cand1) <= 8) {
  next_points <- cand1[order(-cand1$ei1), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand1[order(-cand1$ei1), ][1:nrow(cand1), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei1), c("Ftarget", "Btrigger", "ei1")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round2 <- rbind(round1, new_points)


#REAL START OF ROUND 2
# Joining with full dataset and rescaling again
dat_run <- left_join(round2,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk2_g <- predict(gp_risk,newdata=gridd,type="SK")

# now lets look at the probability that the risk is less than 0.05
med_risk2 <- exp(pred_risk2_g$mean)

image2D(matrix(med_risk2,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
# now lets look at the probability that the risk is less than 0.05 
prisk2 <- pnorm(log(0.05),pred_risk2_g$mean,pred_risk2_g$sd+1e-12)
image2D(matrix(prisk2,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

# now the catch

pred_cat2_g <- predict(gp_cat,newdata=gridd,type="SK")
# median catch
max2 <- max(runs$C_long[runs$risk3_long < 0.05])# the max so far
pcat2 <- pnorm(log(max2),pred_cat2_g$mean,pred_cat2_g$sd+1e-12)

possible2 <- (apply(cbind((1-pcat2) , prisk2),1,min) >  eps)
# If no plausible points remain, stop the process
iteration <- 2
if (sum(possible2) == 0) {
    cat("No plausible points remaining. Stopping at round", iteration, "\n")
    break
  }

med_cat2 <- exp(pred_cat2_g$mean)

#These are probably the same images as in the first round for the second round

image2D(matrix(med_cat2,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat2,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible2 * (1-pcat2),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# Round 3 - where is the condition to stop if there is only one point left in each round? - issue for round 7 really

mu2 <- pred_cat2_g$mean
sigma2 <- pred_cat2_g$sd

ei2 <- expected_improvement(mu2, sigma2, log(max2), xi = 0.05, pred_risk = prisk2, eps = 1e-4)
gridd2_with_ei <- gridd
gridd2_with_ei$ei2<- ei2


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand2 <- subset(gridd2_with_ei, possible2 == TRUE & ei2 > 0)

# Create composite keys to avoid floating point errors
cand2$key <- paste(cand2$Ftarget, cand2$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand2 <- cand2[!(cand2$key %in% runs$key), ]


# If there are fewer than 8, take them all
if (nrow(cand2) <= 8) {
  next_points <- cand2[order(-cand2$ei2), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand2[order(-cand2$ei2), ][1:nrow(cand2), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei2), c("Ftarget", "Btrigger", "ei2")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round3 <- rbind(round2, new_points)

dat_run <- left_join(round3,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk3_g <- predict(gp_risk,newdata=gridd,type="SK")
med_risk3 <- exp(pred_risk3_g$mean)
image2D(matrix(med_risk3,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
prisk3 <- pnorm(log(0.05),pred_risk3_g$mean,pred_risk3_g$sd+1e-12)
image2D(matrix(prisk3,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

pred_cat3_g <- predict(gp_cat,newdata=gridd,type="SK")
max3 <- max(runs$C_long[runs$risk3_long < 0.05])
pcat3 <- pnorm(log(max3),pred_cat3_g$mean,pred_cat3_g$sd+1e-12)

possible3 <- (apply(cbind((1-pcat3) , prisk3),1,min) >  eps)

med_cat3 <- exp(pred_cat3_g$mean)
image2D(matrix(med_cat3,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat3,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible3 * (1-pcat3),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# Round 4

mu3 <- pred_cat3_g$mean
sigma3 <- pred_cat3_g$sd

ei3 <- expected_improvement(mu3, sigma3, log(max3), xi = 0.05, pred_risk = prisk3, eps = 1e-4)
gridd3_with_ei <- gridd
gridd3_with_ei$ei3<- ei3


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand3 <- subset(gridd3_with_ei, possible3 == TRUE & ei3 > 0)

# Create composite keys to avoid floating point errors
cand3$key <- paste(cand3$Ftarget, cand3$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand3 <- cand3[!(cand3$key %in% runs$key), ]

# If there are fewer than 8, take them all
if (nrow(cand3) <= 8) {
  next_points <- cand3[order(-cand3$ei3), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand3[order(-cand3$ei3), ][1:nrow(cand3), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei3), c("Ftarget", "Btrigger", "ei3")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round4 <- rbind(round3, new_points)

dat_run <- left_join(round4,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk4_g <- predict(gp_risk,newdata=gridd,type="SK")
med_risk4 <- exp(pred_risk4_g$mean)
image2D(matrix(med_risk4,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
prisk4 <- pnorm(log(0.05),pred_risk4_g$mean,pred_risk4_g$sd+1e-12)
image2D(matrix(prisk4,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


pred_cat4_g <- predict(gp_cat,newdata=gridd,type="SK")
max4 <- max(runs$C_long[runs$risk3_long < 0.05])
pcat4 <- pnorm(log(max4),pred_cat4_g$mean,pred_cat4_g$sd+1e-12)

possible4 <- (apply(cbind((1-pcat4) , prisk4),1,min) >  eps)

med_cat4 <- exp(pred_cat4_g$mean)
image2D(matrix(med_cat4,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat4,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible4 * (1-pcat4),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

# Round 5

mu4 <- pred_cat4_g$mean
sigma4 <- pred_cat4_g$sd

ei4 <- expected_improvement(mu4, sigma4, log(max4), xi = 0.05, pred_risk = prisk4, eps = 1e-4)
gridd4_with_ei <- gridd
gridd4_with_ei$ei4<- ei4


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand4 <- subset(gridd4_with_ei, possible4 == TRUE & ei4 > 0)

# Create composite keys to avoid floating point errors
cand4$key <- paste(cand4$Ftarget, cand4$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand4 <- cand4[!(cand4$key %in% runs$key), ]

# If there are fewer than 8, take them all
if (nrow(cand4) <= 8) {
  next_points <- cand4[order(-cand4$ei4), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand4[order(-cand4$ei4), ][1:nrow(cand4), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei4), c("Ftarget", "Btrigger", "ei4")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round5 <- rbind(round4, new_points)

dat_run <- left_join(round5,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk5_g <- predict(gp_risk,newdata=gridd,type="SK")
med_risk5 <- exp(pred_risk5_g$mean)
image2D(matrix(med_risk5,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
prisk5 <- pnorm(log(0.05),pred_risk5_g$mean,pred_risk5_g$sd+1e-12)
image2D(matrix(prisk5,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

pred_cat5_g <- predict(gp_cat,newdata=gridd,type="SK")
max5 <- max(runs$C_long[runs$risk3_long < 0.05])
pcat5 <- pnorm(log(max5),pred_cat5_g$mean,pred_cat5_g$sd+1e-12)

possible5 <- (apply(cbind((1-pcat5) , prisk5),1,min) >  eps)

med_cat5 <- exp(pred_cat5_g$mean)
image2D(matrix(med_cat5,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat5,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible5 * (1-pcat5),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# Round 6

mu5 <- pred_cat5_g$mean
sigma5 <- pred_cat5_g$sd

ei5 <- expected_improvement(mu5, sigma5, log(max5), xi = 0.05, pred_risk = prisk5, eps = 1e-4)
gridd5_with_ei <- gridd
gridd5_with_ei$ei5<- ei5


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand5 <- subset(gridd5_with_ei, possible5 == TRUE & ei5 > 0)

# Create composite keys to avoid floating point errors
cand5$key <- paste(cand5$Ftarget, cand5$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand5 <- cand5[!(cand5$key %in% runs$key), ]

# If there are fewer than 8, take them all
if (nrow(cand5) <= 8) {
  next_points <- cand5[order(-cand5$ei5), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand5[order(-cand5$ei5), ][1:nrow(cand5), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei5), c("Ftarget", "Btrigger", "ei5")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round6 <- rbind(round5, new_points)

#Does the lookup to get catch and risk for each point - like evaluating the objective funciton
dat_run <- left_join(round6,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk6_g <- predict(gp_risk,newdata=gridd,type="SK")
med_risk6 <- exp(pred_risk6_g$mean)
image2D(matrix(med_risk6,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
prisk6 <- pnorm(log(0.05),pred_risk6_g$mean,pred_risk6_g$sd+1e-12)
image2D(matrix(prisk6,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

# now the catch

pred_cat6_g <- predict(gp_cat,newdata=gridd,type="SK")
max6 <- max(runs$C_long[runs$risk3_long < 0.05])
pcat6 <- pnorm(log(max6),pred_cat6_g$mean,pred_cat6_g$sd+1e-12)

possible6 <- (apply(cbind((1-pcat6) , prisk6),1,min) >  eps)

med_cat6 <- exp(pred_cat6_g$mean)
image2D(matrix(med_cat6,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat6,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible6 * (1-pcat6),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


# Round 7

mu6 <- pred_cat6_g$mean
sigma6 <- pred_cat6_g$sd

ei6 <- expected_improvement(mu6, sigma6, log(max6), xi = 0.05, pred_risk = prisk6, eps = 1e-4)
gridd6_with_ei <- gridd
gridd6_with_ei$ei6<- ei6


# Now, putting distance between points I am going to sample

# Filter to plausible points with non-zero EI
cand6 <- subset(gridd6_with_ei, possible6 == TRUE & ei6 > 0)

# Create composite keys to avoid floating point errors
cand6$key <- paste(cand6$Ftarget, cand6$Btrigger, sep = "_")
runs$key <- paste(runs$Ftarget, runs$Btrigger, sep = "_")

# Remove already evaluated points
cand6 <- cand6[!(cand6$key %in% runs$key), ]

# If there are fewer than 8, take them all
if (nrow(cand6) <= 8) {
  next_points <- cand6[order(-cand6$ei6), c("Ftarget", "Btrigger")]
} else {
  # Rank by EI
  top_candidates <- cand6[order(-cand6$ei6), ][1:nrow(cand6), ]
  
  # Use k-means to enforce spatial diversity among high-EI points
  set.seed(123)
  km <- kmeans(top_candidates[, c("Ftarget", "Btrigger")], centers = 8)
  
  # Pick the point with the highest EI within each cluster
  top_candidates$cluster <- km$cluster
  next_points <- do.call(rbind, lapply(split(top_candidates, top_candidates$cluster), function(df) {
    df[which.max(df$ei6), c("Ftarget", "Btrigger", "ei6")]
  }))
}

coords <- next_points[, c("Ftarget", "Btrigger")]
new_points <- signif(unrescale_Her(coords, dat1), 2)
names(new_points) <- c("Ftrgt", "Btrigger")
round7 <- rbind(round6, new_points)

dat_run <- left_join(round7,dat)
names(dat_run) <- c("Ftarget","Btrigger","C_long","risk3_long")
runs <- rescale_Her(dat_run,dat=dat1)

res_cat <- log(runs$C_long)# - predict(qgams_cat)
gp_cat <- km(~I(log(Ftarget+0.1)^2)+I(log(Ftarget+0.1))+ I(log(Ftarget+0.1)^3) + I(Btrigger) + I(Btrigger * log(Ftarget+0.1)),design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_cat,nugget=1e-12*var(res_cat),covtype = "exp")

res_risk <- log(runs$risk3_long)# - predict(qgams_risk)
gp_risk <- km(~.^2,design=runs[,c("Ftarget","Btrigger")],estim.method="MLE",response = res_risk,nugget=1e-12*var(res_risk),covtype = "exp")

pred_risk7_g <- predict(gp_risk,newdata=gridd,type="SK")
med_risk7 <- exp(pred_risk7_g$mean)
image2D(matrix(med_risk7,nrow=11),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
prisk7 <- pnorm(log(0.05),pred_risk7_g$mean,pred_risk7_g$sd+1e-12)
image2D(matrix(prisk7,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))


pred_cat7_g <- predict(gp_cat,newdata=gridd,type="SK")
max7 <- max(runs$C_long[runs$risk3_long < 0.05])
pcat7 <- pnorm(log(max7),pred_cat7_g$mean,pred_cat7_g$sd+1e-12)

possible7 <- (apply(cbind((1-pcat7) , prisk7),1,min) >  eps)

med_cat7 <- exp(pred_cat7_g$mean)
image2D(matrix(med_cat7,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt")
image2D(matrix(1-pcat7,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))
image2D(matrix(possible7 * (1-pcat7),nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab="Btrigger",ylab="Ftrgt",breaks=c(-1e-12,0.0001,0.05,0.5,0.9,1))

#TODO: Make number of rounds more flexible

#only one left

# We assume it ends in seven rounds every time in this case study, which wouldn't be great for general application
# Maybe there are other files that are more general than this one that I haven't looked at yet?
ans<-unrescale_Her(pot_points7[1,],dat1)
ans

# Prints the variable round7
round7

round7$Round <- c(rep(1:6,each=8),rep(7,nrow(round7) - nrow(round6)))

# Makes a data set such that we can plot which points were in which round
dat_round<-left_join(dat,round7) 


#Helps us colour code points by the round which they were sampled in
col_round<-c("white",hcl.colors(7, "viridis", rev = TRUE))