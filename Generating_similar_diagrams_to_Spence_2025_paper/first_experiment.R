##### plots

# dat <- data.frame(
#   Ftrgt =c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.11,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.14,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.16,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.17,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.18,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.19,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.22,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.23,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.24,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.26,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.29,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.31,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.32,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.34,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.36,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.37,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.42,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.43,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.45,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.47,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.48,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.49,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
#   Btrigger =c(110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05,110000,120000,130000,140000,150000,160000,170000,180000,190000,210000,2e+05),
#   catch_median_long =c(34965,34973,34989.5,34990.5,35005,35005.5,35015,35019,35047.5,35046,35026.5,36976.5,36973.5,36984.5,36974,37000.5,37015,37015,37018.5,37004.5,37020.5,37014.5,38775.5,38786.5,38799,38804.5,38803.5,38826.5,38822.5,38824.5,38848,38839,38847.5,40406,40387,40424.5,40424.5,40437.5,40453.5,40465.5,40473.5,40481.5,40466.5,40485.5,41900,41916.5,41901.5,41904,41918.5,41927,41924,41929.5,41937.5,41944,41965.5,43249,43262,43263,43258,43276.5,43278.5,43293.5,43288.5,43284,43296,43303,44441,44448,44468.5,44474,44469,44476.5,44489.5,44499,44511.5,44514,44509.5,45502,45492.5,45539,45532,45532,45559,45577,45602.5,45602,45638.5,45621.5,46449,46475.5,46498.5,46495.5,46502.5,46521.5,46534,46570.5,46592.5,46616,46614,47329.5,47330,47353.5,47361,47406.5,47401,47413,47450.5,47454.5,47504,47477,48110.5,48139.5,48121,48138.5,48166.5,48168.5,48189,48197.5,48232,48294,48251.5,48816,48826,48827,48851.5,48864.5,48885.5,48899.5,48928,48943,49010.5,48981.5,49424,49426,49475.5,49480,49508.5,49512.5,49551,49563.5,49591,49707.5,49621.5,50010.5,50026.5,50053,50045,50074.5,50077,50104.5,50127,50168.5,50294,50241.5,50516,50512,50524,50521.5,50552.5,50586.5,50605,50648,50675,50884.5,50764.5,50907.5,50928.5,50923,50945,50961.5,51000.5,51039.5,51067,51160,51422,51264,51248.5,51245.5,51281.5,51322,51304,51358,51409,51470.5,51608,51973.5,51746,51582.5,51585,51600,51604.5,51607,51650.5,51765.5,51868,52010.5,52472.5,52272.5,51851,51821,51837.5,51857.5,51896.5,51927.5,52033.5,52219.5,52497,52888.5,52702.5,52057,52051.5,52052.5,52111,52119,52156,52348.5,52657.5,52891,53250,53097.5,52231.5,52247,52263,52306,52357,52515,52734,53027,53254,53558,53496,52389,52414,52462.5,52540.5,52609.5,52813,53066.5,53354,53671.5,53717.5,53801.5,52523.5,52585,52619,52691,52820.5,53134.5,53399,53699,53919,53687,53941,52642.5,52705,52737.5,52871,53118.5,53363,53668,54009.5,54090,53593.5,53977,52744,52803,52877.5,53062.5,53300.5,53619.5,53987.5,54232,54228.5,53419,53952,52793.5,52875.5,53004,53258.5,53534,53909.5,54241.5,54371,54177,53122,53795,52888.5,52972,53227.5,53430.5,53789.5,54167.5,54434.5,54372.5,54124.5,52846,53447.5,52927.5,53096.5,53306,53642,54036,54411,54563,54344,53819.5,52607,53170.5,52978.5,53158,53496.5,53883.5,54324.5,54572.5,54596.5,54219,53546.5,52452.5,52930,53011.5,53303.5,53629,54083.5,54523,54736,54448.5,54037,53236,52258,52705,53086.5,53440,53809,54358.5,54728,54774,54374.5,53781.5,53087.5,52109,52514.5,53224,53548,54034,54553.5,54865,54638,54099,53511,52836.5,52007,52371.5,53258.5,53672,54240.5,54704.5,54831,54494.5,53881,53206,52633,51814.5,52251.5,53336,53804.5,54406,54778.5,54735.5,54321.5,53657.5,52993,52475,51750,52086,53374,53947.5,54517.5,54838,54621.5,54069.5,53362.5,52779,52283.5,51672,51870,53457.5,54079,54628,54775,54426,53850,53073.5,52559,52126,51534.5,51776.5,53527.5,54255.5,54685,54695.5,54206.5,53605.5,52829,52348,51928.5,51483.5,51650.5,53691,54390.5,54735.5,54504,54035,53248,52578.5,52115,51779.5,51388.5,51488.5,53901,54507.5,54640,54320,53624.5,52873,52265.5,51886,51583,51219,51340,53935,54551,54509,54191.5,53215,52642,52084,51839,51514,51077.5,51204.5,54004,54454,54412,53796.5,52951,52420.5,51899.5,51653.5,51298,50914.5,51074),
#   risk1_full =c(0.01035,0.01025,0.01015,0.01015,0.0101,0.01005,0.01005,0.01,0.00995,0.0099,0.0099,0.0104,0.01035,0.01035,0.01015,0.01015,0.0101,0.0101,0.01005,0.01005,0.00995,0.01,0.01065,0.0105,0.01035,0.01035,0.0102,0.01015,0.0101,0.0101,0.01005,0.01005,0.01005,0.01075,0.01065,0.0105,0.01035,0.01035,0.01025,0.01015,0.01015,0.0101,0.01005,0.0101,0.01095,0.0108,0.01065,0.0105,0.01035,0.01035,0.01025,0.01015,0.01015,0.0101,0.0101,0.01125,0.01105,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.0102,0.01015,0.01015,0.01145,0.0113,0.01095,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.01015,0.0102,0.01165,0.0115,0.0112,0.0109,0.01075,0.01065,0.0105,0.0104,0.01035,0.01025,0.01035,0.0118,0.0117,0.01135,0.0111,0.0108,0.01075,0.01065,0.0105,0.0104,0.01035,0.01035,0.012,0.0118,0.0117,0.01135,0.01105,0.0108,0.0107,0.01065,0.0105,0.01035,0.0104,0.0123,0.012,0.0117,0.0116,0.0113,0.01105,0.0108,0.0107,0.01065,0.0104,0.0105,0.0125,0.0123,0.01185,0.0117,0.0114,0.0112,0.01095,0.0108,0.01065,0.0105,0.01065,0.0128,0.0124,0.012,0.0118,0.0117,0.01135,0.01115,0.01095,0.0108,0.01065,0.01065,0.01335,0.0128,0.0124,0.01195,0.01175,0.01165,0.0114,0.0111,0.01095,0.0107,0.0108,0.01375,0.01305,0.0126,0.01215,0.0119,0.01175,0.01155,0.01135,0.0111,0.0108,0.01085,0.01435,0.01365,0.013,0.01255,0.0121,0.0118,0.01175,0.01145,0.0113,0.01085,0.0111,0.01505,0.01435,0.01345,0.0129,0.0126,0.0122,0.01195,0.0118,0.01145,0.0111,0.0112,0.01595,0.01535,0.0144,0.01365,0.01315,0.01265,0.01225,0.0121,0.01185,0.0113,0.01155,0.01825,0.01695,0.0157,0.0146,0.01395,0.01345,0.013,0.01245,0.0122,0.0115,0.01175,0.02055,0.0187,0.0174,0.0162,0.01515,0.0142,0.0136,0.01325,0.0126,0.01175,0.01225,0.023,0.02145,0.0195,0.0183,0.0166,0.01555,0.0147,0.01365,0.0132,0.0122,0.0126,0.02655,0.0243,0.02165,0.02035,0.01905,0.0173,0.0159,0.0149,0.01365,0.0127,0.0131,0.03135,0.02835,0.0257,0.0232,0.02135,0.01945,0.0173,0.0162,0.01445,0.01305,0.01375,0.03595,0.03275,0.02965,0.0266,0.02455,0.0224,0.02,0.01775,0.01615,0.01385,0.0143,0.04305,0.0394,0.0356,0.03165,0.0288,0.02615,0.02275,0.02035,0.01795,0.0144,0.01565,0.05125,0.04695,0.04195,0.03715,0.0325,0.02935,0.0263,0.02325,0.02055,0.01565,0.0178,0.06055,0.0556,0.0498,0.04385,0.0387,0.0333,0.0299,0.02595,0.0229,0.0176,0.0201,0.0693,0.0646,0.05805,0.05135,0.04465,0.0387,0.03355,0.03,0.02545,0.0194,0.0221,0.0838,0.07655,0.06795,0.0602,0.05215,0.0439,0.03785,0.03375,0.02885,0.02175,0.0249,0.098,0.08945,0.07985,0.06965,0.061,0.05115,0.0439,0.0381,0.0328,0.0244,0.02835,0.1138,0.10325,0.0922,0.08155,0.0709,0.0597,0.05085,0.0434,0.0374,0.02765,0.03245,0.1311,0.11815,0.1061,0.09305,0.08055,0.0686,0.05815,0.0497,0.04275,0.03125,0.03625,0.1511,0.1359,0.12035,0.10545,0.0924,0.07835,0.06595,0.05555,0.0471,0.0347,0.0406,0.1708,0.15385,0.1352,0.1193,0.1038,0.0897,0.07535,0.0632,0.0526,0.0394,0.04605,0.1919,0.17235,0.1528,0.13285,0.11585,0.1,0.08535,0.07135,0.0599,0.0438,0.05095,0.21485,0.19175,0.16985,0.1475,0.1277,0.11085,0.09605,0.08165,0.068,0.049,0.05735,0.23525,0.21115,0.1869,0.1634,0.1423,0.12235,0.10575,0.09195,0.07625,0.05485,0.06385,0.25645,0.22975,0.2028,0.1787,0.1558,0.13495,0.1151,0.1004,0.08545,0.0613,0.072,0.2779,0.25085,0.22095,0.19525,0.17,0.1477,0.12705,0.1107,0.09435,0.06765,0.0799,0.30175,0.27355,0.24105,0.2126,0.18495,0.1607,0.1399,0.1203,0.10375,0.0743,0.088,0.3241,0.2949,0.26,0.2288,0.2007,0.1747,0.1507,0.1303,0.1143,0.08145,0.0981)
# )

# ## MAKING PLOTS FROM PRECOMPUTED DATA
# # load libraries
# library(plot3D)
# # set up to save as EPS
# setEPS()
# # provide file name and size
# postscript("full_grid.eps",width=5.5,height=5)
# # create a grid of six plots
# par(mfrow=c(2,3))
# # set outer margins
# par(oma=c(1,1,1,3))
# # set up plot margins for this plot
# par(mar=c(2,4,0,0))
# # first plot: median catch v Ftarget (taken straight from precomputed data)
# plot(dat$Ftrgt,(dat$catch_median_long),log="y",pch=16,ylab="Median yield",xlab=expression(F[target]),axes=F)
# # draws the axis on the left
# axis(2)
# # makes a box around the plot
# box()
# # again, set up plot margins but slightly different this time
# par(mar=c(2,2,0,0))
# # second plot: median catch v Btrigger (taken straight from precomputed data)
# plot(dat$Btrigger,(dat$catch_median_long),log="y",pch=16,ylab="Median yield",xlab=expression(B[trigger]),axes=F)
# # makes a box around the plot
# box()
# # set amrgins for the third plot
# par(mar=c(2,4.5,0,0))
# # third plot: heatmap of median catch v Ftarget and Btrigger (taken straight from precomputed data)
# image2D(matrix(dat$catch_median_long,nrow=11),y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab=expression(B[trigger]),ylab=expression(F[target]),axes=F)
# # draws the axis on the left
# axis(2)
# # add margins for next plot
# par(mar=c(4,4,0,0))
# # plot risk v Ftarget (taken straight from precomputed data)
# plot(dat$Ftrgt,dat$risk1_full,log="y",pch=16,ylab="Risk",xlab=expression(F[target]))
# # draw a line across the plot at risk = 0.05
# abline(h=0.05,lty=2)
# # set margins for next plot
# par(mar=c(4,2,0,0))
# # plot risk v Btrigger (taken straight from precomputed data)
# plot(dat$Btrigger,dat$risk1_full,log="y",xlab=expression(B[trigger]),pch=16,ylab="",axes=F)
# # draw axis on the bottom
# axis(1)
# # make a box around the plot
# box()
# # draw a line across the plot at risk = 0.05
# abline(h=0.05,lty=2)
# # add margins for the last plot
# par(mar=c(4,4.5,0,0))
# # heatmap of risk v Ftarget and Btrigger (taken straight from precomputed data)
# image2D(matrix(dat$risk1_full,nrow=11),y=sort(unique(dat$Ftrgt)),breaks=c(0,0.01,0.025,0.05,0.1,0.2,0.4),x=sort(unique(dat$Btrigger)),xlab=expression(B[trigger]),ylab=expression(F[target]))
# # save file and close
# dev.off()


######### demonstrate a Gaussian process
# load libraries
library(DiceKriging)
# loads the package that for GAMs that draws a smooth trendline
library(mgcv)

# the sample space
dat_all <- data.frame(x=seq(0,2,0.01),y=0)
# Our simple objective function
y_fun <- function(x){-2*x^2 + 3*x + 2}

# sample five points
num_round <- 5
# we want to be optimistic about the highest value the curve could have
cert <- 0.9999
# We use the Gaussian covariance function
covtype <- "gauss"

# generates the first five data points spread evenly throughout the sample space
# unsure on details due to SpenceTools dependency so may have to run to find out
xs <- SpenceTools::specific_floor(seq(0.2,1.8,length.out=num_round),digits=2)
# get the values for the objective function at these points
dat1 <- data.frame(x=xs,y=y_fun(xs))
#Record the best y-value w ehave seen so far
best1 <- max(dat1$y)

# FOR THE TWO LINES BELOW, I CAN LIKELY JUST USE MY GP

# create a trendline using a GAM to get the general shape of the function
gam_1 <- gam(y~x,data=dat1)
# fit the GP to the residuals of the GAM
gp_1 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = gam_1$residuals,covtype = covtype,nugget=1e-8*var(dat1$y))

# Now, predict what the GAM and GP look like everywhere in the sample space
pred_1_gam <- predict(gam_1,newdata=dat_all)
pred_1_gp <- predict(gp_1,newdata=as.matrix(dat_all$x),type="SK")
# Add the above two together to get the full prediction (LIKE MY GP ALREADY DOES)
dat_all$y <- pred_1_gp$mean + pred_1_gam
# adds uncertainty bounds to the prediction
qs1 <- as.numeric(dat_all$y) +cbind(qnorm(0.05,0,pred_1_gp$sd),0,qnorm(0.95,0,pred_1_gp$sd),qnorm(cert,0,pred_1_gp$sd))





##### add another num_round

# look at best scenario for each point in the round and see if it is better 
# than the best plus a small amount, likely for rounding errors
tmp <- which(qs1[,4] > best1 + 1e-8)
# picked five evenly spaced points from the potential points
x2 <- dat_all$x[tmp[floor(seq(1,length(tmp),length.out=num_round))]]
# get the objective function values at these points
y2 <- y_fun(x2)

#points(x2,y2,pch=18)
# add these new points to the existing data
dat2 <- rbind(dat1,data.frame(x=x2,y=y2))
# record the best value seen so far, including in the new data
best2 <- max(dat2$y)

#### 2

# do predictions for after round 2
# identical to in round 1, just use all 10 points we have sampled now
gam_2 <- gam(y~x,data=dat2)
gp_2 <- km(~1,design=as.matrix(dat2$x),estim.method="MLE",response = gam_2$residuals,covtype = covtype,nugget=1e-8*var(dat2$y))

pred_2_gam <- predict(gam_2,newdata=dat_all)
pred_2_gp <- predict(gp_2,newdata=as.matrix(dat_all$x),type="SK")
dat_all$y <- pred_2_gp$mean + pred_2_gam
qs2 <- as.numeric(dat_all$y) +cbind(qnorm(0.05,0,pred_2_gp$sd),0,qnorm(0.95,0,pred_2_gp$sd),qnorm(cert,0,pred_2_gp$sd))


##### add another num_round
# do round 3: get points, sample and add to the data
tmp <- which(qs2[,4] > best2+1e-5)
x3 <- dat_all$x[tmp[floor(seq(1,length(tmp),length.out=num_round))]]
y3 <- y_fun(x3)

dat3 <- rbind(dat2,data.frame(x=x3,y=y3))


## PLOTTING

# Set the size of the dots to 1.3 time the default
# Defined at the top to make it easier to change for all plots at once
cex_arg <- 1.3
# Loads library that can save plots to files
library(grDevices)
# Setting up for creating EPS files which are high quality vector images
# This is a standard for scientific papers so I should consider using this instead when getting my final images
setEPS()
# creates and opens a high quality eps file, presumably to store the plot in
cairo_ps("demo_GP.eps",width=5.5,height=5)
#create a grid of four plots
par(mfrow=c(2,2))
# create some margin space around the plots
par(oma=c(2,2,1,1))
# create some space between the plots
par(mar=c(3,3,0,0))

# ROUND 1 PLOT
# set up axes, scale and the box so we can then add layers on top (Kinda like ggplot)
plot(dat_all$x,qs1[,2],type="n",ylim=range(qs1),xlab="",ylab="")
# add our intial five points as dots
points(dat1$x,dat1$y,pch=16,cex=cex_arg)
# likely draws on the shaded area and mean line, again hard to tell due to custom package
SpenceTools::uncertain_plot(dat_all$x,t(qs1[,1:3]),add=T)
# add the 99.99% line as a dotted line
lines(dat_all$x,qs1[,4],lwd=2,lty=3)
# add a horizontal dashed line at the best value seen so far
abline(h=best1,col="black",lty=2)


# TRANSITION BETWEEN 1 AND 2

# Set up axes, show only areas with a chance of catch > best catch and then shows prediction versus what happened
plot(dat_all$x,qs2[,2],type="n",ylim=range(c(qs1[which(dat_all$x %in% x2),],y2)),xlim=range(dat_all$x[which(qs1[,4] > best1)]),xlab="",ylab="")
#points(dat2$x,dat2$y,pch=16)
# Draw the old mdoel to show why we looked where we did in the above
SpenceTools::uncertain_plot(dat_all$x,t(qs1[,1:3]),add=T)
lines(dat_all$x,qs1[,4],lwd=2,lty=3)
# Fills in the old data
points(dat1$x,dat1$y,pch=16,cex=cex_arg,col="black")
abline(h=best1,col="black",cex=cex_arg,lty=2)
# adds the new data points in red
points(x2,y2,pch=16,cex=cex_arg,col="red")


# ROUND 2 PLOT

# keep the view identical to the above plot
plot(dat_all$x,qs2[,2],type="n",ylim=range(c(qs1[which(dat_all$x %in% x2),],y2)),xlim=range(dat_all$x[which(qs1[,4] > best1)]),xlab="",ylab="")
#points(dat2$x,dat2$y,pch=16)
# add uncertainty plot for round 2
SpenceTools::uncertain_plot(dat_all$x,t(qs2[,1:3]),add=T,unc_col="red")
# add the new 99.99% bound line
lines(dat_all$x,qs2[,4],lwd=2,lty=3,col="red")
# keep the original 5 data points in black
points(dat1$x,dat1$y,pch=16,cex=cex_arg,col="black")
# add the horizontal line at the best value seen so far
abline(h=best2,col="red",cex=cex_arg,lty=2)
# add the new data points in red
points(x2,y2,pch=16,cex=cex_arg,col="red")


# TRANSITION BETWEEN 2 AND 3

# set up axes, show only areas with a chance of catch > best catch and then shows prediction versus what happened
plot(dat_all$x,qs2[,2],type="n",ylim=range(c(qs2[which(dat_all$x %in% x3),],y3)),xlim=range(dat_all$x[which(qs2[,4] >= best2-1e-8)]),xlab="",ylab="")
#points(dat3$x,dat3$y,pch=16)
# Draw the old mdoel to show why we looked where we did in the above
SpenceTools::uncertain_plot(dat_all$x,t(qs2[,1:3]),add=T,unc_col="red")
lines(dat_all$x,qs2[,4],lwd=2,lty=3,col="red")
abline(h=best2,col="red",lty=2)
# Fills in the old data
points(dat1$x,dat1$y,pch=16,cex=cex_arg,col="black")
points(x2,y2,pch=16,cex=cex_arg,col="red")
# adds the new points for round 3 in blue
points(x3,y3,pch=16,cex=cex_arg,col="blue")

# ADD LABELS AND SAVE

# Adds the label theta to the x-axis (except with theta as a symbol)
mtext(expression(theta),1,outer=T,line=0)
# Adds the label f(theta) to the y axis (except with theta as a symbol)
mtext(expression(paste(f(theta))),2,outer=T,line=0)
# saves and closes the file
dev.off()



# #### MAPS FROM RUNS AND HEATMAPS

# # set up eps file
# setEPS()
# # gives eps file anme and dimensions
# cairo_ps("round1.eps",width=5.5,height=5)
# # create a grid of four plots
# par(mfrow=c(2,2))
# # set outer margins
# par(oma=c(0,0,0,0))
# # set plot margins for this plot
# par(mar=c(4,4.5,2,3))
# # shows a map of th Ftarget and Btrigger combinations that are possible
# plot(round1[,2:1],pch=16,main="a)",ylab=expression(F[target]),xlab=expression(B[trigger]))
# # plot ftarget versus catch
# plot(runs$Ftrgt[1:20],runs$catch_median_long[1:20],log="y",xlab=expression(F[target]),ylab="Median long-term catch",main="b)",pch=16)
# # plot ftarget versus risk
# plot(runs$Ftrgt[1:20],runs$risk1_full[1:20],xlab=expression(F[target]),ylab="Risk",main="c)",pch=16)
# # line shwoing risk < 0.05
# abline(h=0.05,col="black")

# # MAPS SAFE POINTS

# # take the predictions of the risk GP into matrix form
# tmp <- matrix(prisk1,nrow=11)
# # rounds up any values below 1e-12 to 1e-12 to avoid issues with log scale
# tmp1 <- ifelse(tmp < 1e-12,1e-12,tmp)
# # draw the risk heatmap
# image2D(tmp1,y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab=expression(B[trigger]),ylab=expression(F[target]),breaks=c(0,0.0001,0.05,0.5,0.9,1),main="b)")

# # MAPS POINTS WHERE CATCH COULD BE HIGHER THAN CURRENT BEST

# # takes the 1-pcat for every point into matrix form
# tmp <- matrix(1-pcat1,nrow=11)
# # rounds up any values below 1e-12 to 1e-12 to avoid issues with log scale
# tmp1 <- ifelse(tmp < 1e-12,1e-12,tmp)
# # draw the heatmap showing points where catch could be higher than current best
# image2D(tmp1,y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab=expression(B[trigger]),ylab=expression(F[target]),breaks=c(0,0.0001,0.05,0.5,0.9,1),main="c)")

# # MAPS WHERE IT IS BOTH SAFE AND WHERE CATCH COULD BE HIGHER THAN CURRENT BEST

# # takes the product of the two matrices above into matrix form
# tmp <- matrix(possible * (1-pcat1),nrow=11)
# # rounds up any values below 1e-12 to 1e-12 to avoid issues with log scale
# tmp1 <- ifelse(tmp < 1e-12,1e-12,tmp)
# # draw the heatmap showing points that are both safe and could have higher catch than current best
# image2D(tmp1,y=sort(unique(dat$Ftrgt)),x=sort(unique(dat$Btrigger)),xlab=expression(B[trigger]),ylab=expression(F[target]),breaks=c(0,0.0001,0.05,0.5,0.9,1),main="d)")

# # save and close file
# dev.off()
