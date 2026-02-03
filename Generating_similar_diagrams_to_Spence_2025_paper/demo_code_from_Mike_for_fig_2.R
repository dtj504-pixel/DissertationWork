# demonstrate a Gaussian process
##### plots

library(DiceKriging)
library(mgcv)
dat_all <- data.frame(x=seq(0,2,0.01),y=0)
y_fun <- function(x){-2*x^2 + 3*x + 2}

cert <- 0.9999

num_round <- 5
xs <- SpenceTools::specific_floor(seq(0.2,1.8,length.out=num_round),digits=2)
dat1 <- data.frame(x=xs,y=y_fun(xs))
best1 <- max(dat1$y)
gp_1 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "matern3_2",nugget=1e-8*var(dat1$y))


pred_1_gp <- predict(gp_1,newdata=as.matrix(dat_all$x),type="SK")
dat_all$y <- pred_1_gp$mean
qs1 <- as.numeric(dat_all$y) +cbind(qnorm(0.05,0,pred_1_gp$sd),0,qnorm(0.95,0,pred_1_gp$sd),qnorm(cert,0,pred_1_gp$sd))


##### simulate a 4 of possible runs
nsim <- 4
sim_val <- simulate(gp_1,nsim = nsim,newdata=as.matrix(dat_all$x),cond=T)

exclude <-dat_all$x[which(qs1[,4]<(best1 - 1e-8))]

tmp <- which(qs1[,4] > best1 + 1e-8)
x2 <- dat_all$x[tmp[floor(seq(1,length(tmp),length.out=num_round))]]
y2 <- y_fun(x2)

#points(x2,y2,pch=18)
dat2 <- rbind(dat1,data.frame(x=x2,y=y2))
best2 <- max(dat2$y)
gp_2 <- km(~1,design=as.matrix(dat2$x),estim.method="MLE",response = dat2$y,covtype = "matern3_2",nugget=1e-8*var(dat2$y))

dat_all2 <- dat_all

pred_2_gp <- predict(gp_2,newdata=as.matrix(dat_all2$x),type="SK")
dat_all2$y <- pred_2_gp$mean
qs2 <- as.numeric(dat_all2$y) +cbind(qnorm(0.05,0,pred_2_gp$sd),0,qnorm(0.95,0,pred_2_gp$sd),qnorm(cert,0,pred_2_gp$sd))

exclude2 <-dat_all2$x[which(qs2[,4]<(best2 - 1e-8))]

tmp <- which(qs2[,4] > best2 + 1e-8)

x3 <- dat_all$x[tmp[floor(seq(1,length(tmp),length.out=num_round))]]
y3 <- y_fun(x3)

#points(x2,y2,pch=18)
dat3 <- rbind(dat2,data.frame(x=x3,y=y3))
best3 <- max(dat3$y)
gp_3 <- km(~1,design=as.matrix(dat3$x),estim.method="MLE",response = dat3$y,covtype = "matern3_2",nugget=1e-8*var(dat3$y))

dat_all3 <- dat_all

pred_3_gp <- predict(gp_3,newdata=as.matrix(dat_all3$x),type="SK")
dat_all3$y <- pred_3_gp$mean
qs3 <- as.numeric(dat_all3$y) +cbind(qnorm(0.05,0,pred_3_gp$sd),0,qnorm(0.95,0,pred_3_gp$sd),qnorm(cert,0,pred_3_gp$sd))

exclude3 <-dat_all2$x[which(qs3[,4]<(best3 - 1e-8))]

library(grDevices)
cairo_ps("toy_1.eps",width=5.5,height=5)
par(mfrow=c(2,2))
par(oma=c(2,2,1,1))
par(mar=c(3,3,0,0))
cex_arg <- 1.3
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),y_fun(0.75)),pch=16,xaxs="i",xlab="",ylab="",cex=cex_arg) ## the output of the
text(0.1,y_fun(0.75),labels ="a)")
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,xlab="",ylab="",cex=cex_arg)
for(i in 1:nsim){
lines(dat_all$x,sim_val[i,])
}
text(0.1,max(qs1),labels ="b)")

plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,xlab="",ylab="",cex=cex_arg)
SpenceTools::uncertain_plot(dat_all$x,t(qs1[,1:3]),add=T,lwd = 1,lty=2)
#lines(dat_all$x,qs1[,1],lty=3)
lines(dat_all$x,qs1[,4],lty=3)
abline(h=best_so_far)
points(exclude,rep(0,length(exclude)),pch=4)
text(0.1,max(qs1),labels ="c)")

plot(dat2$x,dat2$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,xlab="",ylab="",cex=cex_arg)
SpenceTools::uncertain_plot(dat_all2$x,t(qs2[,1:3]),add=T,lwd = 1,lty=2)
#lines(dat_all$x,qs1[,1],lty=3)
lines(dat_all$x,qs2[,4],lty=3)
abline(h=best2)
points(exclude2,rep(0,length(exclude2)),pch=4)
text(0.1,max(qs1),labels ="d)")

# plot(dat3$x,dat3$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,cex=cex_arg)
# SpenceTools::uncertain_plot(dat_all3$x,t(qs3[,1:3]),add=T,lwd = 1,lty=2)
# #lines(dat_all$x,qs1[,1],lty=3)
# lines(dat_all$x,qs3[,4],lty=3)
# abline(h=best3)
# points(exclude3,rep(0,length(exclude3)),pch=4)
# text(0.1,max(qs1),labels ="e)")
# 
# plot(dat3$x,dat3$y,xlim=c(0.6,0.9),ylim=c(y_fun(0.9),max(qs3)),xaxs="i",pch=16,cex=cex_arg)
# SpenceTools::uncertain_plot(dat_all3$x,t(qs3[,1:3]),add=T,lwd = 1,lty=2)
# #lines(dat_all$x,qs1[,1],lty=3)
# lines(dat_all$x,qs3[,4],lty=3)
# abline(h=best3)
# points(exclude3,rep(y_fun(0.9),length(exclude3)),pch=4)
# axis(1,at = seq(0.6,0.9,0.01),labels=rep("",length(seq(0.6,0.9,0.01))))
# text(0.62,max(qs3),labels ="f)")

mtext(expression(theta),1,outer=T,line=0)
mtext(expression(paste(f(theta))),2,outer=T,line=0)
dev.off()


#### comparing GP kernels
num_round <- 5
xs <- SpenceTools::specific_floor(seq(0.2,1.8,length.out=num_round),digits=2)
dat1 <- data.frame(x=xs,y=y_fun(xs))
gpd_1 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "gauss",nugget=1e-8*var(dat1$y))
gpd_2 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "matern5_2",nugget=1e-8*var(dat1$y))
gpd_3 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "matern3_2",nugget=1e-8*var(dat1$y))
gpd_4 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "powexp",nugget=1e-8*var(dat1$y))
gpd_5 <- km(~1,design=as.matrix(dat1$x),estim.method="MLE",response = dat1$y,covtype = "exp",nugget=1e-8*var(dat1$y))



predd_1_gp <- predict(gpd_1,newdata=as.matrix(dat_all$x),type="SK")
datd_all$y1 <- predd_1_gp$mean
qsd1 <- as.numeric(datd_all$y1) +cbind(qnorm(0.05,0,predd_1_gp$sd),0,qnorm(0.95,0,predd_1_gp$sd),qnorm(cert,0,predd_1_gp$sd))
###
predd_2_gp <- predict(gpd_2,newdata=as.matrix(dat_all$x),type="SK")
datd_all$y2 <- predd_2_gp$mean
qsd2 <- as.numeric(datd_all$y2) +cbind(qnorm(0.05,0,predd_2_gp$sd),0,qnorm(0.95,0,predd_2_gp$sd),qnorm(cert,0,predd_2_gp$sd))
###
predd_3_gp <- predict(gpd_3,newdata=as.matrix(dat_all$x),type="SK")
datd_all$y3 <- predd_3_gp$mean
qsd3 <- as.numeric(datd_all$y3) +cbind(qnorm(0.05,0,predd_3_gp$sd),0,qnorm(0.95,0,predd_3_gp$sd),qnorm(cert,0,predd_3_gp$sd))
###
predd_4_gp <- predict(gpd_4,newdata=as.matrix(dat_all$x),type="SK")
datd_all$y4 <- predd_4_gp$mean
qsd4 <- as.numeric(datd_all$y4) +cbind(qnorm(0.05,0,predd_4_gp$sd),0,qnorm(0.95,0,predd_4_gp$sd),qnorm(cert,0,predd_4_gp$sd))
###
predd_5_gp <- predict(gpd_5,newdata=as.matrix(dat_all$x),type="SK")
datd_all$y5 <- predd_5_gp$mean
qsd5 <- as.numeric(datd_all$y5) +cbind(qnorm(0.05,0,predd_5_gp$sd),0,qnorm(0.95,0,predd_5_gp$sd),qnorm(cert,0,predd_5_gp$sd))

cairo_ps("diff_kernel.eps",width=5.5,height=5)
par(mfrow=c(3,2))
par(oma=c(2,2,1,1))
par(mar=c(3,3,2,0))
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,main="gauss",xlab="",ylab="")
SpenceTools::uncertain_plot(dat_all$x,t(qsd1[,1:3]),add=T,lwd = 1,lty=2)
#
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,main="Matern5_2",xlab="",ylab="")
SpenceTools::uncertain_plot(dat_all$x,t(qsd2[,1:3]),add=T,lwd = 1,lty=2)
#
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,main="Matern3_2",xlab="",ylab="")
SpenceTools::uncertain_plot(dat_all$x,t(qsd3[,1:3]),add=T,lwd = 1,lty=2)
#
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,main="powexp",xlab="",ylab="")
SpenceTools::uncertain_plot(dat_all$x,t(qsd4[,1:3]),add=T,lwd = 1,lty=2)
#
plot(dat1$x,dat1$y,xlim=c(0,2),ylim=c(y_fun(2),max(qs1)),xaxs="i",pch=16,main="exp",xlab="",ylab="")
SpenceTools::uncertain_plot(dat_all$x,t(qsd5[,1:3]),add=T,lwd = 1,lty=2)
mtext(expression(theta),1,outer=T,line=0)
mtext(expression(paste(f(theta))),2,outer=T,line=0)
dev.off()
