cor_val <- .2;sig_mat <- matrix(c(1,cor_val,cor_val,1),2,2);out <- mvrnorm(10,mu=c(0,0),Sigma=sig_mat);plot(out);cor(out)

x <- c(-2,-1,0,1,2)
y <- x^2
plot(x,y)
cor(x,y)

co2 <- c(314,317,320,326,331,339,346,354,361,369)
temp <- c(13.9,14,13.9,14.1,14,14.3,14.1,14.5,14.5,14.4)
plot(co2,temp)
cor(co2,temp)
.89*sqrt((10-2)/(1-.89^2))
qt(.95,8)
1 - pt(5.52,8)
cor.test(co2,temp,alt="greater")


library(MASS)
cor_val <- .2;sig_mat <- matrix(c(1,cor_val,cor_val,1),2,2);out <- mvrnorm(10,mu=c(0,0),Sigma=sig_mat);plot(out);cor(out)
sig_mat_new <- rbind(cbind(sig_mat,0,0,0),0,0,0)
sig_mat_new[3,3] <- sig_mat_new[4,4] <- sig_mat_new[5,5] <- 1
sig_mat_new
out <- mvrnorm(10,mu=c(0,0,0,0,0),Sigma=sig_mat_new)
out
cor(out)
out <- mvrnorm(100,mu=c(0,0,0,0,0),Sigma=sig_mat_new)
out
cor(out)

#
plot(co2,temp,xlab="CO2",ylab="Temperature",main="CO2 vs Temperature",pch=16,col="green")
m_slr <- lm(temp ~ co2)
abline(m_slr)
anova(m_slr)
10.483 + .011*360

#
MSE <- .0128
Sxx <- sum((co2 - mean(co2))^2)
stderr <- sqrt(MSE/Sxx)
tstar <- (.0109 - 0)/stderr
1 - pt(tstar,10-2)
summary(m_slr)

predict(m_slr,newdata=data.frame("co2"=c(330,350)))
0.010918 - qt(.975,8)*sqrt(MSE/Sxx)
0.010918 + qt(.975,8)*sqrt(MSE/Sxx)
confint(m_slr)
tstar <- (10.483081 - 8)/0.661635
tstar
1 - pt(tstar,8)
10.483081 - qt(.975,8)*  0.661635
10.483081 + qt(.975,8)*  0.661635
confint(m_slr)

qt(.975,8)
#2.306004
MSE <- .0128
Sxx <- sum((co2 - mean(co2))^2)
n<-10
xbar <- mean(co2)
SE1 <- sqrt(MSE * (1/n + (350 - xbar)^2/Sxx))
SE2 <- sqrt(MSE * (1 + 1/n + (350 - xbar)^2/Sxx))
14.3 - 2.306 * SE1
14.3 + 2.306 * SE1
14.3 - 2.306 * SE2
14.3 + 2.306 * SE2
predict(m_slr,newdata=data.frame("co2"=c(350)),interval = c("prediction"))
predict(m_slr,newdata=data.frame("co2"=c(350)),interval = c("confidence"))

plot(co2,temp)
m_slr <- lm(temp ~ co2)
abline(m_slr)
MSE <- .0128
Sxx <- sum((co2 - mean(co2))^2)
n <- 10
x_seq <- seq(310,370,length=100)
SE_confBand <- sqrt(MSE * (1/n + (x_seq - mean(co2))^2/Sxx))
W <- sqrt(2 * qf(.95,2,8))
uppr_band <- (10.48308 + x_seq*0.01092) + W * SE_confBand
lwr_band <- (10.48308 + x_seq*0.01092) - W * SE_confBand
lines(x_seq,uppr_band,lty=2)
lines(x_seq,lwr_band,lty=2)

plot(co2,m_slr$residuals)
abline(h=0)
library(car)
qqPlot(m_slr$residuals)

# -----------------------
# simulated SLR example
# -----------------------
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y <- 30 + 2*x + rnorm(50,0,17.5) #randomly generate some y values
plot(x,y,pch=17,col="blue",main="Scatterplot of x versus y") #create the scatterplot
cor.test(x,y) #test whether rho != 0
m1 <- lm(y~x) #fit SLR model
abline(m1,col="red",lty=2) #add SLR line to scatterplot
#legend(.15,83,legend="Least Squares Line",col="red",lty=2)
abline(30,2)
legend(.15,83,legend=c("Least Squares Line","True Line"),
	col=c("red","black"),lty=2:1)
summary(m1) #get summary of SLR model
anova(m1)

#inference E(Y_h) for X_h =6
Xh <- 6
Yhhat <- m1$coef[1] + m1$coef[2]*Xh
Yhhat
anova(m1)
MSE <- 250.58# from anova table
Sxx <- sum((x - mean(x))^2)
SYhhat <- sqrt(MSE*(1/50 + (Xh - mean(x))^2/Sxx))
#95% ci for Yhhat
Yhhat - qt(.975,50-2)*SYhhat
Yhhat + qt(.975,50-2)*SYhhat


#predict new response for X_h =6
Xh <- 6
Yhnew <- m1$coef[1] + m1$coef[2]*Xh
Yhnew
MSE <- 250.58# from anova table
Sxx <- sum((x - mean(x))^2)
SYhnew <- sqrt(MSE*(1 + 1/50 + (Xh - mean(x))^2/Sxx))
#95% ci for Yhhat
Yhnew - qt(.975,50-2)*SYhnew
Yhnew + qt(.975,50-2)*SYhnew


#confidence bands for regression line
xseq <- seq(0,10,by=.1)
yhat <- m1$coef[1] + m1$coef[2]*xseq
SYhhat <- sqrt(MSE*(1/50 + (xseq - mean(x))^2/Sxx))
W <- sqrt(2*qf(.95,2,50-2))
lwr.band <- yhat - W*SYhhat
upr.band <- yhat + W*SYhhat
plot(x,y,pch=17,col="blue",main="Scatterplot of x versus y") #create the scatterplot
abline(m1,col="red",lty=1) #add SLR line to scatterplot
lines(xseq,lwr.band,col="purple",lty=3)
lines(xseq,upr.band,col="purple",lty=3)
legend(.15,83,legend=c("Least Squares Line","95% Conf. Bands"),col=c("red","purple"),lty=c(1,3))

#diagnostic plots
# 1) use above data
par(mfrow=c(2,2));plot(x,y,main="Scatterplot of X vs Y");abline(m1);plot(x,m1$resid,main="X vs Raw Residuals");abline(0,0);qqnorm(m1$resid);qqline(m1$resid);acf(m1$resid,main="ACF Plot of Residuals")
stndres <- rstandard(m1)
#install.packages("car")
library(car)
par(mfrow=c(2,2));plot(x,y,main="Scatterplot of X vs Y");abline(m1);plot(x,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m1$resid,main="ACF Plot of Residuals")
abs(dffits(m1))
abs(dfbetas(m1))
cooks.distance(m1);max(cooks.distance(m1));qf(.5,1,49)
hatvalues(m1)#leverage values
influence.measures(m1)

# 2) use a different data set (autocorrelation)
rho <- 0.5;sig <- 17.5;sig.mat <- diag(50)
sig.mat <- sig * rho^abs(row(sig.mat)-col(sig.mat))
library(mvtnorm)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y2 <- 30 + 2*x + c(rmvnorm(1,sigma=sig.mat))
plot(x,y2)
m2 <- lm(y2~x)
summary(m2)
par(mfrow=c(2,2));plot(x,y2,main="Scatterplot of X vs Y");abline(m2);plot(x,m2$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m2$resid);acf(m2$resid,main="ACF Plot of Residuals")
par(mfrow=c(2,2));plot(x,y2,main="Scatterplot of X vs Y");abline(m2);plot(1:50,m2$resid,main="Time vs Raw Residuals");abline(0,0);qqPlot(m2$resid);acf(m2$resid,main="ACF Plot of Residuals")
influence.measures(m2)

# 3) use another data set (heteroscedacity)
set.seed(11) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y3 <- 30 + 2*x + c(rmvnorm(1,sigma=diag(3*x^2)))
plot(x,y3)
m3 <- lm(y3~x)
summary(m3)
par(mfrow=c(2,2));plot(x,y3,main="Scatterplot of X vs Y");abline(m3);plot(x,m3$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m3$resid);acf(m3$resid,main="ACF Plot of Residuals")
influence.measures(m3)

# 4) use another data set (curvature)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y4 <- 30 + 2*x + 4*x^2 + rnorm(50,0,17.5)
plot(x,y4)
m4 <- lm(y4~x)
summary(m4)
par(mfrow=c(2,2));plot(x,y4,main="Scatterplot of X vs Y");abline(m4);plot(x,m4$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m4$resid);acf(m4$resid,main="ACF Plot of Residuals")
influence.measures(m4)


# 5) one more data set (outliers)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y5 <- 30 + 2*x + rnorm(50,0,5) #randomly generate some y values
m5 <- lm(y5~x) #fit SLR model
stndres <- rstandard(m5)
summary(m5) #get summary of SLR model
par(mfrow=c(2,2));plot(x,y5,main="Scatterplot of X vs Y");abline(m5);plot(x,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m5$resid,main="ACF Plot of Residuals")
influence.measures(m5)
#add outlier at 20,70
newy <- c(y5,70);newx <- c(x,20)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)
plot(newx,newy,type="n");text(newx,newy,1:51,cex=.6)
#add outlier at 20,20
newy <- c(y5,20);newx <- c(x,20)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)
#add outlier at 5,70
newy <- c(y5,70);newx <- c(x,5)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)


#simulation example to introduce multiple regression
set.seed(3)
x1 <- runif(50,0,10)
x2 <- runif(50,0,10)
x3 <- runif(50,0,10)
y <- 5 + 3*x1 - 5*x2 + 5*x3 + rnorm(50,0,1)
x4 <- (x1 + x2)/2
x5 <- runif(50,0,10)
x6 <- runif(50,0,10)

#fit the model with x1, x2, x3, and x4 'by hand'
X <- cbind(rep(1,50),x1,x2,x3,x4)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
#drop x4
X <- cbind(rep(1,50),x1,x2,x3)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat

#what if the columns of X aren't *exactly* linearly dependent???
set.seed(6)
x4 <- (x1 + x2)/2 + rnorm(50,0,.025)
X <- cbind(rep(1,50),x1,x2,x3,x4)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat
set.seed(11)
x4 <- (x1 + x2)/2 + rnorm(50,0,.025)
X <- cbind(rep(1,50),x1,x2,x3,x4)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat
set.seed(15)
x4 <- (x1 + x2)/2 + rnorm(50,0,.025)
X <- cbind(rep(1,50),x1,x2,x3,x4)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat


#the lm function will drop x4 if vars are perfectly linearly dependent
x4 <- (x1 + x2)/2
m1 <- lm(y ~ x1 + x2 + x3 + x4)
#doesn't drop x4 here
set.seed(15)
x4 <- (x1 + x2)/2 + rnorm(50,0,.025)
m1 <- lm(y ~ x1 + x2 + x3 + x4)
#drop x4 manually, should be the same as the first lm fit
x4 <- (x1 + x2)/2
m1 <- lm(y ~ x1 + x2 + x3)

#check assumptions for m1
library(car)
std.res <- rstandard(m1)
par(mfrow=c(1,2));plot(m1$fitted.values,std.res);abline(0,0);qqPlot(m1$residuals)


#what if we leave out x3
m2 <- lm(y ~ x1 + x2)
std.res <- rstandard(m2)
par(mfrow=c(1,2));plot(m2$fitted.values,std.res);abline(0,0);qqPlot(m2$residuals)
par(mfrow=c(1,2))
 plot(x3,m2$residuals);abline(0,0)#the pattern here suggests that x3 is important!
 plot(x5,m2$residuals);abline(0,0)#the lack of pattern here suggests that x5 is not important!

 
#model comparison
m3 <- lm(y ~ x1 + x2 + x3 + x5)#
std.res <- rstandard(m3)
par(mfrow=c(1,2));plot(m3$fitted.values,std.res);abline(0,0);qqPlot(m3$residuals)
summary(m3)

X <- cbind(rep(1,50),x1,x2,x3,x5)
cov.mat.b <- .6 * solve(t(X) %*% X)
sqrt(diag(cov.mat.b))


anova(m3,m1)#F test for sub-model





#data for HW 3
Soil_pH <- c(3.3,3.4,3.4,3.5,3.6,3.6,3.7,3.7,3.8,3.8,3.9,4.0,4.1,4.2,4.3,4.4,4.5,5.0,5.1,5.2)
Growth_Ret <- c(17.78,21.59,23.84,15.13,23.45,20.87,17.78,20.09,17.78,12.46,14.95,15.87,17.45,14.35,14.64,17.25,12.57,7.15,7.50,4.34)
