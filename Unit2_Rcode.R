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


#first, define the VIF function
vif_func<-function(in_frame,thresh=10,trace=T,...){

  #library(fmsb)

  VIF <- function(X) { 1/(1-summary(X)$r.squared) }

  percentile <- function(dat) { # convert numeric vector into percentiles
   pt1 <- quantile(dat, probs=seq(0, 1, by=0.01), type=7) # set minimum 0 percentile.
   pt2 <- unique(as.data.frame(pt1), fromLast=TRUE)
   pt3 <- rownames(pt2)
   pt4 <- as.integer(strsplit(pt3, "%"))
   datp <- pt4[as.integer(cut(dat, c(0, pt2$pt1), labels=1:length(pt3)))]
   return(datp)
  }
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
      regressors <- var_names[-which(var_names == val)]
      form <- paste(regressors, collapse = '+')
      form_in <- formula(paste(val, '~', form))
      vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
      }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)

  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
        prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
        cat('\n')
        cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
        }
    return(var_names)
    }
  else{

    in_dat<-in_frame

    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
        
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
        }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]

      vif_max<-as.numeric(vif_vals[max_row,2])

      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
        }

      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]

      }

    return(names(in_dat))
    
    }
  
  }#end VIF function



# --------------------
# used car example
# --------------------
Price <- c(85,103,70,82,89,98,66,95,169,70,48)# response variable
Age <- c(5,4,6,5,5,5,6,6,2,7,7)# one explanatory variable
Miles <- c(57,40,77,60,49,47,58,39,8,69,89)# another explanatory variable
car.df <- data.frame("Price"=Price,"Age"=Age,"Miles"=Miles)

#first, look at correlation matrix
cor(car.df)

#next, look at scatterplot matrix
pairs(car.df)

#now, use the VIF function
vif_func(car.df[,-1])#using used car vars



# --------------------
# height example
# --------------------
Age <- c(3,5,2,3,7,6)
Inches <- c(16,24,15,14,28,30)
cm <- c(40.64,60.96,38.10,35.56,71.12,76.20)
# add a little noise
set.seed(2);cm <- c(40.64,60.96,38.10,35.56,71.12,76.20) + rnorm(6,0,.15)

ht.df <- data.frame("Age"=Age,"Inches"=Inches,"cm"=cm)

#first, look at correlation matrix
cor(ht.df)

#next, look at scatterplot matrix
pairs(ht.df)

#now, use the VIF function
vif_func(ht.df[,-1])#using height vars


# --------------------
# ames housing data set
# --------------------
#dat <- read.table("C:/Users/brookr/Desktop/AmesHousingComma.txt",header=TRUE,sep=",")
#X <- read.table("C:/Users/brookr/Desktop/AmesHousingComma.txt",header=TRUE,sep=",")
X <- read.table("AmesHousingComma.txt",header=TRUE,sep=",")
ames.dat <- na.omit(X[,c(82,6,21,56,20,45,51,5)])
pairs(ames.dat)
cor(ames.dat)
vif_func(ames.dat[,-1])


#polynomial regression
library(car)
data(CanPop)
attach(CanPop)
plot(c(1850,2100),c(0,100),type="n")
points(year,population)
m1 <- lm(population ~ year) #linear model
m2 <- lm(population ~ year + I(year^2)) #quadratic model
m3 <- lm(population ~ year + I(year^2) + I(year^3)) #cubic model
m4 <- lm(population ~ year + I(year^2) + I(year^3) + I(year^4) + I(year^5)
			+ I(year^6) + I(year^7) + I(year^8)) #8th degree model
abline(m1)
xseq <- seq(1850,2100,by=.1) #a sequence of x values for which we want predictions
lines(xseq,predict(m2,data.frame("year"=xseq)),col="red") #get predictions for m2
lines(xseq,predict(m3,data.frame("year"=xseq)),col="blue") #get predictions for m3
lines(xseq,predict(m4,data.frame("year"=xseq)),col="green") #get predictions for m3



#potential MC issue
plot(year,year^2)
cor(year,year^2)
vif(m2)

#standardize year
mean.x <- mean(year)
sd.x <- sd(year)
year.stdz <- (year - mean.x)/sd.x
m1.stdz <- lm(population ~ year.stdz)
m2.stdz <- lm(population ~ year.stdz + I(year.stdz^2))
plot(year.stdz,population)
abline(m1.stdz)
xseq.stdz <- seq(-1.6,1.6,by=.05)
lines(xseq.stdz,predict(m2.stdz,data.frame("year.stdz"=xseq.stdz))) #get predictions for m2.stdz
#say that we want to predict population in 1985
predict(m2.stdz,data.frame("year.stdz"=(1985 - mean.x)/sd.x)) #need to standardize x value before making prediction
predict(m2,data.frame("year"=1985)) #should be the same as above

#check for MC
plot(year.stdz,year.stdz^2)
cor(year.stdz,year.stdz^2)
vif(m2.stdz)


#comparing cubic vs quadratic using R^2, adjR^2, PRESS, AIC, and BIC
summary(m2)#R-squared:  0.9964,    Adjusted R-squared:  0.9959
#PRESS statistic
pr <- resid(m2)/(1 - lm.influence(m2)$hat);sum(pr^2)#6.574237
extractAIC(m2)#AIC = -14.84459
extractAIC(m2,k=log(length(year)))#BIC = -12.52683

summary(m3)#R-squared:  0.9965,    Adjusted R-squared:  0.9957
#PRESS statistic
pr <- resid(m3)/(1 - lm.influence(m3)$hat);sum(pr^2)#8.775978
extractAIC(m3)#AIC = -13.3222
extractAIC(m3,k=log(length(year)))#BIC = -2.901522

                                                                    23
detach(CanPop)






#example of model selection using simulated data
set.seed(11);pred.vars <- matrix(rnorm(100*10),nrow=100,ncol=10)
beta.vec <- c(.5,1.2,-1.1,.75,-.3)
set.seed(9);resp.var <- cbind(rep(1,100),pred.vars[,1:4]) %*% beta.vec + rnorm(100,0,.3)

round(cor(cbind(resp.var,pred.vars)),2)
pairs(cbind(resp.var,pred.vars))
#vif_func(data.frame(pred.vars))

#FIRST MODEL
m1 <- lm(resp.var ~ pred.vars[,1] + pred.vars[,2] + pred.vars[,7] + pred.vars[,8])
summary(m1)#R-squared:  0.7583,    Adjusted R-squared:  0.7481
#PRESS statistic
pr <- resid(m1)/(1 - lm.influence(m1)$hat);sum(pr^2)#76.20835
extractAIC(m1)#AIC = -29.33751
extractAIC(m1,k=log(length(resp.var)))#BIC = -16.31166

library(car)
stndres <- rstandard(m1)
par(mfrow=c(1,3));plot(m1$fit,stndres,main="Fitted vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m1$resid,main="ACF Plot of Residuals")

#some residual plots for m1
plot(pred.vars[,5],m1$resid,);abline(0,0,lty=2)
plot(pred.vars[,3],m1$resid,);abline(0,0,lty=2)#indicates that x3 should be added to the model


#SECOND MODEL
m2 <- lm(resp.var ~ pred.vars[,1] + pred.vars[,2] + pred.vars[,3])
summary(m2)#R-squared: 0.9374, Adjusted R-squared:  0.9355
#PRESS statistic
pr <- resid(m2)/(1 - lm.influence(m2)$hat);sum(pr^2)#18.8558
extractAIC(m2)#AIC = -166.4796
extractAIC(m2,k=log(length(resp.var)))#BIC = -156.0589

stndres <- rstandard(m2)
par(mfrow=c(1,3));plot(m2$fit,stndres,main="Fitted vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m2$resid,main="ACF Plot of Residuals")


#some residual plots for m2
plot(pred.vars[,6],m2$resid);abline(0,0,lty=2)
plot(pred.vars[,4],m2$resid);abline(0,0,lty=2)#indicates that x4 should be added to the model



#cross validation
set.seed(4);inds <- sample(rep(1:5,20))

# ---------------------
#5-fold CV for model 1
sse.vec.m1 <- rep(NA,5)
for (grp in 1:5){
rws.tst <- which(inds==grp,arr.ind=TRUE)
rws.trn <- which(inds!=grp,arr.ind=TRUE)

expl <- pred.vars[rws.trn,]
resp <- resp.var[rws.trn]
mod.out <- lm(resp ~ expl[,1] + expl[,2] + expl[,7] + expl[,8])
preds <- cbind(rep(1,20),pred.vars[rws.tst,c(1,2,7,8)]) %*% mod.out$coef
sse.vec.m1[grp] <- sum((resp.var[rws.tst] - preds)^2)
}
sum(sse.vec.m1)

# ---------------------
#5-fold CV for model 2
sse.vec.m2 <- rep(NA,5)
for (grp in 1:5){
rws.tst <- which(inds==grp,arr.ind=TRUE)
rws.trn <- which(inds!=grp,arr.ind=TRUE)

expl <- pred.vars[rws.trn,]
resp <- resp.var[rws.trn]
mod.out <- lm(resp ~ expl[,1] + expl[,2] + expl[,3])
preds <- cbind(rep(1,20),pred.vars[rws.tst,c(1,2,3)]) %*% mod.out$coef
sse.vec.m2[grp] <- sum((resp.var[rws.tst] - preds)^2)
}
sum(sse.vec.m2)


#PRESS statistic for both models
pr <- resid(m1)/(1 - lm.influence(m1)$hat);sum(pr^2)
pr <- resid(m2)/(1 - lm.influence(m2)$hat);sum(pr^2)


#stepwise method
library(MASS)
out <- stepAIC(m1, 
	scope = list(upper = ~pred.vars[,1] + pred.vars[,2] + pred.vars[,3] + pred.vars[,4] +
	pred.vars[,5] + pred.vars[,6] + pred.vars[,7] + pred.vars[,8] + pred.vars[,9] + 
	pred.vars[,10], lower = ~1),
	direction = "both")





#RB analysis
#use 40 speed as response
load("C:\\Users\\brookr\\Documents\\MATH8050flashDrive\\RB.RData")
load("\\\\home.clemson.edu\\brookr\\Downloads\\RB.RData")
rb <- rb[,-(1:4)]
round(cor(rb[,c(6,1:5,7:8)]),2)
pairs(rb[,c(6,1:5,7:8)])
#vif_func(rb[,-6])

attach(rb)
library("car")
#fit full model
m1 <- lm(FortySpeed ~ Height + Weight + Bench + Vert + BroadJump + ShuttleSpeed + ThreeConeSpeed)
stndres <- rstandard(m1)
par(mfrow=c(1,2));plot(m1$fit,stndres,main="Fitted vs Standardized Residuals");abline(0,0);qqPlot(stndres)

#fit a reduced model
m2 <- lm(FortySpeed ~ Weight + Bench + Vert + BroadJump)
stndres <- rstandard(m2)
par(mfrow=c(1,2));plot(m2$fit,stndres,main="Fitted vs Standardized Residuals");abline(0,0);qqPlot(stndres)

#test for reduced model
anova(m2,m1)

#get R^2 and adjusted R^2
summary(m1)
summary(m2)

#get AIC
extractAIC(m1)
extractAIC(m2)

extractAIC(m1,k=log(230))
extractAIC(m2,k=log(230))

#PRESS statistic for both models
pr <- resid(m1)/(1 - lm.influence(m1)$hat);sum(pr^2)
pr <- resid(m2)/(1 - lm.influence(m2)$hat);sum(pr^2)

#cross validation
set.seed(7);inds <- sample(rep(1:5,46))

pred.vars <- rb[,-6]
resp.var <- rb$FortySpeed

# ---------------------
#5-fold CV for model 1
sse.vec.m1 <- rep(NA,5)
for (grp in 1:5){
rws.tst <- which(inds==grp,arr.ind=TRUE)
rws.trn <- which(inds!=grp,arr.ind=TRUE)

expl <- pred.vars[rws.trn,]
resp <- resp.var[rws.trn]
mod.out <- lm(resp ~ expl[,1] + expl[,2] + expl[,3] + expl[,4] + expl[,5] + expl[,6] + expl[,7])
preds <- cbind(rep(1,46),as.matrix(pred.vars)[rws.tst,c(1:7)]) %*% mod.out$coef
sse.vec.m1[grp] <- sum((resp.var[rws.tst] - preds)^2)
}
sum(sse.vec.m1)

# ---------------------
#5-fold CV for model 2
sse.vec.m2 <- rep(NA,5)
for (grp in 1:5){
rws.tst <- which(inds==grp,arr.ind=TRUE)
rws.trn <- which(inds!=grp,arr.ind=TRUE)

expl <- pred.vars[rws.trn,]
resp <- resp.var[rws.trn]
mod.out <- lm(resp ~ expl[,2] + expl[,3] + expl[,4] + expl[,5])
preds <- cbind(rep(1,46),as.matrix(pred.vars)[rws.tst,c(2:5)]) %*% mod.out$coef
sse.vec.m2[grp] <- sum((resp.var[rws.tst] - preds)^2)
}
sum(sse.vec.m2)



#stepwise method
library(MASS)
out1 <- stepAIC(m1, 
	scope = list(upper = ~Height + Weight + Bench + Vert + BroadJump + ShuttleSpeed + ThreeConeSpeed, lower = ~1),
	direction = "both")

out2 <- stepAIC(m2, 
	scope = list(upper = ~Height + Weight + Bench + Vert + BroadJump + ShuttleSpeed + ThreeConeSpeed, lower = ~1),
	direction = "both")

m3 <- lm(FortySpeed ~ 1)
out3 <- stepAIC(m3, 
	scope = list(upper = ~Height + Weight + Bench + Vert + BroadJump + ShuttleSpeed + ThreeConeSpeed, lower = ~1),
	direction = "both")



influence.measures(out3)
stndres <- rstandard(out3)
par(mfrow=c(1,2));plot(out3$fit,stndres,main="Fitted vs Standardized Residuals");abline(0,0);qqPlot(stndres)

Heavy <- ifelse(Weight < 210, 0, 1)
m4 <- lm(FortySpeed ~ Heavy + Bench + Vert + BroadJump + ThreeConeSpeed)
summary(m4)
extractAIC(m4)

detach(rb)







#consider the standardized variables
rb.stndz <- data.frame(scale(rb,center=TRUE,scale=TRUE))

attach(rb.stndz)

m3.stndz <- lm(FortySpeed ~ Weight + Bench + Vert + BroadJump + ThreeConeSpeed)
summary(m3.stndz)

detach(rb.stndz)







#data for HW 3
Soil_pH <- c(3.3,3.4,3.4,3.5,3.6,3.6,3.7,3.7,3.8,3.8,3.9,4.0,4.1,4.2,4.3,4.4,4.5,5.0,5.1,5.2)
Growth_Ret <- c(17.78,21.59,23.84,15.13,23.45,20.87,17.78,20.09,17.78,12.46,14.95,15.87,17.45,14.35,14.64,17.25,12.57,7.15,7.50,4.34)



# --------------------
# ames housing data set
# --------------------
#dat <- read.table(file="/media/brookr/Win7/Users/brookr/Desktop/AmesHousingComma.txt",header=TRUE,sep=",")
#X <- read.table(file="/media/brookr/Win7/Users/brookr/Desktop/AmesHousingComma.txt",header=TRUE,sep=",")
#X <- read.table(file="D:/AmesHousingComma.txt",header=TRUE,sep=",")
#X <- read.table("C:/Users/brookr/Documents/MATH8050flashDrive/AmesHousingComma.txt",header=TRUE,sep=",")
X <- read.table("AmesHousingComma.txt",header=TRUE,sep=",")
ames.dat <- na.omit(X[,c(82,6,21,56,20,45,51,5)])
names(ames.dat)
pairs(ames.dat)

attach(ames.dat)
hist(Full.Bath)


plot(X1st.Flr.SF,SalePrice)
m1 <- lm(SalePrice ~ X1st.Flr.SF)
abline(m1)

plot(X1st.Flr.SF,m1$resid)
abline(0,0)

numBRs <- ifelse(Full.Bath <= 1,0,1)
cbind(Full.Bath,numBRs)

m2 <- lm(SalePrice ~ X1st.Flr.SF + numBRs)
m4 <- lm(SalePrice ~ X1st.Flr.SF + factor(Full.Bath))
m5 <- lm(SalePrice ~ X1st.Flr.SF + Full.Bath)
plot(X1st.Flr.SF,SalePrice,type="n")
points(X1st.Flr.SF[numBRs==0],SalePrice[numBRs==0],col="red")
points(X1st.Flr.SF[numBRs==1],SalePrice[numBRs==1],col="blue")
abline(m2$coef[1],m2$coef[2],col="red")
abline(m2$coef[1] + m2$coef[3],m2$coef[2],col="blue")

m3 <- lm(SalePrice ~ X1st.Flr.SF + numBRs + X1st.Flr.SF:numBRs)
m3 <- lm(SalePrice ~ X1st.Flr.SF*numBRs)
plot(X1st.Flr.SF,SalePrice,type="n")
points(X1st.Flr.SF[numBRs==0],SalePrice[numBRs==0],col="red")
points(X1st.Flr.SF[numBRs==1],SalePrice[numBRs==1],col="blue")
abline(m3$coef[1],m3$coef[2],col="red",lty=2)
abline(m3$coef[1] + m3$coef[3],m3$coef[2] + m3$coef[4],col="blue",lty=2)

anova(m1,m3)

detach(ames.dat)

detach(ames.dat)



# penalized regression
library(glmnet)
library(mvtnorm)

sig.mat <- diag(rep(1,12))
sig.mat[1,2]<-sig.mat[2,1]<-.8
sig.mat[7,8]<-sig.mat[8,7]<-.85
sig.mat[7,9]<-sig.mat[9,7]<-.75
sig.mat[8,9]<-sig.mat[9,8]<-.9

set.seed(6);x.vars <- rmvnorm(150,mean=rep(0,12),sigma=sig.mat)

#first model
#true beta vector
beta.vec <- c(1.5,.3,-.5,.1,.1,-.15,0,.2,0,.1,0,0,0)
set.seed(11);y <- cbind(rep(1,150),x.vars) %*% beta.vec + rnorm(150,0,.15)

cv.out <- cv.glmnet(x.vars,c(y),gamma=1,alpha=0)
plot(cv.out)
print(cv.out)
cbind(coef(cv.out,c(cv.out$lambda.min,cv.out$lambda.1se)),beta.vec)


cv.out <- cv.glmnet(x.vars,c(y),gamma=1,alpha=1)
plot(cv.out)
print(cv.out)
cbind(coef(cv.out,c(cv.out$lambda.min,cv.out$lambda.1se)),beta.vec)


cv.out <- cv.glmnet(x.vars,c(y),gamma=1,alpha=.5)
plot(cv.out)
print(cv.out)
cbind(coef(cv.out,c(cv.out$lambda.min,cv.out$lambda.1se)),beta.vec)


cv.out <- cv.glmnet(x.vars,c(y),gamma=1,alpha=0)
cv.out <- cv.glmnet(x.vars,c(y),gamma=1,alpha=1,
   penalty.factor= abs(1/c(matrix(coef(cv.out,c(cv.out$lambda.min))))[-1]))
plot(cv.out)
print(cv.out)
cbind(coef(cv.out,c(cv.out$lambda.min,cv.out$lambda.1se)),beta.vec)



lm.out <- lm(c(y) ~ x.vars)
cbind(lm.out$coefficients,beta.vec)

# alpha = 1 ===> LASSO
# alpha = 0 ===> Ridge

