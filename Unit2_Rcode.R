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



