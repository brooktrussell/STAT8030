salaries <- c(38.0, 36.0, 35.0, 27.0, 15.0, 13.0, 12.0, 10.0,  9.6,  8.4)

mean(salaries)
var(salaries)
sd(salaries)

t.test(salaries)#gives two sided 95% CI

t.test(salaries,mu=10,alternative="greater",conf.level=.99)

hist(salaries)
qqnorm(salaries);qqline(salaries)

#install.packages("car")
library(car)

qqPlot(salaries)


#Non-parametric bootstrapping
set.seed(1)
sample(salaries,rep=TRUE)

B <- 10000
boot_vec <- rep(NA,B)

for (b in 1:B){
 boot_vec[b] <- mean(sample(salaries,rep=TRUE))
}

#95% CI based on percentile method
quantile(boot_vec,c(.025,.975))

library(boot)
boot_out <- boot(salaries,function(dat,inds){mean(dat[inds])},R=10000)
boot.ci(boot_out)
