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



#Miss America problem
recent.grp <- c(19.5,20.3,19.6,20.2,17.8,17.9,19.1,18.8,17.6,16.8)
past.grp <- c(20.4,21.9,22.1,22.3,20.3,18.8,18.9,19.4,18.4,19.1)

mean(recent.grp);sd(recent.grp)
mean(past.grp);sd(past.grp)

#test on variances, then pooled or non-pooled test
var.test(recent.grp,past.grp)#FTR
t.test(recent.grp,past.grp,alt="less",var.equal=TRUE)

#permutation test on difference of means
choose(20,10)#number of possible permutations
new.dat <- c(recent.grp,past.grp)
obs.mean.diff <- mean(recent.grp) - mean(past.grp)
nsim <- 100000
sim.mean.diff <- rep(NA,length=nsim)
for (i in 1:nsim){
  grps <- sample(c(rep(1,10),rep(2,10)),replace=FALSE)
  sim.mean.diff[i] <- mean(new.dat[grps==1]) - mean(new.dat[grps==2])
}

hist(sim.mean.diff);abline(v=obs.mean.diff,col="red",lty=2)
length(sim.mean.diff[sim.mean.diff<=obs.mean.diff])/nsim #estimated p-value


#10 remote controlled cars were each run with battery brand A and B
#and the time each car ran was recorded (in hours)
#Is there a difference in average length of time different for the brands?
battery_A <- c(4.1, 4.6, 3.2, 4.9, 4.1, 4.0, 3.7, 4.3, 3.9, 4.0)
battery_B <- c(3.6, 4.6, 3.5, 3.0, 4.4, 4.1, 3.6, 3.2, 4.3, 3.0)

diffs <- battery_A - battery_B
cbind(battery_A,battery_B,diffs)

t.test(diffs)

t.test(battery_A,battery_B,paired=TRUE)

#Song length data
song_lengths <- c(448,242,231,246,246,293,280,227,244,213,262,239,213,258,255,257)
