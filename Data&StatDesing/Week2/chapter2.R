################################################################################################
# CHAPTER 2

rm(list=ls())

getwd() #view working directory

data2 <- read.csv("chapter2.csv", header=TRUE,
  colClasses=c(rep("numeric",2),rep("factor",4),rep("numeric",2))) #read data

data2$activity <- factor(data2$activity,
  levels=c("low","medium","high")) #re-order levels

write.csv(x=data2, row.names=FALSE, file="chapter2_new.csv") #write CSV data



# Slide 8
#########
n <- 5 #sample size

repeats <- 10^5 #repeat to build histogram

mu <- 5 #mean parameter of normal distribution

sigma <- 2 #standard deviation parameter of normal distribution

Y <- rnorm(n=n*repeats, mean=mu, sd=sigma) #generate Y~N(5,2) RVs

Y <- matrix(data=Y, ncol=repeats) #convert to  N-by-repeats sized matrix

Y.bar <- colMeans(Y) #compute means for each of the samples

Z <- (Y.bar-mu) / (sigma/sqrt(n)) #standardise sample means

Z.mean <- mean(Z) #mean of means
Z.mean #view

Z.sd <- sd(Z) #std. dev. of means
Z.sd #view

hist(x=Z, freq=FALSE, xlab="z", ylab="density",
  main="Y~N(5,2) with n=5") #histogram of Z

curve(dnorm(x, mean=Z.mean, sd=Z.sd), col="red", lwd=2,
  add=TRUE) #normal PDF fitted to Z histogram



# Slide 9
#########
n <- 5 #sample size

repeats <- 10^5 #repeat to build histogram

lambda <- 2 #parameter of exponential distribution

Y <- rexp(n=n*repeats, rate=lambda) #generate Y~Exp(2) RVs

Y <- matrix(data=Y, ncol=repeats) #convert to N-by-repeats sized matrix

Y.bar <- colMeans(Y) #compute means for each of the samples

Z <- (Y.bar-1/lambda) / (sigma/sqrt(n)) #standardise sample means

Z.mean <- mean(Z) #mean of means
Z.mean #view

Z.sd <- sd(Z) #std. dev. of means
Z.sd #view

hist(x=Z, freq=FALSE, xlab="z", ylab="density",
  main="Y~Exp(2) with n=5") #histogram of Z

curve(dnorm(x, mean=Z.mean, sd=Z.sd), col="red", lwd=2,
  add=TRUE) #normal PDF fitted to Z histogram



# Slide 14
##########
curve(expr=dnorm(x), from=-4, to=4, main="Plot of density functions",
  xlab="y", ylab="density", col="black") #PDF Y~N(0,1)

curve(expr=dt(x,df=20), from=-4, to=4, col="red", add=TRUE) #PDF Y~T(20)

curve(expr=dt(x,df=2), from=-4, to=4, col="blue", add=TRUE) #PDF Y~T(2)

legend("topright",lty=c(1,1,1),  col=c("black", "red", "blue"), 
  legend = c("N(0,1)", "t(20)", "t(2)")) #add legend to plot



# Slide 25
###########
stat <- c("sample size", "mean", "standard deviation", "variance",
  "median", "IQR") #vector of statistic labels

value <- c(length(data2$pulse1), mean(data2$pulse1), sd(data2$pulse1),
  var(data2$pulse1), median(data2$pulse1),
  IQR(data2$pulse1)) #vector of statistic values

statTable <- data.frame(stat,value) #store in data frame

statTable #view



# Slide 26
###########
t.test(x=data2$pulse1, alternative="two.sided", mu=70,
  conf.level=0.95) #test HA: mu != 70



# Slide 27
###########
qt(p=0.975, df=91) #0.975 quantile from T(91)



# Slide 29
###########
t.test(x=data2$pulse1, alternative="less", mu=74,
  conf.level=0.95) #test HA: mu < 74



# Slide 30
###########
qt(p=0.05, df=91) #0.05 quantile from T(91)



# Slide 37
###########
stat <- c("sample size", "mean", "standard deviation", "variance",
  "median", "IQR") #vector of statistic labels

pulse1.f <- data2[data2$sex=="female","pulse1"] #pulse1 females

pulse1.m <- data2[data2$sex=="male","pulse1"] #pulse1 males

stat.f <- c(length(pulse1.f), mean(pulse1.f), sd(pulse1.f),
  var(pulse1.f), median(pulse1.f),
  IQR(pulse1.f)) #vector of statistic values for females

stat.m <- c(length(pulse1.m), mean(pulse1.m), sd(pulse1.m),
  var(pulse1.m), median(pulse1.m),
  IQR(pulse1.m)) #vector of statistic values for males

statTable <- data.frame(stat,female=stat.f,male=stat.m) #store in data frame

statTable #view



# Slide 38
###########
t.test(x=pulse1.f, y=pulse1.m, alternative="greater", mu=2,
  conf.level=0.95, var.equal=FALSE) #test HA: mu1-mu2 > 2



# Slide 39
###########
qt(p=0.95, df=63) #0.95 quantile from T(63)



# Slide 46
###########
pulse1.y <- data2[data2$ran=="yes","pulse1"] #pulse1 run group

pulse2.y <- data2[data2$ran=="yes","pulse2"] #pulse2 run group

stat <- c("sample size", "mean", "standard deviation", "variance",
  "median", "IQR") #vector of statistic labels

stat.1 <- c(length(pulse1.y), mean(pulse1.y), sd(pulse1.y),
  var(pulse1.y), median(pulse1.y),
  IQR(pulse1.y)) #vector of statistic values for pulse 1 for run group

stat.2 <- c(length(pulse2.y), mean(pulse2.y), sd(pulse2.y),
  var(pulse2.y), median(pulse2.y),
  IQR(pulse2.y)) #vector of statistic values for pulse 2 for run group

statTable <- data.frame(stat,pulse1=stat.1,pulse2=stat.2) #store in data frame

statTable #view



# Slide 47
###########
t.test(x=pulse2.y, y=pulse1.y, alternative="greater", mu=15,
  conf.level=0.95, paired=TRUE) #test HA: mu2-mu1 > 15



# Slide 48
###########
qt(p=0.95, df=34) #0.95 quantile from T(34)



# Slide 50
###########
t.test(x=pulse2.y - pulse1.y, alternative="greater", mu=15,
  conf.level=0.95) #test HA: mu2-mu1 > 15

