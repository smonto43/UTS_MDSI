################################################################################################
# CHAPTER 1

rm(list=ls())

# IMPORTING CSV DATA
# Method 1:
#  1. save CSV and R files in same location
#  2. with RStudio closed, double-click R file to open RStudio and set
#     working directory to location of CSV file
#  3. run "data1 <- read.csv" line of code

# Method 2:
#  1. modify "setwd" line of code to point to location of CSV file
#  2. run "setwd" line of code to set working directory
#  3. run "data1 <- read.csv" line of code


setwd(paste("C:/Users/Scott/Desktop/2023/",
  "60117_UnderstandingDataAndStatisticalDesign_Spring2023/",
  "SubjectDocuments/Chapter1/Lecture", sep="")) #Scott home read -- modify as needed

setwd(paste("C:/Users/127858/OneDrive - UTS/Desktop/2023/",
  "60117_UnderstandingDataAndStatisticalDesign_Spring2023/",
  "SubjectDocuments/Chapter1/Lecture", sep="")) #Scott work read -- modify as needed

getwd() #view working directory

data1 <- read.csv("chapter1.csv", header=TRUE,
  colClasses=c(rep("numeric",2),rep("factor",4),rep("numeric",2))) #read data

data1$activity <- factor(data1$activity,
  levels=c("low","medium","high")) #re-order levels

write.csv(x=data1, row.names=F, file="chapter1_new.csv") #write CSV data



# Slide 10
##########
table(data1$activity) #view frequencies of each level

barplot(height=table(data1$activity), main="Bar chart of activity",
  xlab="activity", ylab="frequency", col="lightblue")



# Slide 11
##########
pie(x=table(data1$activity), main="Pie chart of activity")



# Slide 12
##########
boxplot(x=data1$pulse2, main="Boxplot of pulse2", ylab="pulse2")



# Slide 14
##########
pulse2.mean <- mean(data1$pulse2) #sample mean pulse2

pulse2.median <- median(data1$pulse2) #sample median pulse2

hist(x=data1$pulse2, main="Histogram of pulse2", ylab="frequency",
  xlab="pulse2")

abline(v=pulse2.mean, col="red", lwd=2) #add line for mean

abline(v=pulse2.median, col="blue", lwd=2) #add line for median

legend("topright",lty=c(1,1),  col=c("red", "blue"), 
  legend = c("mean", "median")) #add legend to plot



# Slide 16
##########
boxplot(formula=data1$pulse2~data1$ran, main="Boxplot of pulse2 by ran",
  xlab="ran", ylab="pulse2")



# Slide 17
##########
plot(x=data1$height, y=data1$weight, xlab="height", ylab="weight",
  main="Scatter plot of height v. weight",)

abline(a=-204.7408, b=5.0918, col="red", lwd=2) #add regression line



# Slide 19
##########
sum(data1$pulse2) #sum pulse2

sum(data1$pulse2)/92 #sample mean pulse2

mean(data1$pulse2) #sample mean pulse2



# Slide 20
##########
pulse2.sort <- sort(data1$pulse2) #sort pulse2 and save

mean(pulse2.sort[c(46,47)]) #sample median pulse2

median(data1$pulse2) #sample median pulse2



# Slide 21
##########
sum((data1$pulse2-pulse2.mean)^2) #sum squared deviations pulse2

sum((data1$pulse2-pulse2.mean)^2)/91 #sample variance pulse2

var(data1$pulse2) #sample variance pulse2

sd(data1$pulse2) #sample std dev pulse2



# Slide 22
##########
height.mean <- mean(data1$height) #sample mean height

weight.mean <- mean(data1$weight) #sample mean weight

sum((data1$height-height.mean)*(data1$weight-weight.mean)) #sum cross deviations height and weight

sum((data1$height-height.mean)*(data1$weight-weight.mean))/91 #sample covariance height and weight

cov(data1$height,data1$weight) #sample covariance height and weight



# Slide 23
##########
sd(data1$height) #sample std dev height

sd(data1$weight) #sample std dev weight

cor(data1$height,data1$weight) #sample correlation height and weight



# Slide 24
##########
stat <- c("sample size", "mean", "median", "variance", "standard deviation",
  "IQR") #vector of statistic labels

value <- c(length(data1$pulse2), mean(data1$pulse2), median(data1$pulse2),
  var(data1$pulse2), sd(data1$pulse2), IQR(data1$pulse2,0.25)) #vector of statistic values

statTable <- data.frame(stat,value) #store in data frame

statTable #view


stat <- c("standard deviation height", "standard deviation weight",
  "covariance height and weight",
  "correlation height and weight") #vector of statistic labels

value <- c(sd(data1$height), sd(data1$weight),
  cov(data1$height, data1$weight),
  cor(data1$height, data1$weight)) #vector of statistic values

statTable2 <- data.frame(stat,value) #store in data frame

statTable2 #view



# Slide 27
##########
dbinom(x=50, size=100, prob=0.5) #P(X=50) where X~Bin(100,0.5)



# Slide 28
##########
n <- 100 #parameter bin dist

p <- 0.5 #parameter bin dist

xData <- 0:100 #data range of X

pX <- dbinom(x=xData, size=100, prob=0.5) #compute mass function values

plot(x=xData, y=pX, type='h', main="PMF of X~Bin(100,0.5)", xlab="x",
  ylab="p(x)")

abline(v=n*p, col="red", lwd=2) #add line for mean and median

legend("topright",lty=c(1),  col=c("red"), 
  legend = c("mean and median")) #add legend to plot



# Slide 31
##########
mu <- 0 #parameter norm dist

sigma <- 1 #parameter norm dist

curve(expr=dnorm(x,mean=mu,sd=sigma), from=-4, to=4, ylab="f(y)",
      main="PDF of Y~N(0,1)", xlab="y") #plot density function

abline(v=mu, col="red", lwd=2) #add line for mean and median

legend("topright",lty=c(1),  col=c("red"), 
  legend = c("mean and median")) #add legend to plot



# Slide 32
##########
hist(x=data1$pulse2, freq=F, xlab="pulse2", ylab="density",
  main="Histogram of pulse2 (with fitted normal PDF)")

curve(expr=dnorm(x, mean=mean(data1$pulse2), sd=sd(data1$pulse2)), 
  col="red", lwd=2, add=TRUE) #fit normal density function to Pulse2

