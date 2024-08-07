################################################################################
# LECTURE 6

rm(list=ls())

getwd() #view working directory

intro.data <- read.csv("intro.data.csv", header=TRUE,
  colClasses=c("numeric","numeric")) #read CSV data

life.data <- read.csv("life.data.csv", header=TRUE,
  colClasses=c("character", rep("numeric", times=6))) #read CSV data




# Slide 4
#########
life.data$gniLog <- log(life.data$gni) #save log of gni

life.model <- lm(formula=life~gniLog, data=life.data) #linear regression model

summary(life.model)  #fitted model summary

confint(life.model, level=0.95) #obtain 95% CIs on parameters 

beta0 <- coef(life.model)[1] #extract estimated beta0

beta1 <- coef(life.model)[2] #extract estimated beta1



# Slide 6
#########
qt(p=0.975, df=185) #0.975 quantile from T(185) distribution



# Slide 7
#########
t <- (17.2798-15) / 2.9577 #test statistic
t #view

pt(q=t, df=185, lower.tail=FALSE) #p-value

qt(p=0.95, df=185) #0.95 quantile from T(185) distribution



# Slide 12
##########
curve(expr=df(x,df1=1,df2=30), xlim=c(0,4), ylim=c(0,1),
  xlab="y", ylab="density",
  main="Plot of density functions", col="red") #PDF F(1,30)

curve(expr=df(x,df1=2,df2=30), xlim=c(0,4), col="blue",
  add=TRUE) #PDF F(2,30)

curve(expr=df(x,df1=3,df2=30), xlim=c(0,4), col="orange",
  add=TRUE) #PDF F(3,30)

legend("topright",lty=c(1,1,1),  col=c("red", "blue", "orange"), 
  legend = c("F(1,30)", "F(2,30)", "F(3,30)")) #add legend to plot



# Slide 17
##########
intro.model <- lm(formula=y~x, data=intro.data) #linear regression model

summary(intro.model)  #coefficients table

plot(x=intro.data$x, y=intro.data$y, xlab="x", ylab="y",
  main="Fitted model -- no outlier") #scatter plot

abline(a=0.99466, b=1.98904, col="red", lwd=2) #add fitted regression line


intro.data2 <- rbind(intro.data, c(3,3)) #add row to data frame and save

intro.model2 <- lm(formula=y~x, data=intro.data2) #linear regression model

summary(intro.model2)  #coefficients table

plot(x=intro.data2$x, y=intro.data2$y, xlab="x", ylab="y",
  main="Fitted model -- outlier") #scatter plot

abline(a=1.17120, b=1.77949, col="red", lwd=2) #add fitted regression line



# Slide 18
##########
intro.data$resid <- residuals(intro.model) #save residuals in data frame

plot(x=intro.data$x, y=intro.data$resid, xlab="x", ylab="residual",
  main="Residuals model -- no outlier") #scatter plot of residuals


intro.data2$resid <- residuals(intro.model2) #save residuals in data frame

plot(x=intro.data2$x, y=intro.data2$resid, xlab="x", ylab="residual",
  main="Residuals model -- outlier") #scatter plot of residuals



# Slide 27
##########
rvNorm <- rnorm(n=50)

qqnorm(rvNorm, main="QQ plot of N(0,1) random sample") #sampling points

qqline(rvNorm, col="red", lwd=2) #line if normal


rvT <- rt(n=50, df=4)

qqnorm(rvT, main="QQ plot of T(4) random sample") #sampling points

qqline(rvT, col="red", lwd=2) #line if normal


rvExp <- rexp(n=50, rate=2)

qqnorm(rvExp, main="QQ plot of Exp(2) random sample") #sampling points

qqline(rvExp, col="red", lwd=2) #line if normal



# Slide 28-29
#############
plot(x=intro.data$x, y=intro.data$resid, xlab="x", ylab="residual",
  main="Plot of residuals") #scatter plot of residuals



# Slide 34
##########
summary(life.model)  #fitted model summary

names(life.model) #view elements of lm object


#install.packages("lmtest") #install if necessary
library("lmtest")

dwtest(life.model, alternative="two.sided") #run Durbin-Watson test



# Slide 35
##########
anova(life.model)  #fitted model ANOVA table



# Slide 36
##########
life.data$cooksD <- cooks.distance(life.model)  #save Cook's D

cooksCritical <- 4/(187-2) #critical value of Cook's D

plot(x=life.data$cooksD, xlab="sample id i", ylab="Cook's D",
  main="Scatter plot of Cook's D")

abline(a=cooksCritical, b=0, col="red", lwd=2) #Cook's D threshold



# Slide 37
##########
life.data[life.data$cooksD > cooksCritical,
  c("life","gniLog","cooksD")] #view influential points

life.data.reduced <- life.data[life.data$cooksD <= cooksCritical,
  1:8] #create reduced data set by removing influential points



# Slide 38
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(life.model, which=1:6, ncol=3, label.size=3) #show diagnostic plots



# Slide 40
##########
life.data$resid <- resid(life.model) #save residuals

shapiro.test(life.data$resid) #run Shapiro-Wilk normality test



# Slide 41
##########
life.model.reduced <- lm(formula=life~gniLog,
  data=life.data.reduced) #linear regression model reduced dataset

summary(life.model.reduced)  #fitted model summary reduced dataset


#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(life.model.reduced, which=1:6, ncol=3,
  label.size=3) #show diagnostic plots reduced dataset model



# Slide 42
##########
life.data.reduced$resid <- resid(life.model.reduced)#save residuals 

shapiro.test(life.data.reduced$resid) #run Shapiro-Wilk normality test reduced dataset

