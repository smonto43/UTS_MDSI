################################################################################
# CHAPTER 5

rm(list=ls())

getwd() #view working directory

intro.data <- read.csv("intro.data.csv", header=TRUE,
  colClasses=c("numeric","numeric")) #read CSV data

life.data <- read.csv("life.data.csv", header=TRUE,
  colClasses=c("character", rep("numeric", times=6))) #read data




# Slide 3
#########
# Regression model
# y_i = 1 + 2*x + e_i with 0<x<=2, dx=0.02 and var(e_i)=0.25^2. 

# Plot data
plot(x=intro.data$x, y=intro.data$y, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot



# Slide 16-17
#############
intro.model <- lm(formula=y~x, data=intro.data) #linear regression model

summary(intro.model)  #coefficients table

plot(x=intro.data$x, y=intro.data$y, xlab="x", ylab="y",
  main="Scatter plot of sample data with fitted regression line")

abline(a=0.99466, b=1.98904, col="red", lwd=2) #add fitted regression line



# Slide 18
##########
intro.data$ySqrd <- intro.data$y^2 #save y^2 in data frame  

plot(x=intro.data$x, y=intro.data$ySqrd, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot

plot(x=intro.data$x^2, y=intro.data$ySqrd, xlab="x^2", ylab="y",
  main="Scatter plot of sample data") #scatter plot transform 1

plot(x=intro.data$x, y=sqrt(intro.data$ySqrd), xlab="x", ylab="sqrt(y)",
  main="Scatter plot of sample data") #scatter plot transform 2



# Slide 19
##########
intro.data$ySqrt <- sqrt(intro.data$y) #save sqrt(y) in data frame

plot(x=intro.data$x, y=intro.data$ySqrt, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot

plot(x=sqrt(intro.data$x), y=intro.data$ySqrt, xlab="sqrt(x)", ylab="y",
  main="Scatter plot of sample data") #scatter plot transform 1

plot(x=intro.data$x, y=intro.data$ySqrt^2, xlab="x", ylab="y^2",
  main="Scatter plot of sample data") #scatter plot transform 2


 
# Slide 20
##########
intro.data$yExp <- exp(intro.data$y) #save exp(y) in data frame   

plot(x=intro.data$x, y=intro.data$yExp, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot

plot(x=exp(intro.data$x), y=intro.data$yExp, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot transform 1(a)

plot(x=exp(2*intro.data$x), y=intro.data$yExp, xlab="e^x", ylab="y",
  main="Scatter plot of sample data") #scatter plot transform 1(b)

plot(x=intro.data$x, y=log(intro.data$yExp), xlab="x", ylab="log(y)",
  main="Scatter plot of sample data") #scatter plot transform 2



# Slide 21
##########
intro.data$yLog <- log(intro.data$y) #save log(y) in data frame

plot(x=intro.data$x, y=intro.data$yLog, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot

plot(x=log(intro.data$x), y=intro.data$yLog, xlab="log(x)", ylab="y",
  main="Scatter plot of sample data") #scatter plot transform

plot(x=intro.data$x, y=exp(intro.data$yLog), xlab="x", ylab="e^y",
  main="Scatter plot of sample data") #scatter plot transform 2



# Slide 22
##########
intro.data$xExp <- exp(intro.data$x) #save exp(x) in data frame

plot(x=intro.data$xExp, y=intro.data$yExp, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot



# Slide 23
##########
intro.data$xLog <- log(intro.data$x) #save log(x) in data frame

plot(x=intro.data$xLog, y=intro.data$yLog, xlab="x", ylab="y",
  main="Scatter plot of sample data") #scatter plot



# Slide 33-35
#############
summary(intro.model)  #coefficients table

confint(intro.model)  #coefficient CIs

qt(p=0.975, df=98) #0.975 quantile from T(98)



# Slide 45
##########
plot(x=life.data$gni, y=life.data$life, xlab="gni", ylab="life",
  main="Scatter plot of sample data") #scatter plot



# Slide 46
##########
life.data$gniLog <- log(life.data$gni) #save log of gni

plot(x=life.data$gniLog, y=life.data$life, xlab="log(gni)", ylab="life",
  main="Scatter plot of transformed sample data") #scatter plot



# Slide 48
##########
life.model <- lm(formula=life~gniLog, data=life.data) #linear regression model

summary(life.model)  #fitted model summary

confint(life.model, level=0.95) #obtain 95% CI on parameters



# Slide 50
##########
beta0 <- coef(life.model)[1] #extract estimated beta0

beta1 <- coef(life.model)[2] #extract estimated beta1

plot(x=life.data$gniLog, y=life.data$life, xlab="log(gni)", ylab="life",
  main="Scatter plot of transformed sample data") #scatter plot

abline(a=beta0, b=beta1, col="red", lwd=2) #add regression line


# Another method
life.data$fitted <- fitted(life.model) #save fitted values

plot(x=life.data$gniLog, y=life.data$life, xlab="log(gni)", ylab="life",
  main="Fitted regression model") #scatter plot

lines(x=life.data$gniLog, y=life.data$fitted, col="red",
  lwd=2) #add fitted values to plot



# Slide 51
##########
meanCI <- predict(life.model, life.data, se.fit=T,
  interval="confidence", level=0.95) #obtain mean 95% CI

life.data$mean.LB <- meanCI$fit[,2] #save lower bound

life.data$mean.UB <- meanCI$fit[,3] #save upper bound

lines(x=life.data$gniLog, y=life.data$mean.LB, col="blue") #add mean LB to plot

lines(x=life.data$gniLog, y=life.data$mean.UB, col="blue") #add mean UB to plot




# Slide 52
##########
individCI <- predict(life.model, life.data, se.fit=T,
  interval="prediction", level=0.95) #obtain individual 95% CI

life.data$individ.LB <- individCI$fit[,2] #save lower bound to plot

life.data$individ.UB <- individCI$fit[,3] #save upper bound to plot

lines(x=life.data$gniLog, y=life.data$individ.LB,
  col="orange") #add individual LB to plot

lines(x=life.data$gniLog, y=life.data$individ.UB,
  col="orange") #add individual UB to plot




# Slide 50 (gni scale)
######################
plot(x=life.data$gni, y=life.data$life, xlab="log", ylab="life",
  main="Fitted regression model", type="p") #scatter plot

lines(x=life.data$gni, y=life.data$fitted, col="red", lwd=2,
  type="p") #add fitted values to plot




# Slide 51 (gni scale)
######################
lines(x=life.data$gni, y=life.data$mean.LB, col="blue",
  type="p") #add mean LB to plot

lines(x=life.data$gni, y=life.data$mean.UB, col="blue",
  type="p") #add mean UB to plot




# Slide 52 (gni scale)
######################
lines(x=life.data$gni, y=life.data$individ.LB, col="orange",
  type="p") #add individual LB to plot

lines(x=life.data$gni, y=life.data$individ.UB, col="orange",
  type="p") #add individual UB to plot



# Save data
###########
write.csv(x=life.data, row.names=F, file="life.data.fit.csv") #write data


