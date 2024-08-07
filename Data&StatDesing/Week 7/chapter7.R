################################################################################
# CHAPTER 7

rm(list=ls())

getwd() #view working directory

car.data <- read.csv("car.data.csv", header=TRUE,
  colClasses=rep("numeric",times=4)) #read CSV data




# Slide 17
##########
curve(expr=dnorm(x), from=-4, to=4, main="Plot of density functions",
  xlab="x", ylab="density", col="black") #PDF Y~N(0,1)

curve(expr=dt(x,df=20), from=-4, to=4, col="red", add=TRUE) #PDF Y~T(20)

curve(expr=dt(x,df=2), from=-4, to=4, col="blue", add=TRUE) #PDF Y~T(2)

legend("topright",lty=c(1,1,1),  col=c("black", "red", "blue"), 
  legend = c("N(0,1)", "T(20)", "T(2)")) #add legend to plot



# Slide 24
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



# Slide 37
##########
pairs(formula=~mpg+engine+horse+weight, data=car.data) #matrix scatter plot



# Slide 38
##########
car.model1 <- lm(formula=mpg~engine+horse+weight,
  data=car.data) #linear regression model 0

#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(car.model1, which=1:6, ncol=3, label.size=3) #show diagnostic plots



# Slide 39
##########
car.data$gpm <- 1/car.data$mpg #calculate gallons per mile

pairs(formula=~gpm+engine+horse+weight, data=car.data) #matrix scatter plot



# Slide 40
##########
car.model1 <- lm(formula=gpm~engine+horse+weight,
  data=car.data) #linear regression model 1

summary(car.model1) #view summary information


 
# Slide 42
##########
#install.packages("car") #install if necessary
library("car")

vif(car.model1) #obtain VIF stats



# Slide 43  
##########
car.model2 <- lm(formula=gpm~horse+weight,
  data=car.data) #linear regression model 2

summary(car.model2) #view summary information



# Slide 44
##########
#install.packages("car") #install if necessary
library("car")

vif(car.model2) #obtain VIF stats



# Slide 45
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(car.model2, which=1:6, ncol=3, label.size=3) #show diagnostic plots



# Slide 46
##########
car.data$cooksD <- cooks.distance(car.model2)  #save Cook's D

cooksCritical <- 4/(392-2-1) #critical value of Cook's D

influential <- car.data[car.data$cooksD > cooksCritical,
  1:6] #save influential points

cookOrder <-  order(influential$cooksD, decreasing=T) #cook's sort index

influential[cookOrder,] #view influential points



# Slide 47
##########
car.data.reduced <- car.data[car.data$cooksD <= 0.04,
  1:5] #create reduced data set by removing influential points

car.model2.reduced <- lm(formula=gpm~horse+weight,
  data=car.data.reduced) #linear regression model 2 (reduced dataset)

summary(car.model2.reduced)  #fitted model summary



# Slide 48
##########
#install.packages("car") #install if necessary
library("car")

vif(car.model2.reduced) #obtain VIF stats



# Slide 49
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(car.model2.reduced, which=1:6, ncol=3,
  label.size=3) #show diagnostic plots



# Slide 50
##########
shapiro.test(residuals(car.model2.reduced)) #run normality test


#install.packages("lmtest") #install if necessary
library("lmtest") 

dwtest(car.model2.reduced, alternative="two.sided") #run Durbin-Watson test


plot(x=car.data.reduced$horse, y=resid(car.model2.reduced), xlab="horse",
  ylab="residual", main="Plot of residuals vs. horse")

plot(x=car.data.reduced$weight, y=resid(car.model2.reduced), xlab="weight",
  ylab="residual", main="Plot of residuals vs. weight")
