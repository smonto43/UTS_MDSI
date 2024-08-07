################################################################################
# LAB 6

rm(list=ls())



# Question 1
############
#install.packages("MASS") #install if necessary
library(MASS) #load MASS package into library

data("iris") #read in data set

#data frame with more convenient variable names
data1 <- data.frame(sl=iris$Sepal.Length, sw=iris$Sepal.Width,
  pl=iris$Petal.Length, pw=iris$Petal.Width, species=iris$Species)



# Full dataset model
####################
model1 <- lm(formula=sl~pl, data=data1) #linear regression model

summary(model1)  #fitted model summary

confint(model1, level=0.95) #obtain 95% CI on parameters



# Q1(a)
#######
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(model1, which=1:6, ncol=3, label.size=3) #residual diagnostic plots



# Q1(b)
#######

shapiro.test(model1$resid) #run Shapiro-Wilk normality test


# Q1(c)
#######
#install.packages("lmtest") #install if necessary

library("lmtest")

dwtest(model1, alternative="two.sided") #run Durbin-Watson test



# Q1(d)
#######

r2 = 0.76
r = sqrt(r2)

# Q1(e)
#######
n <- 150 #sample size

m <- 1 #number of predictors

cooksCritical <- 4/(n-m-1) #critical value of Cook's D
cooksCritical #view


data1$cooksD <- cooks.distance(model1)  #save Cook's D

plot(x=data1$cooksD, xlab="sample id i", ylab="Cook's D",
  main="Scatter plot of Cook's D and critical value in red")

abline(a=cooksCritical, b=0, col="red", lwd=2) #Cook's D threshold


data1[data1$cooksD > cooksCritical,] #view influential points



# Reduced dataset model
#######################
data1.reduced <- data1[data1$cooksD <= cooksCritical, -6] #create reduced dataset

model1.reduced <- lm(formula=sl~pl, data=data1.reduced) #linear regression model

summary(model1.reduced)  #fitted model summary

confint(model1.reduced, level=0.95) #obtain 95% CI on parameters



# Q1(f)
#######

((0.39978-0.40892)/0.40892)*100

((4.30535-4.30660)/4.30660)*100

# Q1(g)
#######

autoplot(model1.reduced, which=1:6, ncol=3, label.size=3) #residual diagnostic plots




# Q1(h)
#######
shapiro.test(model1.reduced$resid) #run Shapiro-Wilk normality test



# Q1(i)
#######
dwtest(model1.reduced, alternative="two.sided") #run Durbin-Watson test
