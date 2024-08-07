################################################################################
# LECTURE 8

rm(list=ls())

getwd() #view working directory

maths.data <- read.csv("maths.data.csv", header=TRUE,
  colClasses=c(rep("factor",times=2),rep("numeric",times=3))) #read CSV data



# Model 1
#########

# slide 21
########## 
maths.data$sex <- factor(maths.data$sex, levels = c("M","F")) #re-order levels


boxplot(formula=maths.data$m2~maths.data$sex, xlab="sex", ylab="m2",
  main="Box plots of year 2 maths scores by sex") #box plot of burn times



# slide 22
########## 
maths.model1 <- lm(formula=m2~sex, data=maths.data) #regression model

summary(maths.model1) #summary information



# slide 23
########## 
tapply(X=maths.data$m2, INDEX=maths.data$sex,
  FUN=mean) #calculate mean year 2 score by sex



# slide 24
########## 
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(maths.model1, which=1:6, ncol=3, label.size=3) #show diagnostic plots

shapiro.test(resid(maths.model1)) #normality test

#install.packages("lmtest") #install if necessary
library("lmtest") 

dwtest(maths.model1, alternative="two.sided") #run Durbin-Watson test



# Model 2
#########

# slide 25
##########
colour.sex <- c("blue","red")[maths.data$sex] #assign colours to factor levels

plot(x=maths.data$m1, y=maths.data$m2, xlab="m1",
  ylab="m2", main="Plot of year 2 v. year 1 maths scores by sex",
  col=colour.sex, lwd=2) #scatter plot

legend("topleft",lty=c(1,1),  col=c("blue", "red"), 
  legend = c("male", "female")) #add legend to plot



# slide 26
##########
maths.model2 <- lm(formula=m2~sex*m1, data=maths.data) #regression model

summary(maths.model2) #summary information



# slide 28
########## 
#install.packages("car") #install if necessary
library("car")

vif(maths.model2) #obtain VIF stats



# slide 29
########## 
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(maths.model2, which=1:6, ncol=3, label.size=3) #show diagnostic plots

shapiro.test(resid(maths.model2)) #normality test

#install.packages("lmtest") #install if necessary
library("lmtest") 

dwtest(maths.model2, alternative="two.sided") #run Durbin-Watson test



# Model 3
#########

# slide 30
##########
colour.sch <- c("blue","red","orange")[maths.data$sch] #assign colours to factor levels

plot(x=maths.data$cur, y=maths.data$m2, xlab="cur",
  ylab="m2", main="Plot of year 2 maths scores v. curriculum by school",
  col=colour.sch, lwd=2) #scatter plot

legend("topleft",lty=c(1,1,1),  col=c("blue","red","orange"), 
  legend = c("school A", "school B", "school C")) #add legend to plot



# slide 31
##########
plot(x=maths.data$m1, y=maths.data$m2, xlab="m1",
  ylab="m2", main="Plot of year 2 v. year 1 maths scores by school",
  col=colour.sch, lwd=2) #scatter plot

legend("topleft",lty=c(1,1,1),  col=c("blue","red","orange"), 
  legend = c("school A", "school B", "school C")) #add legend to plot



# slide 32
##########
maths.model3 <- lm(formula=m2~cur+sch*m1, data=maths.data) #regression model

summary(maths.model3) #summary information



# slide 35
##########
anova(maths.model3) #anova table

maths.model3a <- lm(formula=m2~cur+m1+sch, data=maths.data) #regression model

anova(maths.model3a) #model without interaction term

1621.44 - 1602.73 #addition to SSR by adding interaction term 



# slide 37
########## 
#install.packages("car") #install if necessary
library("car")

vif(maths.model3) #obtain VIF stats



# slide 39
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(maths.model3, which=1:6, ncol=3, label.size=3) #show diagnostic plots

shapiro.test(resid(maths.model3)) #normality test

#install.packages("lmtest") #install if necessary
library("lmtest") 

dwtest(maths.model3, alternative="two.sided") #run Durbin-Watson test
