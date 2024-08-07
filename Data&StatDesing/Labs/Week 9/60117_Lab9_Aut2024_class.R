################################################################################
# LAB 9

rm(list=ls())

getwd() #view working directory

lab9.data <- read.csv("lab9.csv", header=TRUE,
  colClasses=c("factor","numeric","factor")) #read CSV data




# QUESTION 1
############

# Q1(a)
#######
n <- length(lab9.data$health) #sample size
n #view

ni <- table(lab9.data$health) #frequencies of health levels
ni #view


ni/n #calculate sample probabilities


pi <- c(0.34,0.43,0.18,0.04,0.01) #hypothesised probabilities
sum(pi)

xSqr <- sum( (ni - pi*n)^2 / (pi*n) ) #test statistic
xSqr #view

xSqr95 <- qchisq(p=0.95, df=5-1) #0.95 quantile from chi-sqr(4)
xSqr95 #view

pchisq(q=xSqr, df=5-1, lower.tail=F) #p-value


chisq.test(x=ni, p=pi) #chi-square test




# QUESTION 2
############

# Q2(a)
#######
#install.packages("gmodels") #install if necessary
library("gmodels") #load library

CrossTable(x=lab9.data$health, y=lab9.data$cvd, expected=TRUE, prop.r=FALSE,
  prop.c=FALSE, prop.t=FALSE, chisq=TRUE, prop.chisq=TRUE) #cross tab and test



# Q2(b)-(d)
###########
#install.packages("gmodels") #install if necessary
library("gmodels") #load library

CrossTable(x=lab9.data$health, y=lab9.data$cvd, expected=FALSE, prop.r=TRUE,
  prop.c=FALSE, prop.t=FALSE, chisq=FALSE, prop.chisq=FALSE) #cross tab and test


odds3 <- 266/428 #odds health=3
odds3

odds1 <- 170/1180 #odds health=1
odds1

odds3/odds1 #odds ratio




# QUESTION 3
############

#install.packages("dplyr") #install if necessary
library("dplyr") #load library

lab9.data$ageBin <- as.factor(ntile(x=lab9.data$age,
  n=4)) #convert age to factor of quartiles

table(lab9.data$ageBin) #count of level observations



# Q3(a)
#######



# Q3(b)-(d)
###########
