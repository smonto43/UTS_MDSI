################################################################################################
# LAB 1

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


getwd() #view working directory



data1 <- read.csv("lab1.csv", header=TRUE,
  colClasses=c("numeric","numeric")) #read CSV data




# Question 3
############

# Q3(a)
#######
mean(data1$x) #sample mean of x

sd(data1$x) #sample standard deviation of x


sapply(X=data1, FUN=mean) #sample means of x and y

sapply(X=data1, FUN=sd) #sample standard deviations of x and y



# Q3(b)
#######
cor(data1$x, data1$y) #sample correlation of x and y data



# Q3(c)
#######
boxplot(x=data1$x, main="Boxplot", ylab="x")


# Q3(d)
#######
plot(x=data1$x, y=data1$y, xlab="x", ylab="y",main="Scatter plot of x v. y",)

plot(formula= data1$y~data1$x, xlab="x", ylab="y")

abline(a=-6.41061, b=1.08606, col= "red",wd=2)



# Q3(e)
#######
hist(data1$x, freq=FALSE, xlab="x",
  main="Hisotogram of x data with fitted normal density") #histogram of x data

curve(expr=dnorm(x, mean=mean(data1$x), sd=sd(data1$x)), 
  col="red", lwd=2, add=TRUE) #fit normal density function to x data

