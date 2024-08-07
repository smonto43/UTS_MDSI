################################################################################################
# CHAPTER 4

rm(list=ls())


getwd() #view working directory

data4a <- read.csv("chapter4a.csv", header=TRUE,
  colClasses=c("numeric","factor", "factor")) #read CSV data
  
data4b <- read.csv("chapter4b.csv", header=TRUE,
  colClasses=c("numeric","factor", "factor")) #read CSV data

write.csv(x=data4a, row.names=F, file="chapter4a_new.csv") #write CSV data



# Slide 16
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



# Slide 27
##########
boxplot(formula=yield~concentration, data=data4a,
  main="Box plot of yield by concentration")

boxplot(formula=yield~farm, data=data4a,
  main="Box plot of yield by farm")



# Slide 29
##########
fit <- aov(formula=yield~concentration+farm, data=data4a) #save aov object

summary(fit) #view results



# Slide 30
##########
#install.packages("emmeans") #install if necessary
library("emmeans") #load library

meansObj <- emmeans(fit, ~concentration) #save emmeans object

fit.tukey <- contrast(object=meansObj, method="pairwise",
  adjust="tukey") #save contrast object

summary(fit.tukey, infer=c(T,T), level=0.95, side="two-sided") #view Tukey CIs

plot(fit.tukey) #view Tukey CI plots


meansObj <- emmeans(fit, ~concentration*farm) #save emmeans object

fit.tukey <- contrast(object=meansObj, method="pairwise",
  adjust="tukey") #save contrast object

summary(fit.tukey, infer=c(T,T), level=0.95, side="two-sided") #view Tukey CIs

plot(fit.tukey) #view Tukey CI plots





# Slide 31
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(fit, which=1:6, ncol=3, label.size=3) #show diagnostic plots



# Slide 32
##########
res <- residuals(fit) #save residuals

shapiro.test(res) #perform Shapiro-Wilk normality test



# Slide 35
##########
fit <- aov(formula=yield~concentration, data=data4a) #save aov object

summary(fit) #view results




# Slide 40
##########
boxplot(formula=gain~vitamin, data=data4b,
  main="Box plot of gain by vitamin")

boxplot(formula=gain~diet, data=data4b,
  main="Box plot of gain by diet")



# Slide 42
##########
with(data4b, interaction.plot(x.factor=vitamin, trace.factor=diet, response=gain,
  main="Iteraction plot vitamin by diet"))

with(data4b, interaction.plot(x.factor=diet, trace.factor=vitamin, response=gain,
  main="Iteraction plot diet by vitamin"))




# Slide 43
##########
fit <- aov(formula=gain~vitamin+diet+vitamin*diet, data=data4b) #save aov object

summary(fit) #view results


fit <- aov(formula=gain~vitamin*diet, data=data4b) #save aov object

summary(fit) #view results



# Slide 45
##########
#install.packages("emmeans") #install if necessary
library("emmeans") #load library


# the following IS NOT the way to perform analysis with interaction
meansObj <- emmeans(fit, ~vitamin) #save emmeans object

fit.tukey <- contrast(object=meansObj, method="pairwise",
  adjust="tukey") #save contrast object

summary(fit.tukey, infer=c(T,T), level=0.95,
  side="two-sided") #view Tukey CIs


# the following IS the way to perform analysis with interaction
meansObj <- emmeans(fit, ~vitamin|diet) #save emmeans object

fit.tukey <- contrast(object=meansObj, method="pairwise",
  adjust="tukey") #save contrast object

summary(fit.tukey, infer=c(T,T), level=0.95,
  side="two-sided") #view Tukey CIs




# Slide 46
##########
#install.packages("emmeans") #install if necessary
library("emmeans") #load library

meansObj <- emmeans(fit, ~diet|vitamin) #save emmeans object

fit.tukey <- contrast(object=meansObj, method="pairwise",
  adjust="tukey") #save contrast object

summary(fit.tukey, infer=c(T,T), level=0.95,
  side="two-sided") #view Tukey CIs



# Slide 47
##########
#install.packages("ggplot2") #install if necessary
library(ggplot2) #load library
#install.packages("ggfortify") #install if necessary
library(ggfortify) #load library

autoplot(fit, which=1:6, ncol=3, label.size=3) #show diagnostic plots



# Slide 48
##########
res <- residuals(fit) #save residuals

shapiro.test(res) #perform Shapiro-Wilk normality test

