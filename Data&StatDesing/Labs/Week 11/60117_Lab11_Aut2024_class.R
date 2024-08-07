################################################################################
# LAB 11

rm(list=ls())

 getwd() #view working directory

lab11.data <- read.csv("lab11.csv", header=TRUE,
  colClasses=c("integer","factor",rep("numeric",each=4))) #read CSV data


q1.model <- glm(formula=cvd~age+h*syst, family=binomial(link="logit"),
  data=lab11.data) #fit logistic regression model

summary(q1.model) #view summary

confint(q1.model, level=0.95) #obtain 95% CI on parameters



# Q1(a)
#######



# Q1(b)
#######



# Q1(c)
#######
zstar <- (0.032115 - 0.0375) / 0.002809
zstar

z95<- qnorm(p=0.05)
z95

pnorm(q=zstar)


# Q1(d)
#######
dStar <- 4274.5 - 3529.4 #test statistic
dStar #view

chi95 <- qchisq(p=0.95, df=10) #0.95 quantile from chi-square(10) distribution
chi95 #view

pchisq(q=dStar, df=10, lower.tail=FALSE) #p-value



# Q1(e)
#######



# Q1(f)
#######
lab11.data.new <- data.frame(age=c(37,82), syst=c(110,160),
  h=c("1","5")) #predict panel data

prob <- predict.glm(object=q1.model, newdata=lab11.data.new,
  type="response", se.fit=F) #obtain fitted probability

prob #view



# Q1(g)
#######

