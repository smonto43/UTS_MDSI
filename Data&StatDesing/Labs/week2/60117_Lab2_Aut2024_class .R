################################################################################
# LAB 2

rm(list=ls()) #remove all the files on the enviroment tab

getwd() #check working directory

data2a <- read.csv("lab2a.csv", header=TRUE,
  colClasses=c("numeric","factor")) #read CSV data

data2b <- read.csv("lab2b.csv", header=TRUE,
  colClasses=c("numeric","factor")) #read CSV data




#Question 1
###########
tapply(X=data2a$yld, FUN=mean, INDEX=data2a$fert) #means of yld by fert

tapply(X=data2a$yld, FUN=median, INDEX=data2a$fert) #medians of yld by fert

tapply(X=data2a$yld, FUN=sd, INDEX=data2a$fert) #std devs of yld by fert



# Q1(a)
#######
boxplot(data2a$yld~data2a$fert, xlab="fert", ylab="yld",
  main="Box plot of yld by fert")
#compare 


# Q1(b)
#######
t.test(x=data2a[data2a$fert==1,"yld"], alternative="less",
  mu=115, conf.level=0.95) #run T-test

qt(p=0.05, df=11) #0.05 quantile from T(11)

pt(q=-1.8242, df=11) #recalculate p-value



# Q1(c)
#######

t.test(x = data2a[data2a$fert == 1,"yld"], mu = 80, alternative = "two.sided",  conf.level=0.95)

qt(p=0.025, df=11) #0.025 quantile from T(11)
qt(p=0.975, df=11) #0.975 quantile from T(11)

pt(q=1.551, df=11) #recalculate p-value




#power analysis if mu1=100
power.t.test(n=12, delta=15, sd=sd(data2a[data2a$fert==1,"yld"]),
  sig.level=0.05, power=, type="one.sample",
  alternative="one.sided") #find power of test just conducted

power.t.test(n=, delta=15, sd=sd(data2a[data2a$fert==1,"yld"]),
  sig.level=0.05, power=0.8, type="one.sample",
  alternative="one.sided") #find sample size needed for power=0.8



# Q1(d)
#######
t.test(x=data2a[data2a$fert==2,"yld"], y=data2a[data2a$fert==1,"yld"],
  alternative="greater", mu=20, conf.level=0.95, var.equal=FALSE) #run T-test

qt(p=0.95, df=21) #0.95 quantile from T(21)

pt(q=1.9799, df=21, lower.tail=FALSE) #recalculate p-value


t.test(x=data2a[data2a$fert==1,"yld"], y=data2a[data2a$fert==2,"yld"],
  alternative="less", mu=-20, conf.level=0.95, var.equal=FALSE) #run T-test




# Question 2
############
tapply(X=data2b$time, FUN=mean, INDEX=data2b$noise) #means of time by noise

tapply(X=data2b$time, FUN=median, INDEX=data2b$noise) #medians of time by noise

tapply(X=data2b$time, FUN=sd, INDEX=data2b$noise) #std devs of time by noise



# Q2(a)
#######
boxplot(data2b$time~data2b$noise, xlab="noise", ylab="time",
        main="Box plot of time by noise")


# Q2(b)
#######
t.test(x=data2b[data2b$noise==2,"time"], y=data2b[data2b$noise==1,"time"],
       alternative="greater", mu=5, paired=TRUE, conf.level=0.95) #run T-test

qt(p=0.95, df=21) #0.95 quantile from T(21)

pt(q=1.9799, df=21, lower.tail=FALSE) #recalculate p-value
