################################################################################
# LAB 12

rm(list=ls())

getwd() #view working directory

lab12.data <- read.csv("lab12.csv", header=TRUE,
  colClasses=c("factor",rep("numeric",each=2),"integer")) #read CSV data



# QUESTION 1
############
q1.model <- glm(formula=bp~bmi+h+age, family=binomial(link="logit"),
  data=lab12.data) #fit logistic regression model

summary(q1.model) #view summary

confint(q1.model, level=0.95) #obtain 95% CI on parameters



# Q1(a)
#######
lab12.data$pHat1 <- q1.model$fitted.values #save predicted probabilities

probCutoff1 <- 0.5 #prob cutoff value for classification

lab12.data$bpHat1 <- 0 #vector of bpHat set to 0

lab12.data[lab12.data$pHat1>probCutoff1,"bpHat1"] <- 1 #set case bpHat1=1


#install.packages("gmodels") #install if necessary
library("gmodels") #load library

CrossTable(x=lab12.data$bp, y=lab12.data$bpHat1, expected=FALSE, prop.r=TRUE,
  prop.c=FALSE, prop.t=TRUE, chisq=FALSE, prop.chisq=FALSE) #cross tab


0.019 + 0.856 #overall prediction accuracy

conf_matrix1 <- table(Predicted = lab12.data$bpHat1, Actual = lab12.data$bp)

# Calcular la precisión
accuracy1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)

# Calcular la sensibilidad (True Positive Rate)
sensitivity1 <- conf_matrix1[2,2] / sum(conf_matrix1[,2])

# Calcular la especificidad (True Negative Rate)
specificity1 <- conf_matrix1[1,1] / sum(conf_matrix1[,1])

# Mostrar resultados
accuracy1
sensitivity1
specificity1




# Q1(c)
#######
#install.packages("ROSE") #install if necessary
library("ROSE") #load library

roc.curve(response=lab12.data$bp, predicted=lab12.data$pHat1,
  xlab="1-specificity (false positive rate)",
  ylab="sensitivity (true positive rate)") #plot ROC



# Q1(d)
#######
summary(q1.model) #view summary

(3543.8 - 2610.9) / 3543.8 #pseudo R^2



# Q1(e)
#######
install.packages("ResourceSelection") #install if necessary
library("ResourceSelection") #load library

hl <- hoslem.test(x=lab12.data$bp, y=lab12.data$pHat1,
  g=10) #run Hosmer-Lemeshow test
hl #view

hl$observed #crosstab observed cell counts

hl$expected #crosstab expected cell counts




# QUESTION 2
############
q2.model <- glm(formula=bp~bmi+h*age, family=binomial(link="logit"),
  data=lab12.data) #fit logistic regression model

summary(q2.model) #view summary

confint(q2.model, level=0.95) #obtain 95% CI on parameters



# Q2(a)
#######
lab12.data$pHat2 <- q2.model$fitted.values
probCutoff2 <- 0.5
lab12.data$bpHat2 <- ifelse(lab12.data$pHat2 > probCutoff2, 1, 0)
library("gmodels")
CrossTable(x = lab12.data$bp, y = lab12.data$bpHat2, expected = FALSE, prop.r = TRUE,
           prop.c = FALSE, prop.t = TRUE, chisq = FALSE, prop.chisq = FALSE)
conf_matrix2 <- table(Predicted = lab12.data$bpHat2, Actual = lab12.data$bp)
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
sensitivity2 <- conf_matrix2[2,2] / sum(conf_matrix2[,2])
specificity2 <- conf_matrix2[1,1] / sum(conf_matrix2[,1])
accuracy2
sensitivity2
specificity2



# Q2(b)
#######
roc.curve(response = lab12.data$bp, predicted = lab12.data$pHat2,
          xlab = "1-specificity (false positive rate)",
          ylab = "sensitivity (true positive rate)") # plot ROC


# Q2(c)
#######
summary(q2.model) #view summary

(3543.8 - 2604.2) / 3543.8 #pseudo R^2



# Q2(d)
#######
# Realizar la prueba de Hosmer-Lemeshow para el modelo con interacción (Q2)
hl_q2 <- hoslem.test(x = lab12.data$bp, y = lab12.data$pHat2, g = 10)

# Mostrar los resultados de la prueba
hl_q2

# Mostrar los conteos observados y esperados en la prueba de Hosmer-Lemeshow
hl_q2$observed
hl_q2$expected


# Q2(3)
#######

