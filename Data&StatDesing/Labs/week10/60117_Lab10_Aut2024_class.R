################################################################################
# LAB 10

rm(list=ls())


getwd() #view working directory

lab10.data <- read.csv("lab10.csv", header=TRUE,
  colClasses=c("factor","numeric","factor")) #read CSV data




# QUESTION 1
############

# Q1(a)
#######
q1.model <- glm(formula=cvd~age, family=binomial(link="logit"),
  data=lab10.data) #fit logistic regression model

summary(q1.model) #view summary

confint(q1.model, level=0.95) #obtain 95% CI on parameters



# Q1(c)
#######
exp(-3.737973)

exp(0.050926)



# Q1(d)
#######
qnorm(0.975) #0.975 quantile from N(0,1) distribution



# Q1(e)
#######
plot(x=lab10.data$age, y=q1.model$fitted.values, xlab="age",
  ylab="pHat", main="Plot of predicted p v. age") #scatter plot



# Q1(f)
#######
q1.data.new <- data.frame(age=c(35, 75)) #predict q1 data

predict.glm(object=q1.model, newdata=q1.data.new,
  type="response", se.fit=F) #obtain fitted probability




# QUESTION 2
############

#a
q2.model <- glm(formula=cvd~health, family=binomial(link="logit"),
                data=lab10.data) #fit logistic regression model

summary(q2.model) #view summary

confint(q2.model, level=0.95) #obtain 95% CI on parameters

#b

# Assuming the logistic regression model is stored in q2.model and is already fitted

# Prepare the new data for prediction, ensuring 'health' is a factor
q2.data.new <- data.frame(health=c(3, 1))  
q2.data.new$health <- factor(q2.data.new$health, levels = c(1, 2, 3, 4, 5))

# Predict probabilities using the logistic regression model
predicted_probs <- predict.glm(object=q2.model, newdata=q2.data.new,
                               type="response", se.fit=FALSE)

# Calculate the odds based on predicted probabilities
odds <- predicted_probs / (1 - predicted_probs)

# Calculate the odds ratio for health=3 relative to health=1
odds_ratio <- odds[1] / odds[2]

# Output predicted probabilities, calculated odds, and odds ratio
cat("Predicted Probabilities:", predicted_probs, "\n")
cat("Calculated Odds:", odds, "\n")
cat("Odds Ratio (Average Health vs. Very Good Health):", odds_ratio, "\n")

# If you need to simulate cvd status for demonstration of CrossTable
# Simulate cvd data based on the predicted probabilities (not accurate but for demonstration)
simulated_cvd <- rbinom(2, size=1, prob=predicted_probs)

# Now perform the CrossTable
library(gmodels)
CrossTable(x = q2.data.new$health, y = factor(simulated_cvd, levels = 0:1, labels = c("No CVD", "CVD")),
           prop.chisq=FALSE)





