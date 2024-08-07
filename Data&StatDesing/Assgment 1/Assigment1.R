
rm(list=ls())



# Load the npk data
data(npk)
# Load the necessary libraries
library(ggplot2)
library(ggfortify)

#Q1
#a
hist(npk$yield, breaks = "Sturges", probability = TRUE, main = "Histogram of Yield with Normal Curve",
     xlab = "Yield of peas (lbs/plot)", col = "lightblue")

yield_mean <- mean(npk$yield)
yield_sd <- sd(npk$yield)
curve(dnorm(x, mean = yield_mean, sd = yield_sd), add = TRUE, col = "red", lwd = 2)

#b
shapiro_test <- shapiro.test(npk$yield)
print(shapiro_test)


#c
yield_mean <- mean(npk$yield)
yield_sd <- sd(npk$yield)
n <- length(npk$yield)
t_value <- qt(0.95, df = n - 1)
margin_error <- t_value * (yield_sd / sqrt(n))
ci_upper <- yield_mean + margin_error  # Cambiado a ci_upper para reflejar que es el límite superior

cat("The 95% upper-tail confidence interval for the population mean of yield is above", yield_mean, "up to", ci_upper, "\n")

# Check
t_test_result <- t.test(npk$yield, alternative = "greater", conf.level = 0.95)
print(t_test_result)

#d

npk_with_nitrogen <- npk$yield[npk$N == 1]
wilcox_test <- wilcox.test(npk_with_nitrogen, mu = 55, alternative = "greater")
wilcox_test

#e

yields_with_nitrogen <- npk$yield[npk$N == 1]
yields_without_nitrogen <- npk$yield[npk$N == 0]
t_test_results <- t.test(yields_without_nitrogen, yields_with_nitrogen, alternative = "less", var.equal = FALSE)
t_test_results

boxplot(npk$yield ~ npk$N,
        xlab = "Nitrogen used",
        ylab = "Yield (lbs/plot)",
        main = "Boxplot of Pea Yield with and without Nitrogen",
        names = c("No Nitrogen", "Nitrogen"),
        col = c("red", "green"))


# Q2
#a
boxplot(yield ~ interaction(block, N), data = npk,
        xlab = "Block and Nitrogen Combination",
        ylab = "Yield (lbs/plot)",
        main = "Boxplot of Yield by Block and Nitrogen Combination",
        col = c("blue", "green"),
        las = 2)  # Rotate x-axis labels for better readability

#b

#c

anova_results <- aov(yield ~ block + N, data = npk)
anova_summary <- summary(anova_results)

print(anova_summary)

#d

tukey_results <- TukeyHSD(anova_results, which = "block")
print(tukey_results)

#e


autoplot(anova_results, which=1:6, ncol=3, label.size=3)

#for Q3 and 4
data(mtcars)
mtcars$vs <- factor(x=mtcars$vs, levels=c("0","1"))


#Q3

#a
linear_model <- lm(qsec ~ mpg, data = mtcars)
coefficients <- coef(linear_model)
cat("Regression equation: qsec = ", coefficients[1], " + ", coefficients[2], " * mpg", "\n")
predicted_mpg <- (17.5 - coefficients[1]) / coefficients[2]

cat("Predicted vehicle fuel economy (mpg) for an average quarter mile time of 17.5s: ", predicted_mpg, "\n")

#b

summary_linear_model <- summary(linear_model)
mpg_coefficient_test <- summary_linear_model$coefficients["mpg", ]
mpg_coefficient_test
summary_linear_model

#c

# Install and load the necessary package for Durbin-Watson test
#install.packages("lmtest")
library(lmtest)

dw_test <- dwtest(linear_model)

# Output the results of the Durbin-Watson test
print(dw_test)


#d

residual_standard_error <- summary_linear_model$sigma

sigma_squared <- residual_standard_error^2

cat("Estimated variance of errors (sigma^2) is:", sigma_squared, "\n")


#q4

#a

# Create the scatter plot
ggplot(mtcars, aes(x = hp, y = qsec, color = factor(vs))) +
  geom_point() +
  scale_color_manual(values = c("1" = "blue", "0" = "red")) +
  labs(color = "Engine Configuration", x = "Engine Horsepower (hp)", y = "Quarter Mile Time (qsec)") +
  ggtitle("Scatter Plot of Quarter Mile Time vs. Engine Horsepower")

#b

# Ensure 'vs' is a factor if it's not already
mtcars$vs <- as.factor(mtcars$vs)

# Fit the model with interaction between 'hp' and 'vs'
model <- lm(qsec ~ mpg + hp + vs + hp:vs, data = mtcars)

# Output the summary of the model to get the coefficients
summary_model <- summary(model)
print(summary_model)


#C
anova_table <- anova(model)


f_statistic <- summary_model$fstatistic
f_value <- f_statistic["value"]
f_df1 <- f_statistic["numdf"]
f_df2 <- f_statistic["dendf"]
f_p_value <- pf(f_value, f_df1, f_df2, lower.tail = FALSE)

# Print the F-statistic and p-value
cat("F-Statistic: ", f_value, "\nP-value: ", f_p_value, "\n")



# Print the F-statistic and the p-value from the ANOVA table
anova_table

#d

# Assuming 'model' is your fitted regression model

# New data for prediction
new_data <- data.frame(mpg = 26.5, hp = 300, vs = factor(0, levels = c(0, 1)))

# Compute the prediction interval
prediction_interval <- predict(model, newdata = new_data, interval = "predict", level = 0.95)

# Output the prediction interval
print(prediction_interval)

#e

model_for_vif <- lm(mpg ~ hp + vs + hp:vs, data=mtcars)
summary_model_for_vif <- summary(model_for_vif)
r_squared_for_vif <- summary_model_for_vif$r.squared
vif_mpg <- 1 / (1 - r_squared_for_vif)

cat("VIF for mpg is:", vif_mpg, "\n")



#f

cooks_distances <- cooks.distance(model)

# Find the index of the maximum Cook's D value
most_influential_index <- which.max(cooks_distances)

# Retrieve the maximum Cook's D value
max_cooks_d <- cooks_distances[most_influential_index]

# Print the Cook's D value of the most influential point
cat("The Cook's D of the most influential point is:", max_cooks_d, "\n")
cat("The most influential point is at index:", most_influential_index, "\n")


# Exclude the most influential point from the dataset
mtcars_excluded <- mtcars[-most_influential_index, ]

# Refit the model without the most influential point
model_excluded <- lm(qsec ~ mpg + hp + vs + hp:vs, data = mtcars_excluded)

# Output the summary of the new model to get the coefficients
summary_model_excluded <- summary(model_excluded)
print(summary_model_excluded)



