
rm(list=ls())



# Load the npk data
data(npk)

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
ci_lower <- yield_mean + margin_error
ci_upper <- Inf
cat("The 95% upper-tail confidence interval for the population mean of yield is:", yield_mean, "to", ci_upper, "\n")

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

