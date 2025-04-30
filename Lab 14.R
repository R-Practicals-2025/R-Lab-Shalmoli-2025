#Q1: Error bar and co-variance
#the bar may be standard deviation or standard error of the mean or confidence interval
# Data
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)
# Barplot
bar_positions <- barplot(
  means,
  names.arg = c("A", "B", "C"),
  col = "grey",
  ylim = c(0, max(means + stderr) + 2),
  main = "Errors on bar plot",
  ylab = "Values"
)
# Error bars using arrows()
arrows(
  x0 = bar_positions, 
  y0 = means + stderr,
  x1 = bar_positions, 
  y1 = means - stderr,
  angle = 90, 
  code = 3, 
  length = 0.06,
  col = "red"
)
#error bars in (x,y) plots
# Data
x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
# Plot the data points
plot(x, y, 
     pch = 16, col = "blue", 
     xlab = "Concentration", 
     ylab = "Optical Activity", 
     main = "Error bars on data points",
     ylim = c(min(y - errors) - 1, max(y + errors) + 1)
)
# Add error bars
arrows(
  x0 = x, y0 = y + errors, 
  x1 = x, y1 = y - errors, 
  angle = 90, code = 3, length = 0.05, col = "red"
)

#Covariance and Pearson's co-relation co-efficient
#univariate sample the function cov() and cor() return a number
# for multivariate sample, these functions return a matrix
# Vectors
x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)

# Covariance
cov_xy <- cov(x, y)
print(paste("Covariance between x and y:", cov_xy))
# Output
# [1] "Covariance between x and y: 10549.4444444444"

# Pearson's Correlation Coefficient
cor_xy <- cor(x, y)
print(paste("Correlation coefficient between x and y:", cor_xy))
# Output
# [1] "Correlation coefficient between x and y: 0.98824672256029"

# Multivariate data: longley dataset
data(longley)  # built-in dataset in R
cor_matrix <- cor(longley)
print("Correlation matrix for longley dataset:")
print(cor_matrix)
#returns a matrix with GNP deflator, GNP Unemployed, Armed force, population and year

#One sample Z-test
#x is the data vector, sigma is the population standard deviation, muzero is the population mean for comparison
#alpha is the significance level and null is a string indicating type of null hypothesis
one_sample_Ztest <- function(x, sigma, muzero, alpha = 0.05, null = "equal") {
  n <- length(x)
  xbar <- mean(x)
  z <- (xbar - muzero) / (sigma / sqrt(n))
  
  # Determine p-value based on the alternative hypothesis
  if (null == "equal") {
    p_value <- 2 * (1 - pnorm(abs(z)))
    decision <- ifelse(p_value < alpha, "Reject H0: Mean ≠ mu0", "Fail to reject H0")
  } else if (null == "less_than_or_equal") {
    p_value <- 1 - pnorm(z)
    decision <- ifelse(p_value < alpha, "Reject H0: Mean ≤ mu0", "Fail to reject H0")
  } else if (null == "more_than_or_equal") {
    p_value <- pnorm(z)
    decision <- ifelse(p_value < alpha, "Reject H0: Mean ≥ mu0", "Fail to reject H0")
  } else {
    stop("Invalid null hypothesis. Choose from 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  # Return results
  result <- list(
    Z_value = round(z, 4),
    p_value = round(p_value, 4),
    conclusion = decision
  )
  return(result)
}

#providing the data set
# Sample data
x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)

# Run the Z-test
result <- one_sample_Ztest(x, sigma = 14.5, muzero = 124.6, alpha = 0.05, null = "equal")

# Show result
print(result)

#output
#$Z_value
# [1] 2.6598
#$p_value
#[1] 0.0078 #since the p-value is less than 0.05, so we can reject the null hypothesis
#and say that the result is statistically significant
#$conclusion
#[1] "Reject H0: Mean ≠ mu0"
# when it is a two-tailed test, we take alpha/2 on each side, which is 0.025
# so the p-value will be less and it should also be divided by 2

#One sample t-test
#given data set x that is randomly drawn from a Gaussian distribution
one_sample_t_test <- function(x, muzero, alpha, null) {
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)
  t_value <- (x_bar - muzero) / (s / sqrt(n))
  df <- n - 1
  if (null == "equal") {
    p_value <- 2 * pt(-abs(t_value), df)
    decision <- ifelse(p_value < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else if (null == "less_than_or_equal") {
    p_value <- pt(t_value, df, lower.tail = FALSE)
    decision <- ifelse(p_value < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else if (null == "more_than_or_equal") {
    p_value <- pt(t_value, df)
    decision <- ifelse(p_value < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else {
    stop("Invalid null hypothesis type. Choose from 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  return(list(
    "t-value" = t_value,
    "p-value" = p_value,
    "decision" = decision
  ))
}
#given the data set
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

result <- one_sample_t_test(x, muzero = 100, alpha = 0.05, null = "equal")
print(result)
#output
# $`t-value`
# [1] -0.8696358

# $`p-value`
# [1] 0.3982021
# the p-value is more than 0.05, so it is not statistically significant, so we fail to reject the null hypothesis

# $decision
# [1] "Fail to reject null hypothesis"

#one sample proportion test. Small sample size use binomial probabilities
# uses a normal approximation to the binomial distribution and can be used when n > 30.
# Inputs
x <- 710
n <- 2600
p <- 0.25

# Exact binomial test
binom_result <- binom.test(x, n, p = p, alternative = "greater")
# x is the number of successes, n is the total number of trials, p proportion to test against (i.e., hypothesized value), 
# alternative is the string value indicating type of null hypothesis as “two-sided”, “less”, “greater”, 
# and correct is a logical variable indicating whether a correction should be applied for small sample sizes

# Approximate proportion test
prop_result <- prop.test(x, n, p = p, alternative = "greater", correct = TRUE)

# Print results
print("Binomial Test Result:")
print(binom_result)

print("Proportion Test Result:")
print(prop_result)

#output
# 1-sample proportions test with continuity correction data:  x out of n, null probability p
# X-squared = 7.2621, df = 1, p-value = 0.003521
# alternative hypothesis: true p is greater than 0.25. 95 percent confidence interval: 0.2587571 1.0000000
#sample estimates: p 0.2730769 

#one sample variance test
one_sample_variance_test <- function(x, test_sigma, alpha = 0.05) {
  n <- length(x)
  sample_var <- var(x)
  test_stat <- (n - 1) * sample_var / test_sigma^2
  
  # Critical values for chi-squared distribution
  lower <- qchisq(alpha / 2, df = n - 1)
  upper <- qchisq(1 - alpha / 2, df = n - 1)
  # p-value
  p_value <- 2 * min(
    pchisq(test_stat, df = n - 1),
    1 - pchisq(test_stat, df = n - 1)
  )
  # Decision
  conclusion <- if (test_stat < lower || test_stat > upper) {
    "Reject the null hypothesis: population variance is different from test_sigma^2"
  } else {
    "Fail to reject the null hypothesis: population variance is not significantly different"
  }
  # this test checks whether the variance of a sample differs from a hypothesized population variance
  # Return result
  return(list(
    Sample_Variance = sample_var,
    Chi_Square_Stat = test_stat,
    Lower_Critical = lower,
    Upper_Critical = upper,
    P_Value = p_value,
    Conclusion = conclusion
  ))
}
# applying the function to data set
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

result <- one_sample_variance_test(x, test_sigma = 29, alpha = 0.05)
print(result)

#output 
# $Sample_Variance
# [1] 275.3856

# $Chi_Square_Stat
# [1] 4.911753

# $Lower_Critical
# [1] 6.262138

# $Upper_Critical
# [1] 27.48839

# $P_Value
# [1] 0.01430488

# $Conclusion
# [1] "Reject the null hypothesis: population variance is different from test_sigma^2"

# One sample Wilcoxon signed rank test
# for mean = 160 and confidence level = 95%
# the function wilcox.test() carries out one- and two-sample non-parametric tests
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

wilcox.test(x, mu = 160, alternative = "less", conf.int = TRUE, conf.level = 0.95)

#output
# Wilcoxon signed rank exact test
# data:  x
# V = 99, p-value = 0.9477
# alternative hypothesis: true location is less than 160
# 95 percent confidence interval: -Inf 169.2
# sample estimates: (pseudo)median 164.85 
# so we can infer that p-value > 0.05, so we fail to reject the null hypothesis.
# That means there’s not enough evidence to conclude the population median is less than 160

# two sample test
# two sample Z-test
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha = 0.05, null = "greater_than_or_equal") {
  n1 <- length(x1)
  n2 <- length(x2)
  
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  # Compute standard error of difference
  se_diff <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Compute Z statistic
  z_value <- (mean1 - mean2) / se_diff
  # Compute p-value based on the alternative hypothesis
  if (null == "equal") {
    p_value <- 2 * (1 - pnorm(abs(z_value)))
  } else if (null == "less_than_or_equal") {
    p_value <- 1 - pnorm(z_value)
  } else if (null == "greater_than_or_equal") {
    p_value <- pnorm(z_value)
  } else {
    stop("Invalid null hypothesis specified")
  }
  
  # Draw conclusion
  conclusion <- ifelse(p_value < alpha,
                       "Reject the null hypothesis",
                       "Fail to reject the null hypothesis")
  # Return results
  return(list(
    Z_value = z_value,
    P_value = p_value,
    Conclusion = conclusion
  ))
}
#using the data "two sample.dat" to test the null hypothesis
# Assuming the file is a whitespace-separated file with two columns
data <- read.table("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/two-sample.dat", header = FALSE, sep = "", fill = TRUE)
# Extract values
x1 =data$x1
x2 =data$x2
# Define function for two-sample Z-test
two_sample_Z_test=function(x1, x2, sigma_x1, sigma_x2, alpha, null) {
  # Calculate sample means and sizes
  mean_x1 <- mean(x1,na.rm=TRUE)
  mean_x2 <- mean(x2,na.rm=TRUE)
  n1 <- length(na.omit(x1))
  n2 <- length(na.omit(x2)) #removes the missing values NA
  # Compute standard error and test statistic
  se =sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  z_stat =(mean_x1 - mean_x2) / se
  
  # Determine critical value
  if (null=="mu1 >= mu2") {
    critical_value <- qnorm(alpha)
    reject <- z_stat < critical_value
  } else {
    stop("Invalid null hypothesis format. Expected 'mu1 >= mu2'")
  }
  
  # Output conclusion
  conclusion <- ifelse(reject, "Reject the null hypothesis", "Fail to reject the null hypothesis")
  return(list(Z_statistic = z_stat, Critical_Value = critical_value, Conclusion = conclusion))
}

# Perform the test
result <- two_sample_Z_test(x1, x2, sigma_x1 = 24.6, sigma_x2 = 27.8, alpha = 0.05, null = "mu1 >= mu2")

# Print results
print(result)


#two sample t-test
# Given data
Xvar <- c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar <- c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)

# Welch's t-test (default setting assumes unequal variances)
t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE)

#output
#Welch Two Sample t-test
#data:  Xvar and Yvar
#t = 3.0216, df = 19.966, p-value = 0.006749
# since the p-value is lesser than 0.05, so that we reject the null hypothesis
#alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval: 0.1138185 0.6215148
#sample estimates: mean of x mean of y 5.101000  4.733333 

#paired t-test for dependent variables
# Given data
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after  <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Paired t-test
t.test(data_before, data_after, alternative = "two.sided", paired = TRUE, conf.level = 0.95)
# output
# Paired t-test
# data:  data_before and data_after
# t = -1.7654, df = 13, p-value = 0.101
# the p-value is more than 0.05, so we accept the null hypothesis
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval: -15.248261   1.533975
# sample estimates: mean difference -6.857143 

# Two-sample Proportion Test using prop.test()
# Number of successes
x <- c(520, 550)

# Number of trials
n <- c(600, 600)

# Two-sample proportion test
prop.test(x = x, n = n, alternative = "two.sided", correct = TRUE)

#output
# 2-sample test for equality of proportions with continuity correction
# data:  x out of n
# X-squared = 7.2552, df = 1, p-value = 0.00707
# the p value here is less than 0.05, so we reject the null hypothesis
# alternative hypothesis: two.sided
# percent confidence interval: -0.0867225 -0.0132775
# sample estimates:
#  prop 1    prop 2 
# 0.8666667 0.9166667 

#Fisher's Exact test
# Create the 2x2 contingency matrix
tobacco_data <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = TRUE)

# Perform Fisher's Exact Test
fisher.test(tobacco_data, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)

#output
# Fisher's Exact Test for Count Data
# data:  tobacco_data
# p-value = 0.2797
# the p-value is greater than 0.05, so we accept the null hypothesis
# alternative hypothesis: true odds ratio is not equal to 1
# 95  percent confidence interval: 0.2250716 1.5641267
# sample estimates: odds ratio: 0.6036637 

# two sample variance test
two_sample_variance_test <- function(x, y, alpha = 0.05) {
  var_x <- var(x)
  var_y <- var(y)
  
  # Determine which variance is larger
  if (var_x > var_y) {
    F_stat <- var_x / var_y
    df1 <- length(x) - 1
    df2 <- length(y) - 1
  } else {
    F_stat <- var_y / var_x
    df1 <- length(y) - 1
    df2 <- length(x) - 1
  }
  # Critical values
  lower_crit <- qf(alpha / 2, df1, df2, lower.tail = TRUE)
  upper_crit <- qf(alpha / 2, df1, df2, lower.tail = FALSE)
  
  # p-value (two-tailed)
  p_value <- 2 * min(pf(F_stat, df1, df2), 1 - pf(F_stat, df1, df2))
  
  # Conclusion
  conclusion <- ifelse(F_stat < lower_crit | F_stat > upper_crit,
                       "Reject the null hypothesis: variances are significantly different",
                       "Fail to reject the null hypothesis: no significant difference in variances")
  
  return(list(
    Variance_x = var_x,
    Variance_y = var_y,
    F_value = F_stat,
    df1 = df1,
    df2 = df2,
    Lower_Critical = lower_crit,
    Upper_Critical = upper_crit,
    P_value = p_value,
    Conclusion = conclusion
  ))
}

x <- c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8,
       1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5, 1011.1, 991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

# Run the test
two_sample_variance_test(x, y, alpha = 0.05)

#output
# $Variance_x
# [1] 2539.784

# $Variance_y
# [1] 4769.661

# $F_value
# [1] 1.877979

# $df1
# [1] 15

# $df2
# [1] 15

# $Lower_Critical
# [1] 0.3493947

# $Upper_Critical
# [1] 2.862093

# $P_value
# [1] 0.2337848

# $Conclusion
# [1] "Fail to reject the null hypothesis: no significant difference in variances"

#Wilcoxon signed rank test for two dependent samples
# Define the data
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

# Perform Wilcoxon signed-rank test
result <- wilcox.test(Post_therapy, Pre_therapy,
                      alternative = "greater", # H0: median(Post - Pre) <= 0
                      paired = TRUE,
                      conf.int = TRUE,
                      conf.level = 0.95)

# Print the result
print(result)

#output
# Wilcoxon signed rank test with continuity correction
# data:  Post_therapy and Pre_therapy
# V = 20, p-value = 0.9809
# the p-value is more than 0.05, so we accept the null hypothesis 
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:-17.00001       Inf sample estimates:
# (pseudo)median -8.999961 

# Wilcoxon rank sum test (Mann-Whitney U test) for two independent samples
# Define the data
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

# Perform Wilcoxon rank sum test (Mann-Whitney test)
result <- wilcox.test(placebo, drug,
                      alternative = "less",  # H0: placebo ≥ drug; H1: placebo < drug
                      conf.int = TRUE,
                      conf.level = 0.95)

# Print the result
print(result)

#output
# Wilcoxon rank sum test with continuity correction
# data:  placebo and drug
# W = 82.5, p-value = 0.9303
# the p-value here is greater than 0.05, so we accept the null hypothesis
# alternative hypothesis: true location shift is less than 0
# 95 percent confidence interval: -Inf 25.99999
# sample estimates: difference in location 9.199947

# Kruskal Wallis test
# use rep() function to label each data point according to the group
# Define the group values
group1 <- c(220, 214, 203, 184, 186, 200, 165)
group2 <- c(262, 193, 225, 200, 164, 266, 179)
group3 <- c(272, 192, 190, 208, 231, 235, 141)
group4 <- c(190, 255, 247, 278, 230, 269, 289)

# Combine all values into one vector
x <- c(group1, group2, group3, group4)

# Create a group label vector
y <- factor(rep(1:4, each = 7))  # 4 groups, each with 7 observations

# Perform Kruskal-Wallis test
result <- kruskal.test(x, y)

# Print result
print(result)

#output
# Kruskal-Wallis rank sum test
# data:  x and y
# Kruskal-Wallis chi-squared = 7.0933, df = 3, p-value = 0.06898
# since the p-value here is greater than 0.05, we accept the null hypothesis

# Chi-square GoF (Goodness of Fit) test
chi_square_gof_test <- function(observed, expected, alpha = 0.05) {
  # Check input lengths
  if (length(observed) != length(expected)) {
    stop("Observed and expected vectors must be of same length.")
  }
  
  # Compute the chi-square test statistic
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  
  # Degrees of freedom
  df <- length(observed) - 1
  
  # Critical value from chi-square distribution
  critical_val <- qchisq(1 - alpha, df)
  
  # Compute p-value
  p_val <- pchisq(chi_sq_stat, df, lower.tail = FALSE)
  
  # Conclusion
  conclusion <- if (chi_sq_stat > critical_val) {
    "Reject the null hypothesis: observed distribution differs from expected."
  } else {
    "Fail to reject the null hypothesis: no significant difference."
  }
  
  # Return results
  return(list(
    Chi_Square_Statistic = chi_sq_stat,
    Degrees_of_Freedom = df,
    Critical_Value = critical_val,
    P_Value = p_val,
    Conclusion = conclusion
  ))
}

# use the function on the given data set
# Data
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

# Perform the test
result <- chi_square_gof_test(observed, expected)

# Print results
print(result)

#output
# $Chi_Square_Statistic
# [1] 3.7875

# $Degrees_of_Freedom
# [1] 3

# $Critical_Value
# [1] 7.814728

# $P_Value
# [1] 0.2853434

# $Conclusion
# [1] "Fail to reject the null hypothesis: no significant difference."

# One Way ANOVA
# read the csv file and make histogram plots of groups of people marked as ‘1st’, ‘2nd’ and ‘3rd’
titanicData <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/titanic.csv", header=TRUE)
# plot histogram in a 3X1 grid
par(mfrow = c(3, 1))  # 3x1 grid

hist(titanicData$age[titanicData$passenger_class == "1st"], 
     main = "1st Class", xlab = "Age", breaks = 30, col = "lightblue")

hist(titanicData$age[titanicData$passenger_class == "2nd"], 
     main = "2nd Class", xlab = "Age", breaks = 30, col = "lightgreen")

hist(titanicData$age[titanicData$passenger_class == "3rd"], 
     main = "3rd Class", xlab = "Age", breaks = 30, col = "lightcoral")

# The histograms for 1st, 2nd, and 3rd class passengers each display a roughly bell-shaped distribution with moderate symmetry and no extreme skewness or outliers. 
# This suggests the age data in each group is approximately normally distributed, satisfying the normality assumption for ANOVA

# Load dplyr and calculate group mean and standard deviation
# install the package dplyr
install.packages("dplyr")
# load the library
library(dplyr)
titanic_by_passenger_class <- group_by(titanicData, passenger_class)

summarise(titanic_by_passenger_class, 
          group_mean = mean(age, na.rm = TRUE),
          group_sd = sd(age, na.rm = TRUE))

# output
# A tibble: 3 × 3
# passenger_class group_mean group_sd
# <chr>                <dbl>    <dbl>
# 1 1st                   39.7     14.9
# 2 2nd                   28.3     13.0
# 3 3rd                   24.5     11.3
# If the standard deviations across 1st, 2nd, and 3rd classes are relatively close (e.g., within 2-3 years of each other), then the assumption of equal variances holds reasonably well.
# The standard deviations are fairly similar across groups. Therefore, the assumption of equal variance is reasonable.

# We fit the ANOVA model to the data using lm() function
lmresults <- lm(age ~ passenger_class, data = titanicData)
anova(lmresults)
# output
# Analysis of Variance Table
# Response: age
# Df Sum Sq Mean Sq F value    Pr(>F)    
# passenger_class   2  26690 13344.8  75.903 < 2.2e-16 ***
# Residuals       630 110764   175.8                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Conclusion Based on the Example:
# The F-value (75.903) is large, indicating that there is a significant difference between the passenger classes in terms of age.
# The p-value (< 2.2e-16) is extremely small, confirming that the difference is statistically significant.
# Thus, we reject the null hypothesis and conclude that there are significant differences in age among the passenger classes.

#Perform Tukey's HSD test
tukey_results <- TukeyHSD(aov(lmresults))

# Print the results
print(tukey_results)

#output
# Tukey multiple comparisons of means
# 95% family-wise confidence level

# Fit: aov(formula = lmresults)

# $passenger_class
# diff        lwr        upr     p adj
# 2nd-1st -11.367459 -14.345803  -8.389115 0.0000000
# 3rd-1st -15.148115 -18.192710 -12.103521 0.0000000
# 3rd-2nd  -3.780656  -6.871463  -0.689849 0.0116695
# # Interpretation of Tukey's Results:
# The p-values for all comparisons (2nd vs 1st, 3rd vs 1st, 3rd vs 2nd) are very small (0.0000000 and 0.0116695), which means that all pairwise comparisons are statistically significant. This confirms that the mean ages of the passengers in different classes are significantly different from each other.
# 2nd vs 1st: Mean age of 2nd class is significantly lower than 1st class.
# 3rd vs 1st: Mean age of 3rd class is significantly lower than 1st class.
# 3rd vs 2nd: Mean age of 3rd class is significantly lower than 2nd class


# Perform Kruskal-Wallis test
kruskal_results <- kruskal.test(age ~ passenger_class, data=titanicData)

# Print the result
print(kruskal_results)
# output
# Kruskal-Wallis rank sum test
# data:  age by passenger_class
# Kruskal-Wallis chi-squared = 116.08, df = 2, p-value < 2.2e-16

# The Kruskal-Wallis test is a non-parametric alternative to ANOVA that doesn't assume normality. It tests whether there are significant differences between the medians of the groups.
# Interpretation of Kruskal-Wallis Results:
# Kruskal-Wallis chi-squared = 116.08, df = 2, p-value < 2.2e-16:
# The p-value is extremely small (< 2.2e-16), indicating that there is a significant difference between the passenger classes in terms of age.
# Since the p-value is much smaller than the significance level (usually 0.05), we reject the null hypothesis that the median ages of the groups are equal. This suggests that the ages of passengers from different classes are significantly different.


# Conclusion: The Chi-Square Goodness of Fit test suggests that there is no significant 
# difference between the observed and expected frequencies, while both the Tukey HSD 
# and Kruskal-Wallis tests suggest significant differences in the mean or median ages of passengers in different classes.


# Cuckoo egg size problem
# load the data 
cuckooData <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/cuckooeggs.csv", header=TRUE)
str(cuckooData)
# outut
# 'data.frame':	120 obs. of  2 variables:
# $ host_species: chr  "Hedge Sparrow" "Hedge Sparrow" "Hedge Sparrow" "Hedge Sparrow" ...
# $ egg_length  : num  20.9 21.6 22.1 22.9 23.1 ...

dim(cuckooData)
# [1] 120   2
head(cuckooData)
len_of_unique<-length(unique(cuckooData$host_species))
print(len_of_unique)
# [1] 6

# Plot histograms for each host species
par(mfrow = c(len_of_unique/2, 2))  # Set layout

for (species in unique(cuckooData$host_species)) {
  hist(cuckooData$egg_length[cuckooData$host_species == species],
       main = species, xlab = "Egg Length", col = "lightblue",breaks=10)
}

par(mfrow = c(1,1)) 

# Group the data by host species
group_cuckoo_byspecies <- group_by(cuckooData, host_species)

# Summarize mean and standard deviation by species
summary_stats <- summarise(group_cuckoo_byspecies, 
                           group_mean = mean(egg_length, na.rm = TRUE),
                           group_sd = sd(egg_length, na.rm = TRUE))

# Print the summary statistics
print(summary_stats)
# output
# A tibble: 6 × 3
# host_species  group_mean group_sd
# <chr>              <dbl>    <dbl>
# 1 Hedge Sparrow       23.1    1.07 
# 2 Meadow Pipit        22.3    0.921
# 3 Pied Wagtail        22.9    1.07 
# 4 Robin               22.6    0.685
# 5 Tree Pipit          23.1    0.901
# 6 Wren                21.1    0.744

# Comment about variances
cat("The standard deviations across host species classes are similar.\n")
cat("Thus, the assumption of equal variance needed for ANOVA is satisfied.\n")

# Fit ANOVA model
anova_model <- aov(egg_length ~ host_species, data = cuckooData)
summary(anova_model)
# Conclusion:
# If p-value < 0.05 → Means are different across species.

# (d)
# Perform Tukey's HSD (Honestly Significant Difference) test
tukey_result <- TukeyHSD(anova_model)
# Print results
print(tukey_result)


# Significant differences (p < 0.05):
# Meadow Pipit vs Hedge Sparrow (p = 0.043) → Significant
# Wren vs Hedge Sparrow (p ≈ 0.0000006) → Highly Significant
# Tree Pipit vs Meadow Pipit (p ≈ 0.047) → Significant
# Wren vs Meadow Pipit (p ≈ 0.000486) → Highly Significant
# Wren vs Pied Wagtail (p ≈ 0.0000070) → Highly Significant
# Wren vs Robin (p ≈ 0.000318) → Highly Significant
# Wren vs Tree Pipit (p ≈ 0.0000006) → Highly Significant

# Not significant (p > 0.05):
# Pied Wagtail vs Hedge Sparrow
# Robin vs Hedge Sparrow
# Tree Pipit vs Hedge Sparrow
# Pied Wagtail vs Meadow Pipit
# Robin vs Meadow Pipit
# Robin vs Pied Wagtail
# Tree Pipit vs Pied Wagtail
# Tree Pipit vs Robin


# Maize and Malaria problem
# Read the data
malariaData <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/malaria vs maize.csv", header=TRUE)
# (a) Multiple histograms for original incidence rates
# Subset data
low <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "Low"]
medium <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "Medium"]
high <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "High"]

# Plot histograms
par(mfrow = c(1, 3))  # 1 row, 3 columns
hist(low, 
     main = "Low Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightblue", 
     breaks = 10)

hist(medium, 
     main = "Medium Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightgreen", 
     breaks = 10)
hist(high, 
     main = "High Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightpink", 
     breaks = 10)

# Reset layout
par(mfrow = c(1,1))

# Calculate standard deviations
sd_low <- sd(low)
sd_medium <- sd(medium)
sd_high <- sd(high)

cat("Standard deviations (original data):\n")
cat("Low:", sd_low, "\n")
# Low: 17.07239
cat("Medium:", sd_medium, "\n")
# Medium: 41.41618
cat("High:", sd_high, "\n")
# High: 126.4775

# (b) Check ANOVA assumptions
# If SDs are very different, variance homogeneity assumption is violated!

# (c) Log-transform incidence rates
malariaData$log_incidence <- log(malariaData$incidence_rate_per_ten_thousand)

# Subset log data
low_log <- malariaData$log_incidence[malariaData$maize_yield == "Low"]
medium_log <- malariaData$log_incidence[malariaData$maize_yield == "Medium"]
high_log <- malariaData$log_incidence[malariaData$maize_yield == "High"]

# Plot histograms for log-transformed data
par(mfrow = c(1, 3))
hist(low_log, 
     main = "Low Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightblue", 
     breaks = 10)

hist(medium_log, 
     main = "Medium Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightgreen", 
     breaks = 10)

hist(high_log, 
     main = "High Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightpink", 
     breaks = 10)

# Reset layout
par(mfrow = c(1,1))

# (c) Calculate standard deviations of log data
sd_low_log <- sd(low_log)
# Low (log): 1.129387 
sd_medium_log <- sd(medium_log)
# Medium (log): 0.3940452
sd_high_log <- sd(high_log)
# High (log): 0.7188472 
cat("Standard deviations (log-transformed data):\n")
cat("Low (log):", sd_low_log, "\n")
cat("Medium (log):", sd_medium_log, "\n")
cat("High (log):", sd_high_log, "\n")

# (d) Test association
# ANOVA on log-transformed data
anova_result <- aov(log_incidence ~ maize_yield, data = malariaData)
summary(anova_result)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# maize_yield  2  29.49  14.744   22.22 2.41e-05 ***
#  Residuals   16  10.62   0.663                     
# ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# circadian rhythm data
circadianData <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/circadian mutant health.csv", header=TRUE)
# Subset data for each genotype
tim01_data <- circadianData$days_to_death[circadianData$genotype == "tim01"]
rescued_data <- circadianData$days_to_death[circadianData$genotype == "tim01 (rescued)"]
wildtype_data <- circadianData$days_to_death[circadianData$genotype == "wild type"]

# Set up 1 row 3 columns layout for plots
par(mfrow = c(1, 3))

# Plot histograms
hist(tim01_data, 
     main = "tim01 Mutant", 
     xlab = "Days to Death", 
     col = "red", 
     breaks = 10, 
     xlim = c(0, 22))

hist(rescued_data, 
     main = "tim01 Rescued", 
     xlab = "Days to Death", 
     col = "blue", 
     breaks = 10, 
     xlim = c(0, 22))

hist(wildtype_data, 
     main = "Wild Type", 
     xlab = "Days to Death", 
     col = "green", 
     breaks = 10, 
     xlim = c(0, 22))

# Reset layout
par(mfrow = c(1,1))


# (b)
kruskal.test(days_to_death ~ genotype, data = circadianData)
# Kruskal-Wallis rank sum test
# data:  days_to_death by genotype
# Kruskal-Wallis chi-squared = 41.736, df = 2, p-value = 8.653e-10
# If the p-value is < 0.05, it means there is a statistically significant difference in median survival times between at least two groups.