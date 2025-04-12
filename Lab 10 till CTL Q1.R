#sampling from a vector
x <- seq(1,100)
s <- sample(x,10) #sampling occurs without replacement, no number is repeated
#the numbers are printed only once
print(s)
[1]  2  9 98  7 60 26  1 53 66 61
#sampling with replacement, where a number can be chosen multiple times
sample(x,10,replace=TRUE)
[1] 35 69 92 87 95  7 35 97 92 49

#permutation and combination using package "gtools"
install.packages("gtools")
library(gtools)
x <- c("A","B","C","D")
per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE)
#generates permutation combinations of variables from X, with 3 elements selected
#so a total of 4^3=64 combinations are generated
print(per)
#to perform combination. By default it doesn't allows for repetitions
#so applying the mathematical formula of combination, we get only 4 combinations, since no repetitions are there
comb <- combinations(n=length(x), r=3, v=x)
print(comb)
    [,1] [,2] [,3]
[1,] "A"  "B"  "C" 
[2,] "A"  "B"  "D" 
[3,] "A"  "C"  "D" 
[4,] "B"  "C"  "D" 

#binomial distribution calculations
n <- 10  
p <- 0.4
m=3
dbinom(m,n,p)
[1] 0.2149908 #probability value for the binomial distribution
#to print the cumulative probability
pbinom(m,n,p)
[1] 0.3822806
cum_prob=0.8
#finds the smallest integer, the number of success "x" such that the cumulative probability is atleast 0.8
qbinom(cum_prob,n,p)
[1] 5
#randomly sample 5 point from the binomial distribution
npts=5
rbinom(npts,n,p)
[1] 4 5 0 6 5
#plot the probability density function (PDF) for the above parameters
p1=0.7
x_vals <- 0:n
pdf_p1 <- dbinom(x_vals, n, p)
pdf_p2 <- dbinom(x_vals, n, p1)
#generate barplot for this probability distribution
#beside function signifies that the bars of both the p values should be beside each other
#we use ylim, so that we can signify the limit of the bars
barplot(rbind(pdf_p1, pdf_p2), beside=TRUE, col=c("red", "blue"), 
        names.arg=x_vals, xlab="Number of successes", ylab="Probability", 
        main="Binomial PDF for p=0.4 and p=0.7", border="black", 
        ylim=c(0, max(pdf_p1, pdf_p2) * 1.2))
legend("topright", legend=c("p=0.4", "p=0.7"), fill=c("red", "blue"))
#generate 100 and 10000 points randomly from the distribution and make frequency table
#plot these as bar plots in a 2x1 grid
samples_100 <- rbinom(100, n, p)
samples_10000 <- rbinom(10000, n, p)
freq_100 <- table(samples_100)
print(freq_100)
#output 
samples_100
0  1  2  3  4  5  6  7  8 
1  4 15 18 25 26  8  2  1 
freq_10000 <- table(samples_10000)
print(freq_10000)
#output
samples_10000
0    1    2    3    4    5    6    7    8    9 
59  412 1206 2111 2485 2016 1159  435  101   16 
par(mfrow=c(2,1))  
barplot(freq_100, col="green", main="Histogram of 100 Samples", xlab="Number of successes", ylab="Frequency")
barplot(freq_10000, col="purple", main="Histogram of 10000 Samples", xlab="Number of successes", ylab="Frequency")

#hypergeometric distribution
#Plot a histogram type plot of the hypergeometric probability density function with N=100, K=70, p=0.3, n=12
N <- 100  # Population size
K <- 70   # Number of success states in the population
n <- 12   # Number of draws
x_vals <- 0:n 
pdf_vals <- dhyper(x_vals, K, N-K, n)  # Compute PDF
barplot(pdf_vals, names.arg=x_vals, col="skyblue", border="black",
        xlab="Number of Successes", ylab="Probability", 
        main="Hypergeometric Distribution (N=100, K=70, n=12)")
#add text within the plot window of the parameters meaning
text(5, 0.15, paste("N =", N, "K =", K, "n =", n), col="red", cex=1.2)
#compute cumulative probability up to x = 10
cum_prob_10 <- phyper(10, K, N-K, n)
print(round(cum_prob_10, 3))
[1] 0.928
#find x corresponding to cumulative probability of 0.9
x_cum_09 <- qhyper(0.9, K, N-K, n)
print(x_cum_09)
[1] 10
#sample 5 points randomly from this distribution
samples <- rhyper(5, K, N-K, n)
print(signif(samples, 2))  #print with two significant digits
[1]  6  8 11 12 11

#geometric distribution
#plot 2 probability density functions for p=0.3 and p=0.8
par(mfrow=c(1,2)) #set 1x2 grid for plots
x_vals <- 0:10
#PDF for p=0.3
p1 <- 0.3
pdf_p1 <- dgeom(x_vals, p1)
barplot(pdf_p1, names.arg=x_vals, col="red", border="black",
        xlab="Number of Failures", ylab="Probability", 
        main="Geometric PDF (p=0.3)")
#For p = 0.3: The probability of success is lower (only 30%), so failures are more likely before the first success.
#This results in a more spread-out distribution, meaning the probability is distributed over a larger range of failures
#PDF for p=0.8
p2 <- 0.8
pdf_p2 <- dgeom(x_vals, p2)
barplot(pdf_p2, names.arg=x_vals, col="blue", border="black",
        xlab="Number of Failures", ylab="Probability", 
        main="Geometric PDF (p=0.8)")
#For p = 0.8:The probability of success is much higher (80%), meaning success is likely very early.
#Most of the probability mass is concentrated around 0 or 1 failures, meaning the distribution is steep and decays much faster.
#compute cumulative probability up to x=4 (P(X <= 4) for p=0.3)
cum_prob_4 <- pgeom(4, p1)
print(paste("Cumulative probability up to x=4:", round(cum_prob_4, 3)))
[1] "Cumulative probability up to x=4: 0.832"
#compute m where cumulative probability is 0.2 for p=0.3
m_0.2 <- qgeom(0.2, p1)
print(paste("m for cumulative probability of 0.2:", m_0.2))
[1] "m for cumulative probability of 0.2: 0"
#Generate 6 random deviates from Geometric distribution with p=0.4
samples <- rgeom(6, 0.4)
print("Random samples from Geometric distribution (p=0.4):")
print(samples)
[1] "Random samples from Geometric distribution (p=0.4):"1  2 12  1  0  0""

#negative binomial distribution
y <- 5
r <- 3
p <- 0.3
prob_density <- dnbinom(y, size=r, prob=p)
print(prob_density)
[1] 0.09529569
#compute the negative binomial probability density upto y=5
cum_prob <- pnbinom(y, size=r, prob=p)
print(cum_prob)
[1] 0.4482262
#What is the y value corresponding to a cumulative probabilty value of 0.5? (ie the median)
y_median <- qnbinom(0.5, size=r, prob=p)
print(y_median)
[1] 6
#Print 4 random points sampled from this distribution with r=3 and p=0.3
random_samples <- rnbinom(4, size=r, prob=p)
print(random_samples)
[1]  7 11  8 14
#Plot the negative binomial distribution function using r=10, p=0.3
y_vals <- 0:20  # Possible failure values before 10 successes
prob_vals <- dnbinom(y_vals, size=10, prob=0.3)

barplot(y_vals, prob_vals, col="blue", xlab="Number of Failures (y)",
     ylab="Probability", main="Negative Binomial Distribution (r=10, p=0.3)")
#Generate a frequency histogram of 10,000 random deviates from this distribution with r=10 and p=0.3
random_data <- rnbinom(10000, size=10, prob=0.3)

hist(random_data, breaks=30, col="lightblue", border="black",
     main="Histogram of Negative Binomial Random Samples (r=10, p=0.3)",
     xlab="Number of Failures (y)", ylab="Frequency")

#Poisson distribution. The function used is pois
#Compute and print the Poisson probability given λ = 10 and m = 7
lambda <- 10
m <- 7
poisson_prob <- dpois(m, lambda)
print(poisson_prob)
[1] 0.09007923
#print the cumulative probability for the same values
cum_prob <- ppois(m, lambda)
print(cum_prob)
[1] 0.2202206
#Make two barplots showing a binomial probability distribution with n = 1000, p = 0.3 and a Poisson PDF with λ = np.
#for the poisson distribution to reach binomial distribution, fix the value of N and change p
n <- 1000
p <- 0.029
lambda_poisson <- n * p
x_vals <- 10:350  # Range of values to compare
# Compute Binomial and Poisson probabilities
binom_probs <- dbinom(x_vals, size=n, prob=p)
poisson_probs <- dpois(x_vals, lambda=lambda_poisson)
#plotting binomial distribution
plot(x_vals, binom_probs, type = "h", col = "lightpink3", lwd = 2,
     main = "Binomial vs Poisson Approximation",
     xlab = "X", ylab = "Probability", xlim=c(0,100),ylim = range(c(binom_probs, poisson_probs)))
#adding poisson distribution 
lines(x_vals, poisson_probs, xlim=c(0,100), type = "l", col = "slategray3", lwd = 2)
legend("topright", legend = c("Binomial", "Poisson"),
       col = c("lightpink3", "slategray3"), lty = c(1, 1), lwd = 2)
#for the binomial distribution to reach poisson distribution, fix the value of p and change N
n <- 4500
p <- 0.01
lambda <- n * p  

x_vals <-10:80 

binom_prob <- dbinom(x_vals, n, p)
poiss_prob <- dpois(x_vals, lambda)

plot(x_vals, binom_prob, type = "h", col = "lightpink3", lwd = 1.5,
     main = "Binomial vs Poisson Approximation",
     xlab = "X", ylab = "Probability", xlim = c(10, 80),
     ylim = range(c(binom_prob, poiss_prob)))

lines(x_vals, poiss_prob, col = "darkred", pch = 16)

legend("topright", legend = c("Binomial", "Poisson"),
       col = c("lightpink3", "darkred"), pch = c(124, 16), lwd = 1.8)
#The Poisson distribution is an approximation of the Binomial when n is large and p is small.
#Since n=1000 and p=0.3 is not small, the approximation may not be very accurate.
#However, if we take n even larger and p even smaller, the two distributions will closely match.
#find quantile value corresponding to cumulative probability of 0.22 and λ = 10 
quantile_value <- qpois(0.22, lambda=10)
print(quantile_value)
[1] 7
#Obtain 10000 random sample points from a Poisson distribution with λ = 9 and make a histogram plot
poisson_samples <- rpois(10000, lambda=9)
hist(poisson_samples, breaks=30, col="lightgreen", border="black",
     main="Histogram of Poisson Distributed Samples (λ = 9)",
     xlab="Number of occurrences", ylab="Frequency")

#Gaussian distribution. Compute and print the unit normal PDF value for μ = 12 and σ = 2
mu <- 12
sigma <- 2
x <- 12  # The value for which we compute the PDF
pdf_value <- dnorm(x, mean=mu, sd=sigma)
print(pdf_value)
[1] 0.1994711
#Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)
Z <- 2.0
cum_prob <- pnorm(Z)  # CDF for Z = 2.0
alt_cum_prob <- 1 - pnorm(-Z)  # 1 - CDF for Z = -2
print(cum_prob)
[1] 0.9772499
print(alt_cum_prob)
[1] 0.9772499
#Yes, pnorm(2) should be the same as 1 - pnorm(-2), due to the symmetry of the normal distribution
#Plot a unit normal curve for the above parameters with X range of ±4σ and add a text box to the plot showing the parameter symbols and their values
x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length=100)  # X range ±4σ
y_vals <- dnorm(x_vals, mean=mu, sd=sigma)
plot(x_vals, y_vals, type="l", col="blue", lwd=2,
     xlab="X values", ylab="Density",
     main="Normal Distribution (μ=12, σ=2)")
# Add text box
text(mu, max(y_vals)/2, labels=paste("μ =", mu, "σ =", sigma), col="red")
#Generate the 75th quantile point for a unit normal distribution with the above parameters.
quantile_75th <- qnorm(0.75, mean=mu, sd=sigma)
print(quantile_75th)
[1] 13.34898
#generate 10,000 random deviates from the unit normal distribution and plot a histogram. On top of this plot the unit normal curve from which you sampled
mu <- 12
sigma <- 2
samples <- rnorm(10000, mean=mu, sd=sigma)
#Histogram
hist(samples, probability=TRUE, breaks=50, col="salmon",
     main="Histogram of Normal Samples with Normal Curve",
     xlab="Values", ylab="Density")
#Overlay normal curve
x_vals <- seq(min(samples), max(samples), length=100)
y_vals <- dnorm(x_vals, mean=mu, sd=sigma)
lines(x_vals, y_vals, col="red", lwd=2)
#Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10 and p = 0.5
n <- 100  # Number of trials
p <- 0.5  # Probability of success
mu <- n * p  # Expected value
sigma <- sqrt(n * p * (1 - p))  # Standard deviation
# Generate binomial samples
m <- rbinom(10000, size=n, prob=p)
W <- (m - mu) / sigma  # Normalize
# Plot histogram of W
hist(W, probability=TRUE, breaks=30, col="lightgreen",
     main="Histogram of Normalized Binomial Distribution",
     xlab="W", ylab="Density")
# Overlay standard normal curve
x_vals <- seq(min(W), max(W), length=100)
y_vals <- dnorm(x_vals)
lines(x_vals, y_vals, col="blue", lwd=2)
#the two distributions agree, since for large n, the binomial distribution approximates the normal distribution
#Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10,100 and 1000 in the same graph
lambda_values <- c(1, 10, 100, 1000)

par(mfrow=c(2,2)) # 2X2 grid

for (lam in lambda_values) {
  x_poisson <- 0:ceiling(lam + 3 * sqrt(lam))
  poisson_pmf <- dpois(x_poisson, lam)
  
  x_normal <- seq(min(x_poisson), max(x_poisson), length=100)
  normal_pdf <- dnorm(x_normal, mean=lam, sd=sqrt(lam))
  
  plot(x_poisson, poisson_pmf, type="h", lwd=2, col="darkseagreen4",
       main=paste("Poisson vs Normal (λ=", lam, ")", sep=""),
       xlab="x", ylab="Probability", ylim=c(0, max(poisson_pmf, normal_pdf)))
       
  #overlay standard normal curve
  lines(x_normal, normal_pdf, col="deeppink4", lwd=2)
  
}
par(mfrow=c(1,1)) #setting the grid to 1X1 again
#For large λ (100, 1000), the Poisson distribution closely resembles a normal distribution

#using the library MASS
#Generate two correlated normal vectors using mvrnorm and analyze their properties
library(MASS)
xy <- mvrnorm(1000, mu=c(50,60), Sigma=matrix(c(4,3.7,3.7,9),2,2))

# (i) Compute variance and covariance
var_matrix <- var(xy)
print(var_matrix)  # Variance-Covariance matrix
       [,1]     [,2]
[1,] 4.124848 3.634403
[2,] 3.634403 8.901498
# (ii) Extract vectors and compute individual variances
x <- xy[,1]
y <- xy[,2]
print(var(x))
[1] 4.124848
print(var(y))
[1] 8.901498
# Plot correlation
plot(x, y, main="Scatter Plot of x vs y", xlab="x", ylab="y", col="black", pch=16)
# (iii) Check independence by verifying variance property
var_sum <- var(x) + var(y) 
var_combined <- var(x + y)
print(var_sum)  # Sum of individual variances
[1] 13.02635
print(var_combined)  # Variance of the sum
[1] 20.29515
#the sum of individual variances doesn't match with the variance of their sum 
#so the variances are independent of each other 
# (iv) Compute covariance using correlation coefficient
correlation <- cor(x, y)
computed_cov <- correlation * sqrt(var(x) * var(y))
print(computed_cov)  # Should match var_matrix[1,2]
[1] 3.634403
print(var_matrix[1,2])
[1] 3.634403 #we can see that the two variances are matching with each other

#Uniform distribution 
#Generate 5 uniform random numbers between 0 and 1 and print them
random_numbers <- runif(5, min=0, max=1)
print(random_numbers)
[1] 0.5769336 0.1196848 0.2280108 0.6044555 0.6094680
#Generate 5 random samples from a uniform distribution between 50 and 100
random_samples <- runif(5, min=50, max=100)
print(random_samples)
[1] 66.81976 90.18278 64.48830 69.34908 66.58096
#Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2.
uniform_samples <- runif(10000, min=1, max=2)
hist(uniform_samples, breaks=50, col="skyblue", main="Histogram of Uniform(1,2)",
     xlab="Value", xlim=c(1,2), probability=TRUE)

# Adding the theoretical uniform density line
curve(dunif(x, min=1, max=2), col="red", lwd=2, add=TRUE)

#exponential distribution
#Compute the probability density at x = 3 for λ = 2
lambda <- 2
x <- 3
pdf_value <- dexp(x, rate=lambda)
print(pdf_value)
[1] 0.004957504
#What is the quantile value corresponding to cumulative probability value of 0.995 for the above distribution
cumulative_prob <- 0.995
quantile_value <- qexp(cumulative_prob, rate=lambda)
print(quantile_value)
[1] 2.649159
#Plot the exponential cumulative probability distributions on the same graph for λ = 2, 10 and 100.
x_vals <- seq(0, 2, length.out=100)  # Range for x
lambda_vals <- c(2, 10, 100)  # Different lambda values
colors <- c("red", "blue", "green")

plot(x_vals, pexp(x_vals, rate=lambda_vals[1]), type="l", col=colors[1], lwd=2,
     xlab="x", ylab="Cumulative Probability", main="Exponential CDFs")
lines(x_vals, pexp(x_vals, rate=lambda_vals[2]), col=colors[2], lwd=2)
lines(x_vals, pexp(x_vals, rate=lambda_vals[3]), col=colors[3], lwd=2)
legend("bottomright", legend=c("λ=2", "λ=10", "λ=100"), col=colors, lwd=2)
#Compute and print 4 random deviates from an exponential distribution with λ = 3
random_values <- rexp(4, rate=3)
print(random_values)
[1] 0.3985578 0.3513210 0.3081696 0.2765115

#gamma distribution with shape option and scale option
#Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4
# Define x values
x_vals <- seq(0, 20, length.out=200)

# Set up a 1x2 plotting grid
par(mfrow=c(1,2))

# First plot: Vary α (shape), fix θ=4
alpha_vals <- c(1, 2, 3, 4)
theta <- 4
colors <- c("black", "blue", "red", "magenta")

plot(x_vals, dgamma(x_vals, shape=alpha_vals[1], scale=theta), type="l", col=colors[1], lwd=2,
     xlab="x", ylab="Density", main="Gamma PDFs (θ=4)")
for (i in 2:length(alpha_vals)) {
  lines(x_vals, dgamma(x_vals, shape=alpha_vals[i], scale=theta), col=colors[i], lwd=2)
}
legend("topright", legend=paste0("α=", alpha_vals), col=colors, lwd=2)
# Second plot: Vary θ (scale), fix α=4
theta_vals <- c(1, 2, 3, 4)
alpha <- 4

plot(x_vals, dgamma(x_vals, shape=alpha, scale=theta_vals[1]), type="l", col=colors[1], lwd=2,
     xlab="x", ylab="Density", main="Gamma PDFs (α=4)")
for (i in 2:length(theta_vals)) {
  lines(x_vals, dgamma(x_vals, shape=alpha, scale=theta_vals[i]), col=colors[i], lwd=2)
}
legend("topright", legend=paste0("θ=", theta_vals), col=colors, lwd=2)

#Compute and print the probability density corresponding to x = 6, α = 4 and θ = 1.
pdf_value <- dgamma(6, shape=4, scale=1)
print(pdf_value)
[1] 0.08923508
#Compute and print the cumulative probablity up to x=6 for the above gamma PDF
cdf_value <- pgamma(6, shape=4, scale=1)
print(cdf_value)
[1] 0.8487961
#Compute the x value which corresponds to a cumulative probability of 0.95
quantile_value <- qgamma(0.95, shape=4, scale=1)
print(quantile_value)
[1] 7.753657
#Obtain 10,000 random deviates from the above gamma distribution and plot a histogram of this set
random_values <- rgamma(10000, shape=4, scale=1)
# Plot histogram
hist(random_values, probability=TRUE, breaks=50, col="lightblue",
     main="Histogram of Gamma(4,1) Deviates", xlab="x")
# Overlay theoretical density curve
lines(density(random_values), col="red", lwd=2) 

#Chi-square distribution
#Plot the χ2 distribution with degree of freedom 2,3,5 and 10
# Define x values
x_vals <- seq(0, 30, length.out=300)

# Degrees of freedom
df_vals <- c(2, 3, 5, 10)
colors <- c("black", "blue", "red", "magenta")

# Plot the first chi-square distribution
plot(x_vals, dchisq(x_vals, df=df_vals[1]), type="l", col=colors[1], lwd=2,
     xlab="x", ylab="Density", main="Chi-Square Distributions") 

# Overlay additional chi-square distributions
for (i in 2:length(df_vals)) {
  lines(x_vals, dchisq(x_vals, df=df_vals[i]), col=colors[i], lwd=2)
}
legend("topright", legend=paste0("df=", df_vals), col=colors, lwd=2)

#Compute and print the probability density for x=6 and 5 degrees of freedom
pdf_value <- dchisq(6, df=5)
print(pdf_value)
[1] 0.09730435
#Compute and print the cumulative probability up to x=6 and 10 degrees of freedom
cdf_value <- pchisq(6, df=10)
print(cdf_value)
[1] 0.1847368
#Obtain the 85th quantile for this distribution for 6 degrees of freedom
quantile_value <- qchisq(0.85, df=6)
print(quantile_value)
[1] 9.446103
#Plot a histogram of 10,000 random deviates from this distribution with 6 degrees of freedom with 30 bins
random_values <- rchisq(10000, df=6)
# Plot histogram
hist(random_values, probability=TRUE, breaks=30, col="red",
     main="Histogram of χ² Distribution (df=6)", xlab="x")
# Overlay density curve
lines(density(random_values), col="black", lwd=2)
# Add text annotation
text(15, 0.08, "r=6", col="blue", cex=1.5) 

#Assuming μ = 2 and σ = 1 compute the variable Z^2
# Define x values
x_vals <- seq(-5, 5, length.out=300)
# Define parameters
mu <- 2
sigma <- 1
# Compute Z^2 values
z_squared <- ((x_vals - mu)^2) / sigma^2
# Plot χ² distribution with df=1
plot(x_vals, dchisq(z_squared, df=1), type="l", col="blue", lwd=2,
     xlab="x", ylab="Density", main="Chi-Square PDF (df=1)")

#central limit theorem
#CLT case with sampling from non-normal distribution
samples=replicate(10000,runif(5,min=0,max=10))
print(samples)
# (ii) Calculate the mean for each of the 10,000 samples and store it in a separate array.
# Plot this distribution of means as a histogram. Print the mean and standard
# deviation of the distribution of means.
sample_means=colMeans(samples)
print(sample_means)

hist(sample_means, breaks=30, col="lightblue", probability=TRUE,
     main="Uniform distribution",
     xlab="Sample Mean")

#print mean and standard deviation of sample means
mean_sample_means <- mean(sample_means)
sd_sample_means <- sd(sample_means)

print(paste("Mean of sample means:", mean_sample_means))
print(paste("Standard deviation of sample means:", sd_sample_means))

#output
"Mean of sample means: 4.99202507385234"
"Standard deviation of sample means: 1.27708108926243"
 
#Generate a sequence of numbers between 0 and 10 with 0.1 spacing.
# Generate x values from 0 to 10 with spacing of 0.1
x_vals <- seq(0, 10, by=0.1)
# Compute normal probability densities using mean and SD of sample means
normal_density <- dnorm(x_vals, mean=mean_sample, sd=sd_sample)
print(normal_density)
#Since we have 10,000 samples, we have to scale the normal probability density function to our particular case (since the area is 1.0 otherwise)
# Scaling factor: Total samples * bin width = 10,000 * 0.5 = 5000
scaled_density <- normal_density * 5000
print(scaled_density)
#Plot the normal curve on top of the histogram to see the level of agreement between the normal behaviour of the sample means and the normal curve
# Overlay normal curve
lines(x_vals, scaled_density, col="red", lwd=2)
legend("topright", legend=c("Histogram of Sample Means", "Normal Fit"),
       fill=c("lightblue", "red"), border=c("black", "black"))