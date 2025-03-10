#plot point (2,4) with square point and magenta color
x=2
y=4
plot(x,y,pch=0,col="magenta")

#function values of sin x and cos x
x=seq(-pi,pi,0.1)
y1=sin(x)
y2=cos(x)
plot(x,y1,col="blue",pch=8,xlab="Pi values",ylab="sin and cos values",main="Sine and Cosine graph",type="o")
points(x,y2,pch=4,col="red",type="o")

#biostatistics book problem
#probability distribution of families and number of assistance programs
x=seq(1,8,1)
y=c(0.2088,0.1582,0.1313,0.1313,0.1953,0.1246,0.0135,0.0370)
barplot(names.arg=x,y,main="Bar Plot",xlab="X Values",ylab="Y Values")

#make a 2X3 grid of 6 graphs
#x vs cos x with red color line
par(mfrow=c(2,3))
x=seq(-pi,pi,0.1)
plot(x,cos(x),type="l",col="red",main="x vs cos(x) graph")
#x vs (x^2/3)+4.2 with violet color, points and lines, linewidth=2 and linetype=1
plot(x,(x^2/3)+4.2,pch=0,type="l",lwd=2,lty=1,col="violet",main="x vs (x^2/3)+4.2")
#histogram plot of binomial distribution for 12 trials and p=0.3
hist(rbinom(1000,size=12,prob=0.3),main="Binomial Distribution",xlab="Value",ylab="Frequency")
#histogram plot of binomial distribution for 12 trials and p=0.8
hist(rbinom(1000,size=12,prob=0.8),main="Binomial Distribution",xlab="Value",ylab="Frequency")
#histogram plot using type="h" for x sequence of 1 to 10 with 0.5 spacing and colors=orange and blue
#y=50x/(x+2), colors should be alternate
x=seq(1,10,0.5)
y=50*x/(x+2)
hist(x,y,col=c("blue","orange"),type="h",main="Histogram Plot")
#x vs log(x) with orange color and line type = step
x=seq(1,10,1)
plot(x,log(x),col="orange",type="s",main="Logarithmic Plot")

#re-create the graph given in the slide with the title "This is a graph"
x=c(1,3,5,7,8,9,10,11)
y=c(2,7,5,9,6,8,9,10)
plot(x,y,type="o",col="pink",pch=16,lwd=2,lty=2,main="This is a graph",col.main="black",xlab="Time",ylab="Performance")
legend("topleft",legend="Per Curve",col="pink",lty=2,pch=16)

#histogram of hyper-geometric distribution with N=500,K=50 and n=30
#sampling is done without replacement
N=500
K=50
n=30
# Custom function to compute factorial using logarithm
log_factorial_manual <- function(x) {
  if (x == 0) return(0)  # log(0!) = log(1) = 0
  return(sum(log(1:x)))  # Sum of logs to compute log(x!)
}

log_choose_manual <- function(n, k) {
  return(log_factorial_manual(n) - log_factorial_manual(k) - log_factorial_manual(n - k))
}
#compute hypergeometric probability
hypergeometric_pmf <- function(k, N, K, n) {
  log_numerator <- log_choose_manual(K, k) + log_choose_manual(N - K, n - k)
  log_denominator <- log_choose_manual(N, n)
  return(exp(log_numerator - log_denominator))  # Convert log probability back to normal scale
}
# Define range for k (number of observed successes)
k_values <- 0:min(K, n)  # k ranges from 0 to min(K, n)

# Compute probabilities using manual function
probabilities <- sapply(k_values, function(k) hypergeometric_pmf(k, N, K, n))

# Normalize probabilities to avoid floating-point issues
probabilities <- probabilities / sum(probabilities)

# Plot the bar chart for hypergeometric distribution
barplot(probabilities, names.arg = k_values, col = "blue",
        main = "Hypergeometric Distribution (N=500, K=50, n=30)",
        xlab = "Number of Successes (k)", ylab = "Probability",
        border = "black", ylim = c(0, 0.25), las = 1)

#csv file of height and weight of 25,000 people
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/SOCR-HeightWeight.csv",header=TRUE)
print(colnames(data))
[1] "Index"          "Height.Inches." "Weight.Pounds."
print(dim(data))
[1] 25000     3
# Convert columns to numeric if needed
height <- data$Height.Inches.
weight <- data$Weight.Pounds.
# Compute mean and standard deviation for heights
mean_height <- mean(height, na.rm = TRUE)
sd_height <- sd(height, na.rm = TRUE)
# Compute mean and standard deviation for weights
mean_weight <- mean(weight, na.rm = TRUE)
sd_weight <- sd(weight, na.rm = TRUE)
#Histogram for Heights
hist(height, 
     breaks = "Sturges", 
     prob = TRUE,  #Normalize to probabilities
     col = "red", 
     border = "black",
     main = paste("Histogram of Height (Inches)"),  
     xlim = range(height), 
     xlab = "Height (Inches)", 
     ylab = "Probability Density")
#histogram for Weights (Pounds)
hist(weight, 
     breaks = "Sturges", 
     prob = TRUE,  #Normalize to probabilities
     col = "brown", 
     border = "black",
     main = paste("Histogram of Weight (Pounds)"),  
     xlim = range(weight), 
     xlab = "Weight (Pounds)", 
     ylab = "Probability Density")

