#central limit theorem
#CLT demo with sampling from non-normal, non-functional distribution
#Create 10,000 samples of single dice throw using the sample() function:
# a<- sample(1:6,replace=T,10000)
# Make a plot of this distribution. You should see a uniform distribution
a <- sample(1:6, 10000, replace = TRUE)
print (a)
# Plot barplot of single dice throws (Uniform Distribution)
barplot(table(a),col="blue",main="One dice throw",xlab = "Dice Value", ylab = "Frequency", border = "black")

#Throw two dice and add the scores together
#Generate a new object b similar to the above. Plot a histogram of a+b. 
# You should see a triangular shape developing for the histogram.
b <- sample(1:6, 10000, replace = TRUE) + sample(1:6, 10000, replace = TRUE)
print(b)
# Plot histogram of sum of two dice (Triangular Distribution)
hist(b, breaks = seq(1.5, 12.5, 1), col = "red", main = "Distribution of Two Dice Throws",
     xlab = "Sum of Two Dice", ylab = "Frequency", border = "black")

#Repeat the exercise with three dice. The histogram should start showing the distinct bell shape
c <- sample(1:6, 10000, replace = TRUE) + 
  sample(1:6, 10000, replace = TRUE) + 
  sample(1:6, 10000, replace = TRUE)

# Plot histogram of sum of three dice (Bell Shape)
hist(c, breaks = seq(2.5, 18.5, 1), col = "green", main = "Distribution of Three Dice Throws",
     xlab = "Sum of Three Dice", ylab = "Frequency", border = "black")

#Repeat this with five dice. The histogram is now very close to a normal curve
a <- sample(1:6, size=10000, replace=TRUE)
b <- sample(1:6, size=10000, replace=TRUE)
c <- sample(1:6, size=10000, replace=TRUE)
d <- sample(1:6, size=10000, replace=TRUE)
e <- sample(1:6, size=10000, replace=TRUE)

sum_5_dice <- a+b+c+d+e  # Sum of five dice throws
#plot histogram
h <- hist(sum_5_dice, main="Sum of Five Dice Throws - Very close to normal curve", xlab="Sum of Dice Faces", ylab="Frequency", col="purple", breaks=seq(4.5, 30.5, 1), xaxt="n")
axis(1, at=5:30)
#find mean
mean_val <- mean(sum_5_dice)
#find standard deviation
sd_val <- sd(sum_5_dice)
x <- seq(5, 30, length.out = 100)
y <- dnorm(x, mean=mean_val, sd=sd_val)
max_hist <- max(h$counts)   # Get max frequency from histogram
max_y <- max(y)   # Get max height of the unscaled normal curve
scaled_y <- y*(max_hist/max_y)   # Scale the normal y-values so that max(y) matches max(h$counts)
lines(x, scaled_y, col="black", lwd=2)

#ROC Curve
#use the pROC library of R to perform ROC analysis
#There are 11 input variables (see column names) and the output variable is the quality score in scale of 0 to 10 from wine tasting. 
#We want to test various thresholds of the quality score to achieve the best possible binary classifier of good and bad quality wine.
#Read in the white wine data into a dataframe, and create additional columns that classifies
#the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.
install.packages('pROC', dependencies=TRUE)
library("pROC")
white_wine <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/winequality-white.csv",sep = ";")
# Define function to classify wine as Good (1) or Bad (0) based on a given threshold
library('pROC')
white_wine <- read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/winequality-white.csv",sep = ";")
# Define function to classify wine as Good (1) or Bad (0) based on a given threshold
# Create good (label: 1) or bad (label: 0) wine binary classification columns based on different thresholds -
# If the quality is greater than or equal to the threshold, then it will be labelled as 1, otherwise will be labelled as 0.
white_wine$threshold_6 <- ifelse(white_wine$quality >= 6, 1, 0)
white_wine$threshold_7 <- ifelse(white_wine$quality >= 7, 1, 0)
white_wine$threshold_8 <- ifelse(white_wine$quality >= 8, 1, 0)
white_wine$threshold_9 <- ifelse(white_wine$quality >= 9, 1, 0)
white_wine$threshold_10 <- ifelse(white_wine$quality >= 10, 1, 0)
print(table(white_wine$threshold_9))    
# Output: 0: 4893, 1: 5
# Output: 0: 4893, 1: 5
# the quality score didn't go above threshold 9 is 4893 samples
# it went above the the threshold in 5 samples
print(table(white_wine$threshold_10))   # Output: 0: 4898
#for taking threshold as 10, the samples that went above threshold is 0 (no sample)

#Use plot.roc() function as follows to plot the ROC curves for each threshold value
# Plot ROC curves for each threshold
plot.roc(white_wine$threshold_6, white_wine$alcohol, main="ROC Curve (quality >= 6)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC (Area Under Curve): 0.735
plot.roc(white_wine$threshold_7, white_wine$alcohol, main="ROC Curve (quality >= 7)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.732
plot.roc(white_wine$threshold_8, white_wine$alcohol, main="ROC Curve (quality >= 8)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.750
plot.roc(white_wine$threshold_9, white_wine$alcohol, main="ROC Curve (quality >= 9)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.850
plot.roc(white_wine$threshold_10, white_wine$alcohol, main="ROC Curve (quality >= 10)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE) # No AUC
# for threshold >= 10, the data is classified as under 0 only (one variable). No 2 variable to consider
# Since, threshold >= 10 classifies the data into one class only (label: 0), so there will be no ROC / AUC. 
# A perfect classifier has an AUC of 1.0, which means it perfectly distinguishes b/w the positive and negative classes without any error.
# The threshold >= 9 gives the highest AUC (0.850) and therefore comes closest to being a "perfect classifier". 