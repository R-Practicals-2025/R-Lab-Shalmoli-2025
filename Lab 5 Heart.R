#read the csv file
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/Heart.csv")
print(data)

#dimensions of the data
print(dim(data))
[1] 303  15
#column names of the data
print(colnames(data))
#row names of the data
print(rownames(data))
#print first 30 lines of the data
print(head(data, 30))
#data representation as a frequency table based on specific columns and not on the entire data set
print(table(data$Chol)) #applies table to only the column of Chol
print(table(data$RestBP)) #applies table to only the column of RestBP
#to represent frequency table on the entire data set, we apply a function called as lapply()
lapply(data, table) #applies table to each column
#to know the categorical variables
X <- sapply(data, is.numeric) #gives TRUE for the columns that have numeric values
#we need the columns that are not numeric so that we can apply categories to them
Y <- data[!X]
print (colnames(Y))
[1] "ChestPain" "Thal"      "AHD" #printing the names of the categorical columns
#to print the unique levels present in each category
print(unique(data$ChestPain))
[1] "typical"      "asymptomatic" "nonanginal"   "nontypical"
print(unique(data$Thal))
[1] "fixed"      "normal"     "reversable" NA
print(unique(data$AHD))
[1] "No"  "Yes"

#to know the categorical variables present and the levels of each categorical variables
data$ChestPain <- factor(data$ChestPain, levels = c( "typical","asymptomatic", "nonanginal", "nontypical"  ))
data$Thal <- factor(data$Thal, levels = c("fixed","normal","reversable"))
data$AHD <- factor(data$AHD, levels = c("No","Yes"))
#to know which of the categorical variables/columns are factors
categorical_vars <- sapply(data, is.factor) #gives TRUE for the columns having categories or levels
print(sum(categorical_vars))
[1] 3 #gives the total number of categorical columns
#print the name of the columns that are categories
print(colnames(data)[categorical_vars])
[1] "ChestPain" "Thal"      "AHD" 
#how many levels does each categorical variable have and what are the levels
#the for loop traverses through the columns that have categories
for (var in colnames(data)[categorical_vars]) {
  print(paste("Variable:", var))
  print(paste("Number of levels:", nlevels(data[[var]])))
  print(paste("Levels:", levels(data[[var]])))
}
#Output
[1] "Variable: ChestPain"
[1] "Number of levels: 4"
[1] "Levels: typical"      "Levels: asymptomatic" "Levels: nonanginal"   "Levels: nontypical"  
[1] "Variable: Thal"
[1] "Number of levels: 3"
[1] "Levels: fixed"      "Levels: normal"     "Levels: reversable"
[1] "Variable: AHD"
[1] "Number of levels: 2"
[1] "Levels: No"  "Levels: Yes"

#calculate statistical parameters of the data
print(mean(data$RestBP))
[1] 131.6898
print(mean(data$RestECG))
[1] 0.990099
print(mean(data$MaxHR))
[1] 149.6073
print(mean(data$Oldpeak))
[1] 1.039604
print(median(data$Oldpeak))
[1] 0.8
print(median(data$RestBP))
[1] 130
print(median(data$RestECG))
[1] 1
print(median(data$MaxHR))
[1] 153
#we can't compute mode since R returns output as "Numeric" when we try to compute mode
print(mode(data$RestBP))
[1] "numeric"
#another way to calculate mode
which.max(table(data$RestBP))
120 #the number repeated maximum times is 120, so the mode is 120
15  #R keeps all the 120 in a bin and returns the index value of that bin, so here the index value is 15
#to print standard deviation
print(sd(data$Oldpeak))
[1] 1.161075
print(sd(data$RestBP))
[1] 17.59975
print(sd(data$RestECG))
[1] 0.9949713
print(sd(data$MaxHR))
[1] 22.875
#summary of the columns
print(summary(data$Oldpeak))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00    0.00    0.80    1.04    1.60    6.20 
print(summary(data$RestBP))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
94.0   120.0   130.0   131.7   140.0   200.0 
print(summary(data$RestECG))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0000  0.0000  1.0000  0.9901  2.0000  2.0000 
print(summary(data$MaxHR))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
71.0   133.5   153.0   149.6   166.0   202.0 
#to create histogram plots
hist(data$Oldpeak)
hist(data$RestBP)
hist(data$RestECG)
hist(data$MaxHR)
#to print the skewness value
library(moments)
print(skewness(data$Oldpeak))
[1] 1.263426
print(skewness(data$RestBP))
[1] 0.7025346
print(skewness(data$RestECG))
[1] 0.01980163
print(skewness(data$MaxHR))
[1] -0.5347844
print(kurtosis(data$Oldpeak))
[1] 4.530193
print(kurtosis(data$RestBP))
[1] 3.845881
print(kurtosis(data$RestECG))
[1] 1.013773
print(kurtosis(data$MaxHR))
[1] 2.927602

#to make boxplot by setting default values
boxplot(data$RestBP)
boxplot(data$RestECG)
boxplot(data$MaxHR)
boxplot(data$Oldpeak)
#creating boxplot with various options
boxplot(data$RestECG,range=0.1,xlab="Spread of Rest ECG",ylab="Rest ECG",horizontal=FALSE,border="blue",col="red")
#take various other ranges for the boxplot
boxplot(data$RestECG,range=0.2,xlab="Spread of Rest ECG",ylab="Rest ECG",horizontal=FALSE,border="blue",col="red")
boxplot(data$RestECG,range=0.05,xlab="Spread of Rest ECG",ylab="Rest ECG",horizontal=FALSE,border="blue",col="red")
#make boxplot with 2 more datasets
boxplot(data$RestBP,border="red",col="black")
boxplot(data$MaxHR,border="grey",col="blue")

#building subsets of the data
#subset of the RestBP values greater than 120
filter1 = subset(data, data$RestBP > 120)
print(filter1)
print(dim(filter1)) #to print the dimensions of this subset
[1] 206  15
#to print specific rows
filter2 = data[c(1, 3, 8, 9, 13, 14, 18, 21),]
print(filter2)
#obtain indices of the rows that has only "typical" chestpain entrices and build a subset of the data using these indices
filter3_ind=which(data$ChestPain=="typical")
print(filter3_ind) #prints the indices that has chestpain = "typical"
#building a subset of the data
filter3 = data[filter3_ind, ] #comma and space includes all the columns of the data set to be considered
print(filter3) 
#build a new column with this formula data$Chol * data$MaxHR / 234
new_dataframe <- data.frame(Chol = data$Chol, MaxHR = data$MaxHR, new_column = data$Chol*data$MaxHR/234)
print(new_dataframe)
#subset of the "typical" ChestPain value and writing the dataframe into a csv file
typical_dataframe<-data.frame(filter3)
#write the above sets of data in another csv file and it will show in the folder of the path added
write.csv(typical_dataframe, "C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/lab 5_typical ChestPain data.csv")
