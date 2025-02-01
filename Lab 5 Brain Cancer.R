#read CSV file and store data as data frame
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/BrainCancer.csv",header=TRUE)
print(data)

#dimensions of data shows the number of rows and columns
print(dim(data))

#row and column names of the data
print(colnames(data))  
print(rownames(data))

#first 30 lines show
print(head(data,30)) #if we don't write 30, by default it shows 10 lines

#data represent as a frequency table
print(table(data)) #it gives a frequency count of all the elements present in individual columns
#thus we apply this to individual columns to get a clearer output
print(table(data$sex))  #frequency table for sex
print(table(data$diagnosis))  #frequency table for diagnosis

#to know thw number of categorical variables
X <- sapply(data, is.numeric)   #gives columns containing numeric data
print(X) 
#categorical data are always non-numeric, since we can't categorize data based on numbers
y <- data[!X]    #gives columns containing categorical variables as it removes numeric
print(colnames(y)) #prints the column names that has categorical data
"sex"       "diagnosis" "loc"       "stereo"

print(unique(data$sex))
[1] Female Male #print only the unique categories present under the column sex
print(unique(data$diagnosis))
[1] "Meningioma" "HG glioma"  "LG glioma"  NA  "Other"     
> print(unique(data$loc))
[1] "Infratentorial" "Supratentorial"
> print(unique(data$stereo))
[1] "SRS" "SRT"

#using factors which is a special data type used for categorical variables and combining the levels of the columns together
data$sex <- factor(data$sex, levels = c( "Male","Female"))
data$diagnosis <- factor(data$diagnosis, levels = c( "Meningioma","HG glioma","LG glioma",NA,"Other"))
data$loc <- factor(data$loc, levels = c( "Infratentorial","Supratentorial"))
data$stereo <- factor(data$stereo, levels = c( "SRS","SRT"))
categorical_vars <- sapply(data, is.factor) #find columns that are factors and that contains the levels
#it returns TRUE for the columns that have categories and levels
X       sex diagnosis       loc        ki       gtv    stereo    status      time 
FALSE      TRUE      TRUE      TRUE     FALSE     FALSE      TRUE     FALSE     FALSE
print(sum(categorical_vars))
[1] 4 #returns the number of columns that have categories and levels, which returns TRUE
#to print the column names that have categorical variables
print(colnames(data)[categorical_vars]) 
[1] "sex"       "diagnosis" "loc"       "stereo" 
#to know the number of levels of each category and the levels themselves
for (var in colnames(data)[categorical_vars]) { #find the column names that have the categories from categorical_vars 
     print(paste("Variable:", var)) #print the column name that has the categories
     print(paste("Number of levels:", nlevels(data[[var]]))) #prints the number of unique characters or levels
     print(paste("Levels:", levels(data[[var]]))) #prints the unique categories of the levels
}
[1] "Variable: sex"
[1] "Number of levels: 2"
[1] "Levels: Male"   "Levels: Female"
[1] "Variable: diagnosis"
[1] "Number of levels: 4"
[1] "Levels: Meningioma" "Levels: HG glioma"  "Levels: LG glioma"  "Levels: Other"     
[1] "Variable: loc"
[1] "Number of levels: 2"
[1] "Levels: Infratentorial" "Levels: Supratentorial"
[1] "Variable: stereo"
[1] "Number of levels: 2"
[1] "Levels: SRS" "Levels: SRT"

#statistical parameters of the data
print(mean(data$gtv))
[1] 8.660795
print(mean(data$time))
[1] 27.4575
print(median(data$gtv))
[1] 6.51
print(mode(data$gtv))
[1] "numeric" #R doesnot have inbuilt function to compute mode
#to calculate mode, we use the following function
which.max(table(data$gtv))
2.5 #the number repeated maximum times is 2.5, so the mode is 2.5
20 #R keeps all the 2.5 in a bin and returns the index value of that bin, so here the index value is 20
print(sd(data$gtv))
[1] 8.657576
print(summary(data$gtv))
#Output
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.010   2.500   6.510   8.661  12.100  34.640 
hist(data$gtv) #to plot histogram with the data
#to calculate the skewness value, we need to install moments library
library(moments)
skewness(data$gtv)
[1] 1.37448
> kurtosis(data$gtv)
[1] 4.172248
#boxplot with default values
boxplot(data$gtv)
#provide values for boxplot
boxplot(data$gtv,xlab="Spread of GTV",ylab="GTV",border="blue",col="red")
#provide different range for boxplot
boxplot(data$gtv,range=0.1,xlab="Spread of GTV",ylab="GTV",border="blue",col="red")
boxplot(data$gtv,range=0.2,horizontal=FALSE,xlab="Spread of GTV",ylab="GTV",border="blue",col="red")
boxplot(data$gtv,range=0.05,horizontal=FALSE,xlab="Spread of GTV",ylab="GTV",border="blue",col="red")
#make boxplot with 2 more data sets: ki and time
boxplot(data$ki,border="red",col="black")
boxplot(data$time,border="grey",col="blue")

#building subsets of the data
#subset of gtv values > 20 and also print the dimensions
filter1 = subset(data, data$gtv > 20)
print(filter1)
print(dim(filter1))
[1] 11  9 #print the dimensions of the subset
#to print the details of the specific rows
filter2 = data[c(1, 3, 8, 9, 13, 14, 18, 21),]
print(filter2)
#indices of female sex entries by using which function
filter3_ind = which(data$sex == 'Female')
filter3 = data[filter3_ind, ] #we need to take all the columns, thus comma and space
print(filter3)
#build new data frame and a column with a specified formula
new_dataframe <- data.frame(GTV = data$gtv, KI = data$ki, new_column = data$gtv*data$ki/234)
print(new_dataframe)
#subset of the female entry data and writing the dataframe to a csv file
female_dataframe <- data.frame(filter3)
#write the above sets of data in another csv file and it will show in the folder of the path added
write.csv(female_dataframe, "C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/lab5_female_BrainCancer.csv")
