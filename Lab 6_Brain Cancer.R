#read the csv file
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/BrainCancer.csv",header=TRUE)
print(data)
#change the current class to the class factor
print(class(data$sex))
[1] "character"
data$sex <- as.factor(data$sex) 
#to check it is a factor class
print(is.factor(data$sex))
[1] TRUE
#number of levels in the category sex
print(nlevels(data$sex))
[1] 2
#levels in the category diagnosis
data$diagnosis<-factor(data$diagnosis,levels=c("Meningion","HG glioma","LG glioma","Other"))
> print(levels(data$diagnosis))
[1] "Meningion" "HG glioma" "LG glioma" "Other" 

#factor levels using gl function
temperature<-gl(3,1,88,labels=c("Hot","Cold","Lukewarm")) #there are 3 labels and they are repeated once for 88 times
braindata<-data.frame(data,temperature) #creating a newdata frame and adding this category of data
print(braindata)

#using the tapply function
tapply(data$gtv, data$ki, mean) #tapply is used for group wise calculations
40        60        70        80        90       100 
22.870000 15.744000  9.750714  9.093611  5.683333  8.646000 #it takes the unique values of ki and computes a mean of all the gtv values present under that particular ki value
tapply(data$gtv, data$ki, mean, trim=0.1)
40        60        70        80        90       100 
22.870000 15.744000  8.567500  7.988000  4.494348  8.646000 #it removes 10% of the extreme values from each group and then computes the mean

#to check for the trim function on individual ki value.
#Lets take ki value of 80 and check for the mean after trimming to know whether the value is matching with the value of tapply trim function
subdata1 <- subset(data$gtv, data$ki==80) #creating a subset of the gtv value with ki=80
print(subdata1)
#sort the subset in ascending order
sorted_subdata <- sort(subdata1)
print(sorted_subdata)
#printing the mean of the sorted subdata
mean_subdata_trimmed <- mean(sorted_subdata, trim=0.1)
print(mean_subdata_trimmed)
[1] 7.988
#even if we don't sort the data, still the mean comes same after applying the trim function
mean_subdata_trimmed <- mean(subdata1, trim=0.1)
print(mean_subdata_trimmed)
[1] 7.988

#finding parallel max and min values
print(pmin(data$gtv, data$ki, data$time)) #checks for the smallest value at each position across the 3 vectors
print(pmax(data$gtv, data$ki, data$time)) #checks for the largest value at each position across the 3 vectors

#difference between rank, sort and order
ranks <- rank(data$gtv) #assigns the rank of the gtv values present in the original data, starting from ascending order
sorted <- sort(data$gtv) #sorts the original data in as cending order
ordered <- order(data$gtv) #provides the index number of the sorted data by checking for the index number of the values in the original data
view <- data.frame(gtv=data$gtv, ranks, sorted, ordered) #creating a new dataframe with the columns
print(view) 
#to print the diagnosis data using the indices from the ordered vector, meaning checking the indices from the ordered vector and check what is there in the diagnosis at at particular index
diagnosis_ordered <- data$diagnosis[ordered]
print(diagnosis_ordered)
#create a new data frame with the ordered gtv and diagnosis column
ordered_data <- data.frame(ordered_gtv = data$gtv[ordered], ordered_diagnosis = diagnosis_ordered)
#write this dataframe to a new csv file
write.csv(ordered_data, "C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/lab4_ordered_data.csv")

#converting dataframes into matrices
#extract specific rows and columns
filter1 = data[1:6, 3:8] #extract 1 to 6 rows and 3 to 8 columns
print(filter1)
#convert the data into matrix and check if it is a matrix
convert<-as.matrix(filter1)
print(class(convert))
[1] "matrix" "array" 
#checking the attributes
print(attributes(convert))
$dim
[1] 6 6 #gives the dimensions
$dimnames
$dimnames[[1]]
[1] "1" "2" "3" "4" "5" "6" #states the column names
$dimnames[[2]] #double square brackets states that it is a list
[1] "diagnosis" "loc"       "ki"        "gtv"       "stereo"    "status" #states the row names
#create a new column by adding three columns
newcol<-data$ki + data$gtv + data$time
#create a new dataframe by adding this column to the last
newcoldadded = data.frame(data, newcol)
print(newcoldadded)
#print the column names
print(colnames(newcoladded))
[1] "X"         "sex"       "diagnosis" "loc"       "ki"        "gtv"       "stereo"   
[8] "status"    "time"      "newcol"  
#another way to add columns by using cbind function
newcoladded2=cbind(data,newcol)
print(newcoladded2)
print(colnames(newcoladded)) #the new column has been added
[1] "X"         "sex"       "diagnosis" "loc"       "ki"        "gtv"       "stereo"   
[8] "status"    "time"      "newcol"  
#pick rows 26 and 35 from the original data using data frame and add this to the original data
select<-data.frame(data[c(26,35),])
newrowadded=rbind(data,select) #rbind function is used to add new rows
print(newrowadded)
#dimensions of the newly created dataframe
print(dim(newrowadded))
[1] 90  9

#add rows and column names
#create a 4X5 matrix
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow = 4, byrow = TRUE)
print(X)
[,1] [,2] [,3] [,4] [,5]
[1,]    1    0    2    5    3
[2,]    1    1    3    1    3
[3,]    3    1    0    2    2
[4,]    1    0    2    1    0
print(rownames(X))
NULL #since there are no row names
rownames(X) <- rownames(X, prefix='Trial-')
print(rownames(X))
[1] "Trial-1" "Trial-2" "Trial-3" "Trial-4"
#print the matrix with the row names
print(X)
[,1] [,2] [,3] [,4] [,5]
Trial-1    1    0    2    5    3
Trial-2    1    1    3    1    3
Trial-3    3    1    0    2    2
Trial-4    1    0    2    1    0
#assign column names as drugs
drugs <- c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(X)<-drugs
print(X)
            aspirin paracetamol nurofen hedex placebo
Trial-1       1           0       2     5       3
Trial-2       1           1       3     1       3
Trial-3       3           1       0     2       2
Trial-4       1           0       2     1       0
#assign column names using dimnames
dimnames(X) <- list(NULL, paste("drug", 1:5, sep="."))
print(X)
        drug.1 drug.2 drug.3 drug.4 drug.5
[1,]      1      0      2      5      3
[2,]      1      1      3      1      3
[3,]      3      1      0      2      2
[4,]      1      0      2      1      0
#to indicate both the row and column names in the matrix
dimnames(X) <- list(paste("Trial", 1:4, sep=" "), paste("Drug", 1:5, sep=" "))
print(X)
           Drug 1 Drug 2 Drug 3 Drug 4 Drug 5
Trial 1      1      0      2      5      3
Trial 2      1      1      3      1      3
Trial 3      3      1      0      2      2
Trial 4      1      0      2      1      0

#calculations on rows and columns
print(mean(X[,5]))
[1] 2 #calculates the mean of the 5th column
var(X[4,])
[1] 0.7 #calculates the variance of the 4th row
#calculate the sum of individual rows
print(rowSums(X))
Trial 1 Trial 2 Trial 3 Trial 4 
11       9       8       4 
#another alternative way is to use the apply function
apply(X,1,sum) #here X denotes the matrix and 1 signifies the sum operation has to be done row-wise
#perform sum column wise
colSums(X)
Drug 1 Drug 2 Drug 3 Drug 4 Drug 5 
6      2      7      9      8 
#alternate way
apply(X,2,sum) #the 2 signifies that the operation has to be done column wise
#to calculate mean row-wise
rowMeans(X)
Trial 1 Trial 2 Trial 3 Trial 4 
2.2     1.8     1.6     0.8 
#alternate way
apply(X,1,mean)
#to perform mean column wise 
colMeans(X)
Drug 1 Drug 2 Drug 3 Drug 4 Drug 5 
1.50   0.50   1.75   2.25   2.00 
#alternate way
apply(X,2,mean)

#sum groups of rows within a column
group = c("A","B","B","A") #grouping rows 1 and 4 as A and rows 2 and 3 as B
print(rowsum(X, group))
    Drug 1 Drug 2 Drug 3 Drug 4 Drug 5
A      2      0      4      6      3
B      4      2      3      3      5
print(row(X)) #every row has the row index
      [,1] [,2] [,3] [,4] [,5]
[1,]    1    1    1    1    1
[2,]    2    2    2    2    2
[3,]    3    3    3    3    3
[4,]    4    4    4    4    4
print(col(X)) #every column has the column index
      [,1] [,2] [,3] [,4] [,5]
[1,]    1    2    3    4    5
[2,]    1    2    3    4    5
[3,]    1    2    3    4    5
[4,]    1    2    3    4    5
#alternate way to group the elements in the row and provide the sum
print(tapply(X, list(group[row(X)], col(X)), sum)) #groups the rows based on the group vector and then provides sum
  1 2 3 4 5
A 2 0 4 6 3
B 4 2 3 3 5
#more sophisticated way to perform the sum of rows based on groups, by using the aggregate function
print(aggregate(X, list(group), sum)) 
Group.1 Drug 1 Drug 2 Drug 3 Drug 4 Drug 5
1       A      2      0      4      6      3
2       B      4      2      3      3      5
