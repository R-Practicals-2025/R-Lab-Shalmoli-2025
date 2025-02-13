#shuffling the elements of a column
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=5, byrow=FALSE)
print(X)
      [,1] [,2] [,3] [,4]
[1,]    1    1    3    1
[2,]    0    1    1    0
[3,]    2    3    0    2
[4,]    5    1    2    1
[5,]    3    3    2    0
apply(X,2,sample) #sample is the function for shuffling
      [,1] [,2] [,3] [,4]
[1,]    2    1    2    0
[2,]    3    3    0    0
[3,]    0    1    3    1
[4,]    1    3    2    2
[5,]    5    1    1    1
#shuffle the elements of a row
apply(X,1,sample) #this output transposes the rows into columns and thus the dimensions of the matrix changes
      [,1] [,2] [,3] [,4] [,5]
[1,]    3    1    3    1    0
[2,]    1    0    2    2    3
[3,]    1    1    0    5    3
[4,]    1    0    2    1    2
#as we can see the dimensions of the matrix changed, so to restore and keep the original dimension
#we will use t() function, which will transpose the output of apply(X,1,sample), back to the dimensions of the original matrix
print(t(apply(X,1,sample)))
      [,1] [,2] [,3] [,4]
[1,]    3    1    1    1
[2,]    0    1    0    1
[3,]    0    2    3    2
[4,]    2    1    1    5
[5,]    3    3    0    2

#add row at the bottom of a matrix, by computing the mean of each column
X2 <- rbind(X, apply(X,2,mean)) #here 2 is written since mean of individual columns are computed
print(X2)
     [,1] [,2] [,3] [,4]
[1,]  1.0  1.0  3.0  1.0
[2,]  0.0  1.0  1.0  0.0
[3,]  2.0  3.0  0.0  2.0
[4,]  5.0  1.0  2.0  1.0
[5,]  3.0  3.0  2.0  0.0
[6,]  2.2  1.8  1.6  0.8
#to add a column that computes the variance row-wise
X3 <- cbind(X,apply(X,1,var)) #here 1 is written since the variance is computed row-wise
print(X3)
     [,1] [,2] [,3] [,4]      [,5]
[1,]  1.0  1.0  3.0  1.0 1.0000000
[2,]  0.0  1.0  1.0  0.0 0.3333333
[3,]  2.0  3.0  0.0  2.0 1.5833333
[4,]  5.0  1.0  2.0  1.0 3.5833333
[5,]  3.0  3.0  2.0  0.0 2.0000000
[6,]  2.2  1.8  1.6  0.8 0.3466667
#assign headings to the columns and rows, including mean and variance
headings <- c(paste("drug.",1:5,sep=""),"var") #provides column names as drug 1,2 and so on, along with the variance column
dimnames(X3)<- list(rownames(X3),headings) #dimnames function assign the column names and the row number indexes remain as it is
print(X3)
      drug.1 drug.2 drug.3 drug.4 drug.5     var
[1,]    1.0    0.0   2.00   5.00      3 3.70000
[2,]    1.0    1.0   3.00   1.00      3 1.20000
[3,]    3.0    1.0   0.00   2.00      2 1.30000
[4,]    1.0    0.0   2.00   1.00      0 0.70000
[5,]    1.5    0.5   1.75   2.25      2 0.45625
#to assign rownames
headings2 <- c(paste("Trial-",1:4,sep=""),"Mean")
rownames(X3)<-headings2 #rownames has been assigned as Trial 1,2 and so on, along with the mean row
print(X3)
        drug.1 drug.2 drug.3 drug.4 drug.5     var
Trial-1    1.0    0.0   2.00   5.00      3 3.70000
Trial-2    1.0    1.0   3.00   1.00      3 1.20000
Trial-3    3.0    1.0   0.00   2.00      2 1.30000
Trial-4    1.0    0.0   2.00   1.00      0 0.70000
Mean       1.5    0.5   1.75   2.25      2 0.45625

#to perform sweep function
eg_sweep<-data.frame(data$ki, data$gtv, data$time) #dataframe containing 3 columns from the brain cancer data
#perform mean of each column
column<-apply(eg_sweep,2,mean) #here 2 means that column wise mean is evaluated
print(column)
data.ki  data.gtv data.time 
81.022727  8.660795 27.457500 
#to print the column means, where each column mean is repeated for each row by "rep" function
#dim(eg_sweep)[1] means the number of rows in eg_sweep
#dim(eg_sweep)[2] means the number of columns in eg_sweep
#nrow=dim(eg_sweep)[1] creates a matrix with the same number of rows as eg_sweep
column.means <- matrix(rep(column,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),nrow=dim(eg_sweep)[1])
print(column.means)
#substract the column means from the original matrix
eg_sweep_alt <- eg_sweep - column.means
#this is method 1 where sweep function is not used
print(eg_sweep_alt)
#method 2 where sweep function is used
eg_sweep1 <- sweep(eg_sweep,2,column) #subtracts the column means for each element from the original matrix
#the 2 signifies that the operation has to be done column wise

#using max.col function
#read the data pgfull text
data <- read.table("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/pgfull.txt",header=TRUE)
#create a subset by extracting certain columns
species <- data[, 1:54] #the first comma indicates that we are considering all the rows
print(species)
#using the function max.col to find the column index of the maximum value in each row
max.col(species)
#using these indices to find the names of the species
names(species)[max.col(species)]
#table function to build frequency table of each of the species
table(names(species)[max.col(species)])
#since there is no function to find min.col, we have to use an alternative
#to find the minimum value along each row, we have to put minus infront of the data set, in which we want to find the column index with minimum value and then apply max.col function
min_indices <- max.col(-species)
print(min_indices)

#introduction to list
apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl", "oolite", "CaCO3")
cheese <- c(3.2 - 4.5i, 12.8 + 2.2i )
items <- list(apples, oranges, chalk, cheese)
print(items)
[[1]]
[1] 4.0 4.5 4.2 5.1 3.9
[[2]]
[1]  TRUE  TRUE FALSE
[[3]]
[1] "limestone" "marl"      "oolite"    "CaCO3"    
[[4]]
[1]  3.2-4.5i 12.8+2.2i
#access the list and the elements of individual list.
#double square brackets shows that it is a list and single square brackets shows the individual items in that particular list
print(items[[3]]) #access the 3rd list
[1] "limestone" "marl"      "oolite"    "CaCO3"    
print(items[[3]][3]) #access the 3rd element of the 3rd list
[1] "oolite"
#creating dataframe of the 4 objects given above
df <- data.frame(apples, oranges, chalk, cheese) #this will give error since the number and type of arguments are different in each objects of the list
#difference between single and double square brackets
print(items[3]) #returns the list containing the 3rd vector
[[1]]
[1] "limestone" "marl"      "oolite"    "CaCO3"    

print(items[[3]]) #returns the 3rd vector itself
[1] "limestone" "marl"      "oolite"    "CaCO3" 
#list can be named and can also be accessed by using the dollar symbol
print(names(items))
NULL #prints NULL since the list is not named
#naming the items in the list
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth) #printing the 4th list and its elements
[1]  3.2-4.5i 12.8+2.2i
#to know that it is actually a list
print(class(items))
[1] "list"
#using the function lapply() in list
print(lapply(items,length)) #prints the length of each elements in each list
$first
[1] 5
$second
[1] 3
$third
[1] 4
$fourth
[1] 2
#to know the class of each list
print(lapply(items,class))
$first
[1] "numeric"
$second
[1] "logical"
$third
[1] "character"
$fourth
[1] "complex"
#to print the mean of individual items in the list
print(lapply(items,mean))
$first
[1] 4.34
$second
[1] 0.6666667 #provides mean of the boolean list by assigning TRUE as 1 and FALSE as 0
$third
[1] NA #returns NA since the this specific items contains only characters
$fourth
[1] 8-1.15i
#using the summary and str function in a list
print(summary(items))
     Length    Class  Mode     
first  5      -none- numeric  
second 3      -none- logical  
third  4      -none- character
fourth 2      -none- complex 
#here mode signifies the data type of each objects in a list
#using the structure function
print(str(items))
List of 4
$ first : num [1:5] 4 4.5 4.2 5.1 3.9
$ second: logi [1:3] TRUE TRUE FALSE
$ third : chr [1:4] "limestone" "marl" "oolite" "CaCO3"
$ fourth: cplx [1:2] 3.2-4.5i 12.8+2.2i