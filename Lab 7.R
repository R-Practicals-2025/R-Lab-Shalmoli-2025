#form matrix of dimensions 3X4
amat <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
print(amat)
      [,1] [,2] [,3] [,4]
[1,]   10   20   30   40
[2,]   50   60   70   80
[3,]   90  100  110  120
#put byrow = FALSE
amat2 <- matrix(seq(10, 120, by = 10), nrow = 4, ncol = 3, byrow = FALSE)
print(amat2)
      [,1] [,2] [,3]
[1,]   10   50   90
[2,]   20   60  100
[3,]   30   70  110
[4,]   40   80  120
#both the matrices are transposes of each other
identical(amat,t(amat2))
[1] TRUE
#assign row and column names
rownames(amat)<-c("R1","R2","R3")
dimnames(amat)<-list(rownames(amat),c("C1", "C2", "C3", "C4"))
print(amat)
   C1  C2  C3  C4
R1 10  20  30  40
R2 50  60  70  80
R3 90 100 110 120
#form a 4X4 A and B matrix and perform element wise and matrix multiplication
A <- matrix(c(2,5,7,3, 1,8,9,10, 1,12,5,10, 4,17,15,11), nrow=4, ncol=4, byrow=TRUE)
print(A)
      [,1] [,2] [,3] [,4]
[1,]    2    5    7    3
[2,]    1    8    9   10
[3,]    1   12    5   10
[4,]    4   17   15   11
B <- matrix(c(12,5,3,17, 1,18,9,10, 1,12,5,10, 4,15,15,4), nrow=4, ncol=4, byrow=TRUE)
print(B)
      [,1] [,2] [,3] [,4]
[1,]   12    5    3   17
[2,]    1   18    9   10
[3,]    1   12    5   10
[4,]    4   15   15    4
#element wise multiplication
A*B
     [,1] [,2] [,3] [,4]
[1,]   24   25   21   51
[2,]    1  144   81  100
[3,]    1  144   25  100
[4,]   16  255  225   44
#matrix multiplication
A %*% B
     [,1] [,2] [,3] [,4]
[1,]   48  229  131  166
[2,]   69  407  270  227
[3,]   69  431  286  227
[4,]  124  671  405  432
#take 2 vectors X and Y, and obtain both the outer and inner product
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
#outer product
X %o% Y
      [,1] [,2] [,3] [,4]
[1,]   40   50   60   25
[2,]   48   60   72   30
[3,]   64   80   96   40
[4,]   72   90  108   45
#inner product
sum(X * Y)
[1] 241 #the value of inner product of 2 vectors is always a single number
#form diagonal matrix by taking the elements of vector X
X <- c(5, 6, 8, 9)
diagonal <- diag(X)
print(diagonal)
      [,1] [,2] [,3] [,4]
[1,]    5    0    0    0
[2,]    0    6    0    0
[3,]    0    0    8    0
[4,]    0    0    0    9
#to print the diagonal elements of matrix A
diag(A)
[1]  2  8  5 11
#identity matrix of 6X6. Identity matrix are those that contain only 0 and 1
diag(6)
      [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    0    0    0    0    0
[2,]    0    1    0    0    0    0
[3,]    0    0    1    0    0    0
[4,]    0    0    0    1    0    0
[5,]    0    0    0    0    1    0
[6,]    0    0    0    0    0    1
#create a 3X3 matrix A and a 3X1 matrix B
A <- matrix(c(3,4,-2, 4,-5,1, 10,-6,5), nrow=3, ncol=3) 
print(A) #by default it takes byrow=TRUE
      [,1] [,2] [,3]
[1,]    3    4   -2
[2,]    4   -5    1
[3,]   10   -6    5
B <- matrix(c(5,-3,13), nrow=3, ncol=1)
print(B)
      [,1]
[1,]    5
[2,]   -3
[3,]   13
#find unknown vector X of the equation AX=B
X <- solve(A, B)
print(X) #so here X is basically the inverse of A multipled by B
      [,1]
[1,]    1
[2,]    2
[3,]    3
#to know what type of object is X
print(class(X))
[1] "matrix" "array"
#to know how X is stored in the memory
print(typeof(X))
[1] "double"
#to find inverse of the matrix A
Ainv <- solve(A)
print(Ainv)
         [,1]        [,2]       [,3]
[1,]  0.12751678  0.05369128 0.04026846
[2,]  0.06711409 -0.23489933 0.07382550
[3,] -0.17449664 -0.38926174 0.20805369
#to check whether it is really an inverse matrix by calculating the matrix product of Ainv*A. It should give an identity matrix
identity_check <-Ainv %*% A
         [,1]          [,2]          [,3]
[1,] 1.000000e+00 -1.110223e-16 -5.551115e-17
[2,] 2.220446e-16  1.000000e+00  5.551115e-17
[3,] 4.440892e-16  0.000000e+00  1.000000e+00
#to check that the multiplication of inverse and original matrix (3X3 matrix) should give an identity matrix
all.equal(identity_check, diag(3))
[1] TRUE #shows that both Ainv is really an inverse of A, since the matrix multiplication gave an identity matrix
#eigen values and eigen function of A
results <- eigen(A)
print(results)
eigen() decomposition
$values
[1]  4.126810+3.36613i  4.126810-3.36613i -5.253621+0.00000i

$vectors
                  [,1]                   [,2]          [,3]
[1,] -0.00651792-0.3726404i -0.00651792+0.3726404i  0.2061797+0i
[2,] -0.14331740-0.1104588i -0.14331740+0.1104588i -0.7435073+0i
[3,] -0.91014046+0.0000000i -0.91014046+0.0000000i -0.6361500+0i
#type of class that results is
print(class(results))
[1] "eigen"
#matrix vector multiplication of the second eigen vector
mult <- results$vectors[,2] #take the 2nd eigen vector
multiplication <- A %*% mult
print(multiplication)
[,1]
[1,]  1.2274575+1.5597562i
[2,] -0.2196251+0.9382675i
[3,] -3.7559771+3.0636508i

#read data and store as data frame
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/BrainCancer.csv",header=TRUE)
#creating a new column
newcol<-(data$gtv^2) + (data$time)
newcoladd<-data.frame(data,newcol)
print(newcoladd)
#print the row and column names
print(rownames(newcoladd))
print(colnames(newcoladd))
[1] "X"         "sex"       "diagnosis" "loc"       "ki"        "gtv"       "stereo"   
[8] "status"    "time"      "newcol"  
#change the row names to row 1,2,3 and so on
rownames(data) <- paste("Row", 1:nrow(data), sep="-")
print(rownames(data))
#remove the ki column and add NULL
data$ki <- NULL
print(head(data)) #head function to print the 1st 10 rows

#reading excel files
install.packages("readxl") #to install and read excel files in R
#load the package
library(readxl)
#read the excel file to a dataframe
data<- read_excel("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/pone.0148733.s001.xlsx",1)
# the 1 indicates the first sheet of the excel file
#print dimensions and column names of the data
print(colnames(data))
[1] "ID"                   "Sex"                  "Diagnosis"            "Location"            
[5] "KI"                   "GTV"                  "Stereotactic methods" "status"              
[9] "OS"                  
print(dim(data))
[1] 88  9

#sets and operations on sets
#define two vectors A and B and print them
A <- c("a", "b", "c", "d", "e")
B <- c("d", "e", "f", "g")
print(A)
[1] "a" "b" "c" "d" "e"
print(B)
[1] "d" "e" "f" "g"
#union operation of the two sets
union(A, B)
[1] "a" "b" "c" "d" "e" "f" "g"
#perform intersection operation
intersect(A, B)
[1] "d" "e"
#difference operation between the two sets
setdiff(A, B)
[1] "a" "b" "c" #elements of A which are not present in B
setdiff(B, A)
[1] "f" "g" #elements of B which are not present in A
#to check for equality of the sets
print(c(setdiff(A,B), intersect(A,B), setdiff(B,A)))
[1] "a" "b" "c" "d" "e" "f" "g"
print(setequal(c(setdiff(A, B), intersect(A, B), setdiff(B, A)), union(A, B))) 
[1] TRUE #proves that the operations is same as the union of two sets
#elements of B present in A
B %in% A
[1]  TRUE  TRUE FALSE FALSE #returns TRUE for the elements present in both the sets
intersect(B,A)
[1] "d" "e"
print(B[B %in% A])
[1] "d" "e"
#elements of A present in B
print(A[A %in% B])
[1] "d" "e"

#create vectors with elements and print the filtered data
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
print(vec[vec>12])
[1] 14 16 19 20
print(vec[vec>10 & vec<20])
[1] 12 14 16 19
#form an array A with elements
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
#create another array where NA has been removed and the values are less than 100
newarr<-A[!is.na(A) & A<100] #first we are checking if NA are present in the list and then removing it with the help of NOT operator
print(newarr)
[1]  2  7 29 32 41 11 15 55 32 42
#assign 0 to NA and print the array
A2<-A
> A2
[1]   2   7  29  32  41  11  15  NA  NA  55  32  NA  42 109
A2[is.na(A2)] <-0
print(A2)
[1]   2   7  29  32  41  11  15   0   0  55  32   0  42 109
#create 2 vectors, one with gene names and another with genders
genes <- paste("gene", 1:6, sep="-")
print(genes)
[1] "gene-1" "gene-2" "gene-3" "gene-4" "gene-5" "gene-6"
gender <- c("M","M","F","M","F","F","M")
print(gender)
[1] "M" "M" "F" "M" "F" "F" "M"
#enter the data as 7 vectors
result1 = c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 = c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 = c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 = c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 = c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 = c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 = c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)
#create dataframe with the columns
datframe <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)
print(datframe) #we have to provide same number of rows in all the entries, otherwise it will throw error.
#change the genes to 7 rows
#add column names to this dataframe
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")
print(datframe)
#create subsets of this data
subset(datframe, expt2 > 20)
#subset with only female gender
subset(datframe, Gender == "F" )
#male gender subset where expt 2 is < 30
subset(datframe, Gender == "M" & expt2 < 30)

#if-else-if structure
#to take user input in R, we use the readline() function
#should enter only numeric value for the angle values
 angle <- as.numeric(readline("Enter an angle in degrees")) #first take the input of the angle and then execute the other code
 if (angle >= 0 & angle < 90) {
  print("First quadrant")
} else if (angle >= 90 & angle < 180) {
  print("Second quadrant")
} else if (angle >= 180 & angle < 270) {
  print("Third quadrant")
} else if (angle >= 270 & angle < 360) {
  print("Fourth quadrant") 
} else {
  print("Invalid angle")
}
 #take 3 numeric entry and sort them in decreasing order
 number1<-as.numeric(readline("Enter the first number"))
 number2<-as.numeric(readline("Enter the second number"))
 number3<-as.numeric(readline("Enter the third number"))
 if (number1>number2 & number1>number3) {
   if (number2>number3) {
     print(c(number1,number2,number3))
   }
   else {
     print(c(number1,number3,number2))
   }
 } else if (number2 > number1 & number2 > number3) {      
   if (number1 > number3) {        
     print(c(number2, number1, number3))
   } else {  
     print(c(number2, number3, number1))
   }
 } else {     
   if (number1 > number2) {      
     print(c(number3, number1, number2))   
   } else {  
     print(c(number3, number2, number1))  
   }  
 }  
 #calculation of cost of a journey based on an individual's age and distance travelled
 distance <- as.numeric(readline("Enter the journey distance in km"))
 age <- as.numeric(readline("Enter the traveller's age"))
 if (distance <= 100) {
   cost <- 100
 } else if (distance <= 1000) {
   cost <- 100 + (distance - 100) * 1.50 #after 100 km, the cost is 1.5 rs per km
 } else {
   cost <- 100 + (900 * 1.50) + (distance - 1000) * 2 #for the first 100 km, cost is rs 100, then for 900 km the cost is 1.5 rs per km
   #then for the last part, the distance more than 1000 km, the cost is rs 2 per km
 }
 if (age > 60) {
   cost <- cost-(cost * 0.25) #for senior citizens, the concession is 25%   
 } else if (age < 6) {
   cost <- cost-(cost * 0.5)  #for child, the concession is 50%
 }
 print(paste("Ticket cost: Rs.",cost))
 
 #writing functions
 #function to replace all the negative values in a vector by zero
 negative <- function(vec) {
   vec[vec < 0] <- 0
   return (vec) #returning the vector and passing it to the function negative
 }
 negative(c(22, 30, NA, -85, -97, -8, NA, 15, 3, NA)) #giving the input of the negative variable function
 #output
 [1] 22 30 NA  0  0  0 NA 15  3 NA
 #function to sum the digits of a number
 digits <- function(number) {      
   num <- abs(number) #absolute function removes the negative part of the number
   sum <- 0
   while (number > 0){
     sum <- sum + (number %% 10) #takes the last digit and add to sum.
     number <- number %/% 10 #provides with the quotient after removing the last digit
   }
   return (sum)
 }
 digits(223019851997) 
 #output
 [1] 56
 #calculate factorial using Sterling's approximation
 stirling <- function(n) {
   if (n < 0 || !is.numeric(n)){
     stop("Enter non-negative no.") #stops the function since the number has to be numeric and can't be negative
   } else {
     if (n == 0) { #since we know 0!=1
       return (1)
     } else {
       fac <- (n**n)*(exp(-n))*(sqrt(2*pi*n))*(1+(1/(12*n))+(1/(288*n**2))-(139/(51840*n**3))-(571/(2488320*n**4)))
       #compute the factorial with the given formula
     }
   }
   return (fac)
 }
 stirling(4)
 #output
 [1] 23.99998 #gives the output as 23.99, which is almost equal to the actual factorial of 4, which is 24
 