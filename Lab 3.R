Question 4:
  
  vec <- c(4,7,6,5,6,7)
  [1] 4 7 6 5 6 7
  class(vec) #check type of vector
  [1] "numeric"
  length(vec) #check the total number of characters in the vector
  [1] 6
  max(vec)
  [1] 7 #checks the maximum value present in the vector
  min(vec)
  [1] 4 #checks the minimum value present in the vector
  
  vec <- scan()
  1: 22
  2: 30
  3: 85
  4: 97
  5: 5
  6: 11
  7: 
    Read 6 items #takes user input and an extra enter to signify the end of the vector
  
  vec[4]
  [1] 97 #extracts the 4th character and indexing starts from 1
  ind <- c(2,3,6) #extracting multiple elements by providing the index numbers
  vec[ind]
  [1] 30 85 11
  vec[c(2,3,6)]
  [1] 30 85 11 #another way to extract multiple elements
  
  vec[-1]
  [1] 30 85 97  5 11 #removes the first element of the vector by applying the minus sign and returns the entire vector
  
  vec[-length(vec)]
  [1] 22 30 85 97  5 #removes the last element of the vector and prints the entire vector
  
  trim <- function(v) #function called trim to remove the largest two and smallest two values
     sort(v)[-c(1, 2, (length(v) - 1), length(v))] #sort the vector in ascending order and print the vector excluding two smallest and largest numbers
  trim(vec) #call trim function with vec
  [1] 22 30
  
  sort(vec)[c(1, 2, (length(vec) - 1), length(vec))]
  [1]  5 11 85 97 #print the vector with only two smallest and largest numbers
  
  vec[1:3]
  [1] 22 30 85 #extract elements from indices 1 to 3
  
  vec[seq(2,length(vec),2)]
  [1] 30 97 11 #extract elements from indices 2 to the length of the vector, incremented by 2
  
  vec[-seq(1, length(vec), 2)]
  [1] 30 97 11 #removes elements from odd indices using the minus sign and prints the elements of the even indices
  
  #another way to achieve this
  vec[1:length(vec) %% 2 == 0]
  [1] 30 97 11
  
  x <- 0:10 #create a vector to achieve the sum of elements less than 5
  sum(x[x < 5])
  [1] 10
  
  #another way to achieve this
  subset(x, x < 5) #creating a subset of the vector with numbers less than 5
  [1] 0 1 2 3 4
  sum(subset(x, x < 5)) #computing sum of the subset
  [1] 10
  
  #we can also use the which operator to take out the values less than 5 from the vector
  x[which(x < 5)]
  [1] 0 1 2 3 4
  sum(x[which(x < 5)])
  [1] 10 #then compute the sum
  
  #sum of 3 largest value in a vector
  y<-c(56,89,41,12,30,22)
  sort(y) #sort in ascending order
  [1] 12 22 30 41 56 89
  sum(sort(y)[(length(y)-2):length(y)])
  [1] 186 #compute the sum after sorting
  
  #sort in descending order and then compute the sum
  sort(y, decreasing = TRUE)[1:3]
  [1] 89 56 41
  sum(sort(y, decreasing = TRUE)[1:3])
  [1] 186
  
  #find index of vector corresponding to maximum or minimum value
  which.max(y)
  [1] 2
  which.min(y)
  [1] 4
  
  #combining vectors as columns
  cbind(1:10,10:1)
  [,1] [,2]
  [1,]    1   10
  [2,]    2    9
  [3,]    3    8
  [4,]    4    7
  [5,]    5    6
  [6,]    6    5
  [7,]    7    4
  [8,]    8    3
  [9,]    9    2
  [10,]   10    1
  
  #combining vectors as rows
  rbind(1:10,10:1)
  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  [1,]    1    2    3    4    5    6    7    8    9    10
  [2,]   10    9    8    7    6    5    4    3    2     1
  
  #basic operations of vector
  X <- c(1:10)
  [1]  1  2  3  4  5  6  7  8  9 10
  Y <- c(1:10*5)
  [1]  5 10 15 20 25 30 35 40 45 50
  X*Y
  [1]   5  20  45  80 125 180 245 320 405 500
  X+Y
  [1]  6 12 18 24 30 36 42 48 54 60
  X/Y
  [1] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
  X^Y
  [1] 1.000000e+00 1.024000e+03 1.434891e+07 1.099512e+12 2.980232e+17 2.210739e+23 3.788187e+29
  [8] 1.329228e+36 8.727964e+42 1.000000e+50
  log(X)
  [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379 1.7917595 1.9459101 2.0794415 2.1972246
  [10] 2.3025851
  exp(Y)
  [1] 1.484132e+02 2.202647e+04 3.269017e+06 4.851652e+08 7.200490e+10 1.068647e+13 1.586013e+15
  [8] 2.353853e+17 3.493427e+19 5.184706e+21
  
  #creating array
  #3D array with dimensions M[i, j, k]:
  # i refers to the number of rows.
  # j refers to the number of columns.
  # k refers to the number of times the matrix is repeated
  y <- 1:24
  dim(y) <- c(2,4,3) #create matrix with dimensions 2x4x3
  y
  , , 1
  
  [,1] [,2] [,3] [,4]
  [1,]    1    3    5    7
  [2,]    2    4    6    8
  
  , , 2
  
  [,1] [,2] [,3] [,4]
  [1,]    9   11   13   15
  [2,]   10   12   14   16
  
  , , 3
  
  [,1] [,2] [,3] [,4]
  [1,]   17   19   21   23
  [2,]   18   20   22   24
  
  #by default the elements in the matrix are arranged by column method
  X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3)
   [,1] [,2] [,3]
[1,]1    0    0
[2,]0    1    0
[3,]0    0    1
  
  #arranging the elements in the matrix by row
  vector <- c(1,2,3,4,4,3,2,1)
  > V <- matrix(vector,byrow=T,nrow=2)
  [,1] [,2] [,3] [,4]
[1,]1    2    3    4
[2,]4    3    2    1
  #arranging by column
  V <- matrix(vector,byrow=F,nrow=2)
  [,1] [,2] [,3] [,4]
  [1,]    1    3    4    2
  [2,]    2    4    3    1
  
  #convert vector to matrix using dim() function
  dim(vector) <- c(4,2)
  is.matrix(vector)
  [1] TRUE #returns TRUE since the vector is converted to matrix
  
  #common functions with vectors
  vec <- c(1, 2, 3, 4, 5)
  min(vec)
  [1] 1
  max(vec)
  [1] 5
  sum(vec)
  [1] 15 #sum of all the elements
  range(vec)
  [1] 1 5 #returns the maximum and minimum value
  sort(vec)
  [1] 1 2 3 4 5 #sorts in ascending order
  colMeans(as.matrix(vec))
  [1] 3 #calculates the mean of the column after converting the vector into a matrix (sum/5)
  as.matrix(vec) #converting the vector into matrix
  [,1]
  [1,]    1
  [2,]    2
  [3,]    3
  [4,]    4
  [5,]    5
  
  #Matrix multiplication
  #computing the outer product
  X <- c(2, 4, 6, 8)
  Y <- c(1, 3, 5)
  Z <- X[1:4] %o% Y[1:3] 
  [,1] [,2] [,3]
  [1,]    2    6   10
  [2,]    4   12   20
  [3,]    6   18   30
  [4,]    8   24   40
  #reverse the outer product
  YoX <- Y[1:3] %o% X[1:4] #this is actually the transpose of the given matrix Z
  [,1] [,2] [,3] [,4]
  [1,]    2    4    6    8
  [2,]    6   12   18   24
  [3,]   10   20   30   40
  
  #transpose of Z
  t(Z)
  [,1] [,2] [,3] [,4]
  [1,]    2    4    6    8
  [2,]    6   12   18   24
  [3,]   10   20   30   40
  
  #transpose of YoX
  t(YoX)
  [,1] [,2] [,3]
  [1,]    2    6   10
  [2,]    4   12   20
  [3,]    6   18   30
  [4,]    8   24   40
  
  #take 2 matrix and compute the dot product
  X <- c(2, 4, 6, 8,10,12,14,18,20,22) #take vector X
  X2 <- X[1:9] #take first 9 elements from vector X
  [1]  2  4  6  8 10 12 14 18 20
  X2 <- as.matrix(X) #convert the vector into a matrix
  dim(X2) <- c(3, 3) #convert X2 into a 3x3 matrix
  X2
  [,1] [,2] [,3]
  [1,]    2    8   14
  [2,]    4   10   18
  [3,]    6   12   20
  Y <- c(1, 3, 5,7,9,11,13,15,17,19)
  Y2 <- Y[1:9]
  [1]  1  3  5  7  9 11 13 15 17
  Y2 <- as.matrix(Y2)
  dim(Y2) <- c(3, 3)
  Y2
  [,1] [,2] [,3]
  [1,]    1    7   13
  [2,]    3    9   15
  [3,]    5   11   17
  #compute dot product of matrix
  X2 %*% Y2
  [,1] [,2] [,3]
  [1,]   96  240  384
  [2,]  124  316  508
  [3,]  142  370  598
  
  #perform dot product to vectors and not matrix
  sum(X2 * Y2)
  [1] 1114 #compute element wise multiplication of X2 and Y2 and add it up
  
  #compute cross product of matrix
  crossprod(X2, Y2) #so here we take the transpose of X2 and multiply with matrix Y2
  [,1] [,2] [,3]
  [1,]   44  116  188
  [2,]   98  278  458
  [3,]  168  480  792
  
  #create a 4x4 identity matrix
  diag(4)
  [,1] [,2] [,3] [,4]
  [1,]    1    0    0    0
  [2,]    0    1    0    0
  [3,]    0    0    1    0
  [4,]    0    0    0    1
  
  #to check the type of variables in X2 and X, or any other variable for instance
  class(X)
  [1] "numeric"
  class(X2)
  [1] "matrix" "array"
  