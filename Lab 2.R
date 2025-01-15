Question 1:
  round(12.1343,digits=3)
[1] 12.134 #rounds off the number to 3 places after decimal

round(123.12344,digits=3)
[1] 123.123

round(1234.12344,digits=3)
[1] 1234.123

round(12345.12344,digits=3)
[1] 12345.12 #R has limits on the number of digits it displays for large numbers
#change the environment options of number of digits displayed

options(digits=15)
round(12345.12344,digits=3)
[1] 12345.123 #prints 3 places after decimal

#we can use formatC function for rounding large numbers
formatC(round(12345.12344,digits=3),format="f",digits=3)
[1] "12345.123"

#print option prints whatever is there in the brackets
print(1234.12344)
[1] 1234.12344
print(1234.723,digits=3)
[1] 1235 #number of digits is less than or equal to the number of digits before the decimal point, then print() retains the nearest rounded integer

print(1234.723,digits=5)
[1] 1234.7 #number of digits is greater than the number of digits present before the decimal so it prints total of 5 digits

round(123456788.123,digits=3)
[1] 123456788.123 #same rule applies to round function

print(round(123456788.123,digits=2),digits=20)
[1] 123456788.12000000477 #prints a total of 20 digits putting zeros after the decimal and printed correctly 2 digits after decimal

print(round(123456789.1234,digits=4),digits=20)
[1] 123456789.12340000272 #prints a total of 20 digits and correctly 4 digits after decimal

paste("Hello World")
[1] "Hello World" #considered as one string

paste("Hello","World")
[1] "Hello World" #considered as two strings and printed together. This is the benefit of paste function

paste("Hello","World", sep=",")
[1] "Hello,World" #the separator is used which separates the words with a comma, but consider the whole as a single string

paste(1:10)
[1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" #prints all the 10 numbers as string

paste(1:10)[4]
[1] "4" #prints only the 4th character

as.numeric(paste(1:10))
[1]  1  2  3  4  5  6  7  8  9 10 #convert strings back to numbers

paste(1:10,collapse=".")
[1] "1.2.3.4.5.6.7.8.9.10" #combines the vector as a single string and numbers are separated by dot

paste(c("Hello","World"),1:10,sep="-")
[1] "Hello-1"  "World-2"  "Hello-3"  "World-4"  "Hello-5"  "World-6"  "Hello-7"  "World-8" 
[9] "Hello-9"  "World-10" 

print(paste("Hello",1:10,sep="-"))
[1] "Hello-1"  "Hello-2"  "Hello-3"  "Hello-4"  "Hello-5"  "Hello-6"  "Hello-7"  "Hello-8" 
[9] "Hello-9"  "Hello-10"

Question 2: Generating sequences
0:10
[1]  0  1  2  3  4  5  6  7  8  9 10

15:5
[1] 15 14 13 12 11 10  9  8  7  6  5

seq(0,1.5,0.1)
[1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5

seq(6,4,-0.2)
[1] 6.0 5.8 5.6 5.4 5.2 5.0 4.8 4.6 4.4 4.2 4.0

N <- c(55,76,92,103,84,88,121,91,65,77,99)
> seq(from=0.04,by=0.01,length=11)
[1] 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14

> seq(from=0.04,by=0.01,along=N)
[1] 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 #gives the same output

seq(from=0.04,to=0.14,along=N)
[1] 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 #gives the same output

sequence(c(4,3,4,4,4,5))
[1] 1 2 3 4 1 2 3 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 #prints sequences of unequal lengths starting from 1 to the specified number

rep(9,5)
[1] 9 9 9 9 9 #prints 9 five times

rep(1:4,2)
[1] 1 2 3 4 1 2 3 4

rep(1:4,each=2)
[1] 1 1 2 2 3 3 4 4 #prints each number twice

rep(1:4,each=2,times=3)
[1] 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 #the total numbers of times 1:4 repeated is 3 and each number is repeated 2 times

rep(1:4,1:4)
[1] 1 2 2 3 3 3 4 4 4 4 #Each element of the first vector is repeated according to the corresponding value in the second vector

rep(1:4,c(4,1,4,2))
[1] 1 1 1 1 2 3 3 3 3 4 4 #each element of the first vector is repeated to the number of times specified

rep(c("cat","dog","goldfish","rat"), c(2,3,2,1))
[1] "cat"      "cat"      "dog"      "dog"      "dog"      "goldfish" "goldfish" "rat"

seq(-1,1,by=0.1)
[1] -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7
[19]  0.8  0.9  1.0

seq(-1,1,0.1)
[1] -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7
[19]  0.8  0.9  1.0 #gives the same result

seq(-1,1,length=7)
[1] -1.000000000000000 -0.666666666666667 -0.333333333333333  0.000000000000000
[5]  0.333333333333333  0.666666666666667  1.000000000000000 #calculates the step size according to the length specified

numbers <- -1 + (0:20) * 0.1
print(numbers)
[1] -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7
[19]  0.8  0.9  1.0 #to generate a set of numbers from -1 to 1 with an interval of 0.1 without using seq()

Question 3: 
  3/0
[1] Inf

exp(-Inf)
[1] 0

(0:3)**Inf
[1]   0   1 Inf Inf

0/0
[1] NaN

Inf - Inf
[1] NaN

Inf/Inf
[1] NaN

is.finite(10)
[1] TRUE

is.infinite(10)
[1] FALSE

is.infinite(Inf)
[1] TRUE

y<- c(4,NA,7)
> y=="NA"
[1] FALSE    NA FALSE
> is.na(y)
[1] FALSE  TRUE FALSE
y[!is.na(y)]
[1] 4 7 #keep only the elements where the value is TRUE

c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
   c1 c2 c3 c4
1  1  5  9  NA
2  2  6 NA  14
3  3 NA 11  15
4  NA  8 12 16
reduced.frame <- full.frame[! is.na(full.frame$c1),] #takes column 1 specifically and removes the row containing NA
  c1 c2 c3 c4
1  1  5  9 NA
2  2  6 NA 14
3  3 NA 11 15

x <- c(1, 2, 3, NA, 5)
> mean(x)
[1] NA #returns NA since not all numbers are present 
mean(x,na.rm=T)
[1] 2.75 #removes NA and returns the mean of the numbers

v <- c(1:6,NA,NA,9:12)
[1]  1  2  3  4  5  6 NA NA  9 10 11 12
seq(along=v)[is.na(v)] #returns the indices of this vector where NA is present
[1] 7 8 

which(is.na(v))
[1] 7 8 #another function to return the indices of this vector where NA is present
  