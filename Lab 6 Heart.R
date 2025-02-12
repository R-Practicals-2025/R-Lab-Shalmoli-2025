#read the data csv file
data<-read.csv("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/Heart.csv")
print(data)
#checking the class of the current data
class(data)
[1] "data.frame"
#changing the class to factors
data$ChestPain <- as.factor(data$ChestPain)
print(is.factor(data$ChestPain)) #checking whether its actually a factor class
[1] TRUE
data$Thal <- as.factor(data$Thal)
print(is.factor(data$Thal)) 
[1] TRUE
data$AHD <- as.factor(data$AHD)
print(is.factor(data$AHD))  
[1] TRUE 
#number of levels
print(nlevels(data$Thal))
[1] 3
print(nlevels(data$ChestPain))
[1] 4
#print levels in the category Thal and ChestPain
print(levels(data$Thal))
[1] "fixed"      "normal"     "reversable"
print(levels(data$ChestPain))
[1] "asymptomatic" "nonanginal"   "nontypical"   "typical"

#generating factors levels using gl function
#contains levels such as "High-Risk", "Medium Risk" and "Low Risk"
risk<-gl(3,1,303,labels=c("High-Risk","Medium-Risk","Low-Risk"))
print(risk)
#add this category to the heart dataframe and print the new dataframe
newdata<-data.frame(data,risk)
print(newdata)

#using the tapply() function
tapply(data$Chol, data$RestBP, mean) #it categorizes the data based on RestBP and computes the mean of Chol, based on the categories made by RestBP
#using the trim function
tapply(data$Chol, data$RestBP, mean,trim=0.1) #it trims the values of Chol based on the category of RestBP, both 10% from the initial values and 10% from the end values
#so it trims a total of 20%

#find parallel max and min
print(pmin(data$RestBP, data$Chol, data$MaxHR)) #finds the minimum value of the 3 columns by comparing it parallel wise
print(pmax(data$RestBP, data$Chol, data$MaxHR)) #finds the maximum value of the 3 columns by comparing it parallel wise

#difference between rank, sort and order
#it gives rank 1,2 and so on for the data set, where the lowest value gets rank 1 and the rank gets assigned in an ascending order
ranks <- rank(data$RestBP)
print(ranks)
sorted <- sort(data$RestBP) #sorts the original data in ascending order
print(sorted)
ordered <- order(data$RestBP) #provides the index number of the sorted data by checking for the index number of the values in the original data
print(ordered)
#to create a new dataframe with RestBP and ranks, sorted and order
new<-data.frame(data$RestBP,ranks,sorted,ordered)
print(new)
#to print the chestpain data using the indices from the ordered vector, meaning checking the indices from the ordered vector and check what is there in the chestpain data at that particular index
chestpain_ordered<-data$ChestPain[ordered]
#create a new data frame with the ordered RestBP and ChestPain column
ordered_data <- data.frame(ordered_RestBP = data$RestBP[ordered], ordered_chestpain = chestpain_ordered)
print(ordered_data)
#writing this ordered data in a new dataframe
write.csv(ordered_data, "C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/lab6_ordered_data.csv")

#converting data frames into matrices
#extract specific rows and columns
filter<-data[1:6,3:8]
print(filter)
#convert the class of this data frame to matrix
class(data)
[1] "data.frame" #class of the original data
filter1=as.matrix(filter)
class(filter1)
[1] "matrix" "array"
#print attributes of this filtered data
attributes(filter1)
$dim
[1] 6 6
$dimnames
$dimnames[[1]]
[1] "1" "2" "3" "4" "5" "6"
$dimnames[[2]]
[1] "Sex"       "ChestPain" "RestBP"    "Chol"      "Fbs"       "RestECG"
#create a new column by adding 3 columns
newcol<-data$RestBP + data$Chol + data$MaxHR
print(newcol)
#create new dataframe by adding this column to the last
coladd<-data.frame(data,newcol)
print(coladd)
#print the column names of the new dataframe
print(colnames(coladd))
[1] "X"         "Age"       "Sex"       "ChestPain" "RestBP"    "Chol"      "Fbs"      
[8] "RestECG"   "MaxHR"     "ExAng"     "Oldpeak"   "Slope"     "Ca"        "Thal"     
[15] "AHD"       "newcol"   
#add new column by using cbind function
coladd2<-cbind(data,newcol)
print(coladd2)
#print the column names
print(colnames(coladd2))
[1] "X"         "Age"       "Sex"       "ChestPain" "RestBP"    "Chol"      "Fbs"      
[8] "RestECG"   "MaxHR"     "ExAng"     "Oldpeak"   "Slope"     "Ca"        "Thal"     
[15] "AHD"       "newcol"
#pick rows and add these rows to the original dataframe
filter2=data.frame(data[c(26,35),])
newrow<-rbind(data,filter2)
print(newrow) #to print the entire row till the last, some elements gets wipped off due to space
#we use tail function to print the last rows
print(tail(newrow)) #for the row numbers added 26 and 35, it is showint 2610 and 351, since 26, 35, 261 indices are all used up
#so it created new indices
#print the dimensins of this new dataframe
print(dim(newrow))
[1] 305  15