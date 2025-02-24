#function to find if an integer is a palindrome
palindrome <- function(num) {
  original <- num  # Store the original number
  reverse <- 0
  
  while (num > 0) {
    digit <- num %% 10        # Extract last digit
    reverse <- reverse * 10 + digit  # Add digit to reversed number
    num <- num %/% 10 # Remove last digit and gives the quotient
  }
  
  if (original == reverse) {
    print("The number is a palindrome")
  } else {
    print("The number is not a palindrome")
  }
}
palindrome(121)

#slice the string and print the various substrings
string<-"seemerightnow"
print(substr(string,1,3)) #extract the string from positions 1 to 3
[1] "see"
print(substr(string,4,5)) #extracts the string from positions 4 and 5
[1] "me"
print(substr(string,6,10)) #extracts the string from positions 6 to 10
[1] "right"

#determine the fraction of G and C bases in the nucleotide
seq="ATTGCGCATAGTCCGGG"
ind=unlist(strsplit(seq,split=""))
print(ind)
tg=0
tc=0
for(i in 1:nchar(seq)){
  
if(ind[i]=="G"){
  tg=tg+1
}else if(ind[i]=="C"){
  tc=tc+1
}
}
totg=tg/(nchar(seq))
totc=tc/(nchar(seq))
print(paste("The fraction of G base is",totg))
print(paste("The fraction of C base is",totc))
#output
"The fraction of G base is 0.352941176470588"
"The fraction of C base is 0.235294117647059"

#to check if a DNA sequence is palindrome to its complementary sequence
dnaseq="TGGATCCA"
compl=chartr("ATGC", "TACG", dnaseq)
compl_seq <- paste(rev(strsplit(compl, NULL)[[1]]), collapse = "")
print(compl_seq)
if(dnaseq==compl_seq){
  print("The sequence and its complement are palindromic")
} else{
  print("The sequence and its complement are not palindromic")
}

#print largest word in a sentence and group words by their lengths
sentence <- "She sells hundreds of sea oysters on the sea shore"
words <- unlist(strsplit(sentence, " ")) #split the entire sentence by words, considering the space
print(words)
[1] "She"      "sells"    "hundreds" "of"       "sea"      "oysters"  "on"       "the"     
[9] "sea"      "shore"
#counting the number of characters in each word
word_count <- nchar(words)
print(word_count)
[1] 3 5 8 2 3 7 2 3 3 5
words_by_length <- split(words, word_count)  #group words by their length
print("Words grouped by length")
print(words_by_length)
$`2`
[1] "of" "on"

$`3`
[1] "She" "sea" "the" "sea"

$`5`
[1] "sells" "shore"

$`7`
[1] "oysters"

$`8`
[1] "hundreds"
#print the largest word
print(words_by_length[length(words_by_length)])
$`8`
[1] "hundreds"
#print the second largest word
print(words_by_length[length(words_by_length)-1])
$`7`
[1] "oysters"

#Load data and create separate dataframes for each continent
library(moments)
worldfloras <- read.table("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/worldfloras.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
continents <- unique(worldfloras$Continent)
print(continents)
[1] "Asia"       "Europe"     "N.Africa"   "Africa"     "Antarctica" "S.America"  "Australia" 
[8] "C.America"  "SE.Asia"    "N.America"  "Pacific"  
#create separate suubsets for each continent
asia_data <- subset(worldfloras, Continent == "Asia") #creates subsets for all the countries present in the continent Asia
europe_data <- subset(worldfloras, Continent == "Europe")
africa_data <- subset(worldfloras, Continent == "Africa")
america_data <- subset(worldfloras, Continent == "America")
oceania_data <- subset(worldfloras, Continent == "Oceania")
#view the data
head(asia_data) #shows the first 6 entries under Asia
#boxplot for floral count distribution within each continent
boxplot(Flora ~ Continent, data = worldfloras, main = "Floral Count Distribution by Continent", xlab = "Continent", ylab = "Floral Count", col = "lightgreen", border = "black")
#flora~Continent means how the flora is spread among various continents
#to print the statistical summary
summary_stats <- aggregate(Flora ~ Continent, data = worldfloras, summary)
#the aggregate function is used to group the data on the basis of continent
print(summary_stats)
#calculate mean and standard deviation
mean_floral <- mean(worldfloras$Flora, na.rm = TRUE) #calculate mean of the flora column and remove any NAs present
sd_floral <- sd(worldfloras$Flora, na.rm = TRUE)
print(mean_floral)
print(sd_floral)
#output
[1] 5295.155 #mean
[1] 7182.933 #standard deviation
floral_skewness <- skewness(worldfloras$Flora, na.rm = TRUE)
floral_kurtosis <- kurtosis(worldfloras$Flora, na.rm = TRUE)
print(floral_skewness)
print(floral_kurtosis)
#output
[1] 3.871487 #skewness
[1] 22.37772 #kurtosis
#boxplot and histogram plot of the population distribution within each continent
boxplot(Population ~ Continent, data = worldfloras, main = "Population Distribution by Continent", xlab = "Continent", ylab = "Population", col = "lightgreen", border = "black")
hist(worldfloras$Population, main = "Population Distribution", xlab = "Population", col = "purple", border = "black")
#perform statistical analysis
population_stats <- aggregate(Population ~ Continent, data = worldfloras, summary)
print(population_stats)
#compute skewness and kurtosis
population_skewness <- skewness(worldfloras$Population, na.rm = TRUE)
population_kurtosis <- kurtosis(worldfloras$Population, na.rm = TRUE)
print(population_skewness)
print(population_kurtosis)
#output
[1] 7.729849 #skewness
[1] 68.1785 #kurtosis

#Read in the data from 'HumanBones.txt'
bones_data <- readLines("C:/Users/SHALMOLI/Desktop/IBAB PhD/Semester 1/R Lab practicals/HumanBones.txt")
for (line in bones_data) {
  if (!grepl("\\(", line)) {
    current_category <- trimws(line)  #set new category
  } else {
    bone_info <- strsplit(line, "\\(")[[1]]
    bone_name <- trimws(bone_info[1])  
    bone_number <- as.numeric(str_extract(bone_info[2], "\\d+")) #extract only first number
    categories <- c(categories, current_category)
    bone_names <- c(bone_names, bone_name)
    bone_numbers <- c(bone_numbers, bone_number)
  }
}
#create dataframe with the categories
bones_info <- data.frame(category = categories, name_of_bone = bone_names, number_of_bones = bone_numbers, stringsAsFactors = FALSE)
print(head(bones_info))
#category that contains the maximum number of bones
category_bones_summary <- aggregate(number_of_bones ~ category, data = bones_info, sum)
max_category <- category_bones_summary[which.max(category_bones_summary$number_of_bones), ]
print(max_category$category)
#create a frequency table and barplot of each of the category
category_frequency <- table(bones_info$category)
print(category_frequency)
barplot(category_bones_summary$number_of_bones, names.arg = category_bones_summary$category, main = "Number of Bones by Category", xlab = "Category", ylab = "Number of Bones", col = "pink", border = "black")
#subset category "Legs" and find bones with names longer than 5 letters
legs_data <- subset(bones_info, category == "Legs")
long_bones_legs <- subset(legs_data, nchar(name_of_bone) > 5)
print(long_bones_legs$name_of_bone)
#list bones starting with "M" and replace "a" with "A"
bones_starting_M <- subset(bones_info, grepl("^M", name_of_bone))
bones_starting_M$name_of_bone <- gsub("a", "A", bones_starting_M$name_of_bone)
print(bones_starting_M$name_of_bone)
#list bones ending with "e" and convert them to lowercase
bones_ending_with_e <- subset(bones_info, grepl("e$", name_of_bone))
bones_ending_with_e$name_of_bone <- tolower(bones_ending_with_e$name_of_bone)
print(bones_ending_with_e$name_of_bone)
#list bones with two "o"s in their names
bones_with_two_o <- subset(bones_info, grepl("o.*o", name_of_bone, ignore.case = TRUE))
print(bones_with_two_o$name_of_bone)