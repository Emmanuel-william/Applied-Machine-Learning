install.packages("remotes")# for opening data from git hub
library(remotes)
remotes::install_github("obaezvil/AGEPadvanced")
library(AGEPadvanced)
render_lectures("Lecture1")
render_lectures("Lecture2")
render_lectures("Lecture3")
render_lectures("Lecture4")
render_lectures("Lecture5")
render_lectures("Lecture6")

install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggdendro")
library(ggdendro)
install.packages("igraph")
library(igraph)
install.packages("ape")
library(ape)
install.packages("factoextra")
library(factoextra)
install.packages("jpeg")
library(jpeg)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("FactoInvestigate")
library(FactoInvestigate)
install.packages("e1071")
library(e1071)
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
install.packages("neuralnet")
library(neuralnet)

##################################################################################################################################

plantData <- read.table("plantData.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
plantData
head(plantData) # first 6 rows
tail(plantData) # last 6 rows
summary(plantData)
str(plantData)

#### Dealing with missing values, e.g N.As #### by obtaining there mean values
meanSepalLenght = mean(plantData$Sepal.Length, na.rm = TRUE)
meanSepalLenght

meanSepalWidth = mean(plantData$Sepal.Width, na.rm = TRUE)
meanSepalWidth

#### Replacing the N.As with the mean values ####
is.na(plantData$Sepal.Length) # which values are missing?
plantData$Sepal.Length[is.na(plantData$Sepal.Length)] = meanSepalLenght

#### Replacing the N.As with the mean values ####
is.na(plantData$Sepal.Width) # which values are missing?
plantData$Sepal.Width[is.na(plantData$Sepal.Width)] = meanSepalWidth

summary(plantData)

# Write a data.frame to a file
write.table(plantData, "plantData_NoNA.csv", row.names = FALSE, col.names = TRUE, sep = ",")

#### Conditional statements ####
# calculate the median
x = c(4,5,6,10,12,15,20)
n = length(x)
n
if(n%%2 == 1){
  med = x[(n+1)/2]
  print("Uneven ")
  print(med)
} else{
  med = 0.5 * (x[n/2]+x[(n/2)+1])
  print("Even ")
  print(med)
}
#### Calc the median of sepal length and sepal width, The values have to be sorted first.
sepalLH <- sort(plantData$Sepal.Length)
sepalLH
sepalWT <- sort(plantData$Sepal.Width)
sepalWT

L = length(sepalLH)
L
W = length(sepalWT)
W
# for sepal length
if(L%%2 == 1){
  med = sepalLH[(L+1)/2]
  print("Uneven ")
  print(med)
} else{
  med = 0.5 * (sepalLH[L/2]+sepalLH[(L/2)+1])
  print("Even ")
  print(med)
}

median(sepalLH)

# for sepal width
if(W%%2 == 1){
  med = sepalWT[(W+1)/2]
  print("Uneven ")
  print(med)
} else{
  med = 0.5 * (sepalWT[W/2]+sepalWT[(W/2)+1])
  print("Even ")
  print(med)
}

median(sepalWT)

# Loops ####
x = c(1:6)
x

for (i in x) {
  print(i)
}

for (i in x) {
  print(i)
}

x = c(4,5,6,10,12)
for(i in 1:length(x)) {
  print(i)
}

for(i in 1:length(x)){
  cat("i = ",i," and x[",i,"] = ", x[i], "\n")
}

# Calculate mean of Vector ####
sum = 0 # initialize the value to add up with
size = length(x)
size

for(i in 1:size){
  sum = sum + x[i]
}
sum

mean = sum / size
mean

mean(x)

# Excercise #### 
# Given two DNA sequence calc the number of deviations between the two sequences?
seq1 = c("A", "A", "C", "T", "T", "C", "G", "G", "A", "C")
seq2 = c("A", "C", "C", "A", "T", "C", "T", "G", "T", "C")

sd = 0 # at the start we believe there is no deviations
sizeS = length(seq1)
sizeS
# seq1[1] == seq2[1] 

for(i in 1:length(seq1)){
  print(i)
  if(seq1[i] != seq2[i]){
    cat("here the sequences deviate: ", i, "\n")
    sd = sd + 1
    cat("the distance right now is: ", sd,  "\n")
  }
}

sd
# Answer : the number of deviation is in total 4 

# Calculate the similarity between both seqences
similarity = (length(seq1)-sd) / length(seq1)
similarity

# Functions ####
pow = function(x,y){
  result = x^y
  cat("The result is ", result)
}

pow(2,3)
pow(11,2)
pow(456,4)

pow_with_return = function(x,y){
  result = x^y
  return(result)
}

pow_with_return(2,3)
z = pow_with_return(2,3)
z

# Excersice 2 
# Write a function to calculate the similarity score b/w two DNA sequence
simiL = function(seq1,seq2){
  sim=0 # at the start, we believe there is no deviation
  for(i in 1:length(seq1)){
    print(i)
    if(seq1[i] != seq2[i]){
      cat("here the sequences deviate: ", i, "\n")
      sim = sim + 1
      cat("the distance right now is: ", sd,  "\n")
    }
  }
  result = (length(seq1)-sim) / length(seq1)
  return(result)
}
simiL(seq1, seq2) # here you call the function you are calculating.

# Graphical visualization with ggplot2
library(ggplot2)

# Scatterplot using plot + lines
plot(plantData$Sepal.Width, plantData$Sepal.Length, main = "Sepal.Width - Sepal.Lenght", 
     xlab = "Sepal.Width", ylab = "Sepal.Lenght", col = "red",)

lines(x = plantData$Sepal.Width[plantData$Species=="versicolor"],
      y = plantData$Sepal.Length[plantData$Species=="versicolor"],
      type="p", col ="green")

lines(x = plantData$Sepal.Width[plantData$Species=="virginica"],
      y = plantData$Sepal.Length[plantData$Species=="virginica"],
      type="p", col ="blue")

legend("topright", legend = c("setosa", "versicolor", "virginica"), 
       fill = c("red", "green", "blue"), cex = 0.5)


# Scatterplot using ggplot2
# ggplot main background
ggplot(data = plantData, aes(x = Sepal.Width, y = Sepal.Length))

# add the scatter plot using geom_point()
ggplot(data = plantData, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point()

# specify the color of the points
ggplot(data = plantData, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color=Species))

# add labels using labs
ggplot(data = plantData, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color=Species)) + 
  labs(x = "Sepal Width", y = "Sepal Length", title = "Sepal width - Sepal lengths of Iris") +
  theme(legend.position = "right")
































