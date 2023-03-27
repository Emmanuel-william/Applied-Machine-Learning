#Exercise 03.03

####  PCA + SVM  Exercise ####
# 1) import both data sets: forestTypeTraining.csv and forestTypeTesting.csv in R
# 2) combine  both data sets to one data frame
# 3) Perform a PCA analysis using the prcomp function and save the results 
# of the fviz_contrib function applied for the axes 1:2 in a variable, namely tmp
# 4) save the contribution values in tmp in a data frame
# 5) select the top 10 variables with highest contribution scores
# 6) Finally, using the selected 10 variables, perform a svm classifier for the 
# forestTypeTraining.csv data set and validate performance of your classifier using 
# forestTypeTesting.csv data set

# 1) import both data sets: forestTypeTraining.csv and forestTypeTesting.csv in R
forestTypeTraining <- read.table("forestTypeTraining.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
foresTypeTesting <- read.table("forestTypeTesting.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

dim(forestTypeTraining)
dim(foresTypeTesting)
# they have the same column but different rows
head(forestTypeTraining)
head(foresTypeTesting)
summary(forestTypeTraining)
summary(foresTypeTesting)

# 2) combine  both data sets to one data frame
# they have the same column but different rows
combData <- rbind(forestTypeTraining, foresTypeTesting)
combData
dim(combData)
str(combData)
summary(combData)
which(combData == "NA")


# 3) Perform a PCA analysis using the prcomp function and save the results 
# of the fviz_contrib function applied for the axes 1:2 in a variable, namely tmp
# PCA only works with numeric variable so we split

# Solution
#  Divide mydata in two data frames as df1 and df2
#  df1 contains the attribute “class”
#  df2 contains the remaining information
df1 <- combData[, "class"]
df2 <- combData[, 2:28]
dim(df2)
summary(df2)

#  Apply principal component analysis(PCA)
resultPca <- prcomp(df2, scale. = TRUE) # because our vectors are not scaled so set them to have same variable and variance or standardized(i.e means of zero and variance of one)
summary(resultPca) # Explained variance/variation of the components

# to visulize the scree plot
library(factoextra)
fviz_screeplot(resultPca)

resultPca$rotation # the loading scores for the principle components

resultPca$x # PCA coordinates or the positions of the objects

# Contribution of the variables to principal components
fviz_contrib(resultPca, choice="var", axes = 1) # Variable contribution to PC1
fviz_contrib(resultPca, choice="var", axes = 2) # Variable contribution to PC2
tmp <- fviz_contrib(resultPca, choice="var", axes = 1:2) # Variable contribution to PC1 & 2
tmp

# 4) save the contribution values in tmp in a data frame
# 5) select the top 10 variables with highest contribution scores
contributions <- tmp$data

# Order rows of data frame in decresing contribution value
contributions = contributions[order(contributions$contrib, decreasing = TRUE),]
head(contributions)

# 6) Finally, using the selected 10 variables, perform a svm classifier for the 
# forestTypeTraining.csv data set and validate performance of your classifier using 
# forestTypeTesting.csv data set
myTopVariables <- as.character(contributions$name[1:10]) # we convert the "name" variable to "character string" because the "select" function can only use it for analysis
myTopVariables

myTrainingSelected <- subset(forestTypeTraining, select = c("class", myTopVariables))
summary(myTrainingSelected)

library(e1071)
mySVM <- svm(class~., data = myTrainingSelected)
summary(mySVM)

# to test it on a data that hasn't been used for training or to validate performance
myPred <- predict(mySVM,foresTypeTesting)
myPred
confTable <- table(myPred, foresTypeTesting$class)
confTable

# to get the accuracy whivh is the sum of the diagonal matrix
ACC <- sum(diag(confTable)) / sum(confTable)
ACC # 0.7292308 more than 72 % were accurately predicted by using 10 variables

# But selecting every variable can help increase the performance of the model for eg
myTopVariables <- as.character(contributions$name)
myTopVariables

myTrainingSelected <- subset(forestTypeTraining, select = c("class", myTopVariables))
summary(myTrainingSelected)

library(e1071)
mySVM <- svm(class~., data = myTrainingSelected)
summary(mySVM)

# to test it on a data that hasent been used for training
myPred <- predict(mySVM,foresTypeTesting)
myPred
confTable <- table(myPred, foresTypeTesting$class)
confTable

# to get the accuracy whivh is the sum of the diagonal matrix
ACC <- sum(diag(confTable)) / sum(confTable)
ACC # 0.7292308 more than 72 % were accurately predicted by using 10 variables
# 0.8369231 more than 83% were accurately predicted by using every variables which shows higher accuracy
































































