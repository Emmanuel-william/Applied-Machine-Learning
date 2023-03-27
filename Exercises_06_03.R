#Exercises 06.03
########### Exercise: SVM and Tree #######

## SVM and DT forestType Data Analysis ####
# Import the forestTypeTraining.csv and forestTypeTesting.csv in R
forestTraining <- read.table("forestTypeTraining.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
forestTest <- read.table("forestTypeTesting.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Save them in two data frames for Training  and Test 
dfTraining <- forestTraining
dfTest <- forestTest
summary(dfTraining)
summary(dfTest)

# Perform a SVM classifier by tuning its cost and gamma parameters (use tune())
# cost: 1.0  1.2  1.4  1.6  1.8  2.0  2.2 ... 10
# gamma: 0,1,2,... 10
# Define vector of costs
costVec <- seq(1.0, 10, by = 0.2)
costVec
# Define gamma parameters
gammaVec <- seq(0, 10, by = 1)
gammaVec

# Define a function to calculate the MCC
calcMCC <- function(truePos, trueNeg, falsePos, falseNeg){
  numerator = (truePos * trueNeg) - (falsePos * falseNeg)
  denominator = sqrt((truePos+falsePos) * (truePos+falseNeg) * (trueNeg+falsePos) * (trueNeg+falseNeg))
  mcc = numerator / denominator
  return(mcc)
}

# Create an empty vector to store the results
mccVec <- c()

for (i in 1:length(costVec)) {
  print("Iteration: ")
  print(i)
  # Train the svm
  mySVM = svm(class~., data = dfTraining, cost = costVec[i])
  # Make predictions
  myPred = predict(mySVM, dfTest)
  # Create the confusion matrix
  confTable = table(myPred, dfTest$class)
  truePos = confTable[1,1]
  trueNeg = confTable[2,2]
  falsePos = confTable[1,2]
  falseNeg = confTable[2,1]
  print(confTable)
  # calculate the MCC and save it in mccVec
  mccVec[i] = calcMCC(truePos, trueNeg, falsePos, falseNeg)
  print(paste("MCC value = ", mccVec[i]))
}

# Create an empty vector to store the results
mccGamma <- c()

for (i in 1:length(gammaVec)) {
  print("Iteration: ")
  print(i)
  # Train the svm
  mySVM = svm(class~., data = dfTraining, cost = gammaVec[i])
  # Make predictions
  myPred = predict(mySVM, dfTest)
  # Create the confusion matrix
  confTable = table(myPred, dfTest$class)
  truePos = confTable[1,1]
  trueNeg = confTable[2,2]
  falsePos = confTable[1,2]
  falseNeg = confTable[2,1]
  print(confTable)
  # calculate the MCC and save it in mccVec
  mccGamma[i] = calcMCC(truePos, trueNeg, falsePos, falseNeg)
  print(paste("MCC value = ", mccGamma[i]))
}


# Tuning 
tunedParam <- tune(svm, train.x = dfTraining[,2,3], train.y = dfTraining$class, ranges = list(cost = costVec)) # here ranges = list helps to iterate through the whole costVec
tunedParam



# Visulizing the results
plot(x = costVec, y = mccVec, type = "p")

ggplot(data = data.frame(cost = costVec, mcc = mccVec), aes(x = cost, y = mcc)) + geom_point()

# so default cost value of 1 result in highest MCC value 
# Different cost values could not improve the MCC value

# Create a confusion matrix and calculate the prediction performance by 
# TP/(TP+FP+FN+TN)
# Perform a decision tree  to classify the data again
myTree <- tree(class~., data = dfTraining)
myTree
# Plot the tree
plot(myTree)
text(myTree)
# Create a confusion matrix and calculate the prediction performance by 
# TP/(TP+FP+FN+TN)
# Compare the results


##### Exercise: Regression Tree
# Import gasoline.csv in R
# Split the data into training and test
# Use a regression tree to predict the variable consumption 
# Just like we did with bostondata during the lecture
# Calculate correlation between prediction and true values of the test data

gasData <- read.table("gasolineData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
gasData
dim(gasData)
summary(gasData)

# split the dataset 
countTraining <- round(nrow(gasData)*0.7)
countTraining
randomRows <- sample(1:nrow(gasData), size = countTraining, replace = FALSE)

dfTraining <- gasData[randomRows,]
dfTest <- gasData[-randomRows,]

summary(dfTraining$capacity)
summary(dfTest$capacity)

myTree <- tree(consumption~., data = dfTraining)
myTree

summary(myTree)

# Visualize the decision tree
plot(myTree)
text(myTree)

# ggplot
ggplot(data = gasData, aes(x = seq_along(capacity), y = capacity)) + 
  geom_point()


# Using decision tree for making predictions
myPred <- predict(myTree, dfTest)
myPred

# Correlation coefficent is  the alternative of contingency tables for regression problems.
# High correlation -> good predictions
cor(myPred, dfTest$capacity)


## SVM and DT forestType Data Analysis ####
# Import the forestTypeTraining.csv and forestTypeTesting.csv in R
# Save them in two data frames for Training  and Test 
# Perform a SVM classifier by tuning its cost and gamma parameters (use tune())
# cost: 1.0  1.2  1.4  1.6  1.8  2.0  2.2 ... 10
# gamma: 0,1,2,... 10
# Create a confusion matrix and calculate the prediction performance by 
# TP/(TP+FP+FN+TN)

# Perform a decision tree  to classify the data again
# Plot the tree
# Create a confusion matrix and calculate the prediction performance by 
# TP/(TP+FP+FN+TN)
# Compare the results

myTraining <- read.table("forestTypeTraining.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
myTesting <- read.table("forestTypeTesting.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

summary(myTraining)
library(e1071)

mySVM <- svm(class~., data = myTraining)
summary(mySVM)

# How well it works on the testing data
myPred <- predict(mySVM, myTesting)
confTable <- table(myPred, myTesting$class)
confTable

# Tuning
bestParams <- tune(svm, train.x = myTraining[,-1], train.y = myTraining[,1], ranges = list(cost = seq(1, 10, by = 0.2), gamma = 0:10))
bestParams
# train.x = contains all values in the dataset except the categorical variable
# while train.y = cointans every other variable

# here we optimized by tuning the svm classifire with cost and gamma
svmTuned <- svm(class~., data = myTraining, cost = bestParams$best.parameters$cost, gamma = bestParams$best.parameters$gamma)
svmTuned
myPred <- predict(svmTuned, myTesting)
confTable <- table(myPred, myTesting$class)
confTable

accuracySVM <- sum(diag(confTable)) / sum(confTable)
accuracySVM

library(tree)
myTree <- tree(class~., myTraining)
summary(myTree)
myTree

plot(myTree)
text(myTree)

myPred <- predict(myTree, myTesting, type = "class") # always use class for classification problem, this shows 
myPred
confTable <- table(myPred, myTesting$class)
confTable
accuracyTree <- sum(diag(confTable)) / sum(confTable)
accuracyTree
# ans: 0.8 (80%) 

##### Exercise: Regression Tree
# Import gasoline.csv in R
# Split the data into training and test
# Use a regression tree to predict the variable consumption 
# Just like we did with bostondata during the lecture
# Calculate correlation between prediction and true values of the test data

mydata <- read.table("gasolineData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
mydata
dim(mydata)
summary(mydata)

# since we have only numerical variable it means we have a regression problem

# split the dataset 
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

myTraining <- mydata[randomRows,]
myTesting <- mydata[-randomRows,]
head(myTraining)

myTree <- tree(consumption~., data = myTraining)
myTree
plot(myTree) 
text(myTree) # in the plot tree text, we only see the mean values
summary(myTree)

myPred <- predict(myTree, myTesting)# becos it is a regression problem, we don't need to specify class
myPred

cor(myPred, myTesting$consumption) # our prediction works 0.7892% to predict the value of consumption




