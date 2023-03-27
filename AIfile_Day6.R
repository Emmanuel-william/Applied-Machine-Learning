
# ~ tilde
# A Higher Mathew correlation coefficent means less error in the prediction
mydata <- read.table("catsData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(mydata)

# Split data into training and test; NB training dataset should at least be 70%
countTraining <- round(nrow(mydata)*0.7)
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]
summary(dfTraining)

# Define vector of costs
costVec <- c(0.3, 0.5, 0.8, 1, 5, 10, 100, 500, 1000, 5000, 10000)
costVec

# Define a function to calculate the MCC
calcMCC <- function(truePos, trueNeg, falsePos, falseNeg){
  numerator = (truePos * trueNeg) - (falsePos * falseNeg)
  denominator = sqrt((truePos+falsePos) * (truePos+falseNeg) * (trueNeg+falsePos) * (trueNeg+falseNeg))
  mcc = numerator / denominator
  return(mcc)
}

# Create an empty vector to store the results
mccVec <- c()

for (i in 1:length(costVec)){
  print("Iteration:")
  print(i)
  # Train the svm
  mySVM = svm(Sex~., data = dfTraining, cost = costVec[i])
  # Make predictions
  myPred = predict(mySVM, dfTest)
  # Create the confusion matrix
  confTable = table(myPred, dfTest$Sex)
  truePos = confTable[1,1]
  trueNeg = confTable[2,2]
  falsePos = confTable[1,2]
  falseNeg = confTable[2,1]
  print(confTable)
  # calculate the MCC and save it in mccvec
  mccVec[i] = calcMCC(truePos, trueNeg, falsePos, falseNeg)
  print(paste("MCC value = ", mccVec[i]))
}

# Visulizing the results
plot(x = costVec, y = mccVec, type = "p")

ggplot(data = data.frame(cost = costVec, mcc = mccVec), aes(x = cost, y = mcc)) + geom_point()

# so default cost value of 1 result in highest MCC value 
# Different cost values could not improve the MCC value

# Tune model parameters automatically
costVec <- c(0.3, 0.5, 0.8, 1, 5, 10, 100, 500, 1000, 5000, 10000)

tunedParam <- tune(svm, train.x = dfTraining[,2:3], train.y = dfTraining$Sex, ranges = list(cost = costVec)) # here ranges = list helps to iterate through the whole costVec
tunedParam

# to access the best values of parameters
tunedParam$best.parameters$cost

mySVM <- svm(Sex~., data = dfTraining, cost = tunedParam$best.parameters$cost)
plot(mySVM, data = dfTraining) # Higher cost reulted in more complex hyperplane

##################################################################################################################
#### DECISION TREE ####

mydata <- read.table("animals.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
mydata
dim(mydata)

summary(mydata)

# split the dataset 
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]

summary(dfTraining$Animal)
summary(dfTest$Animal)

# create a decision tree
library(tree)
?tree

myTree <- tree(Animal~., data = dfTraining)
myTree # has 3500 in total, 
#node), split, n, deviance, yval, (yprob) * denotes terminal node

#1) root 3500 9703 Horse ( 0.2483 0.2469 0.2477 0.2571 )  
#2) Torso_7 < 39.01 1733 2402 Cat ( 0.5014 0.4986 0.0000 0.0000 )  
#4) Torso_7 < 22 869    0 Cat ( 1.0000 0.0000 0.0000 0.0000 ) *
#  5) Torso_7 > 22 864    0 Dog ( 0.0000 1.0000 0.0000 0.0000 ) *
#  3) Torso_7 > 39.01 1767 2449 Horse ( 0.0000 0.0000 0.4907 0.5093 )  
#6) Torso_7 < 59.96 900    0 Horse ( 0.0000 0.0000 0.0000 1.0000 ) *
#  7) Torso_7 > 59.96 867    0 Giraffe ( 0.0000 0.0000 1.0000 0.0000 ) *
#  > 
# the above output shows the visual representation of the decision tree that has all animals and the 
# ("<">") symbols suggests higher and lower and which direction to go during the split. 

summary(myTree)
# Misclassification error rate: 0 = 0 / 3500 shows we peformed a good classification

# Plot the complete dataset based on the variables that are actually used in the tree
# here -> only Torso_7
ggplot(data = mydata, aes(x = seq_along(Torso_7), y = Torso_7)) + 
  geom_point(aes(color = Animal, shape = Animal))

# Visualize the decision tree
plot(myTree)
text(myTree)

# Using decision tree for making predictions
myPred <- predict(myTree, dfTest, type = "class")
myPred

confTable <- table(myPred, dfTest$Animal)
confTable # this confusion matrix show that our prediction is optimal and a perfect classifier which works perfectly on the training data and test data

# Excurse
myTree <- tree(Animal~Head_4, data = dfTraining)# this dosent work because the variable head_4 is just a constant
myTree 

myPred <- predict(myTree, dfTest, type = "class")
confTable <- table(myPred, dfTest$Animal)
confTable

myTree <- tree(Animal~Head_4+Head_5, data = dfTraining)# this dosent work because the variable head_4 is just a constant
myTree 

myPred <- predict(myTree, dfTest, type = "class")
confTable <- table(myPred, dfTest$Animal)
confTable

ggplot(data = mydata, aes(x = seq_along(Head_4), y = Head_4)) + 
  geom_point(aes(color = Animal, shape = Animal))

ggplot(data = mydata, aes(x = seq_along(Head_5), y = Head_5)) + 
  geom_point(aes(color = Animal, shape = Animal))


myTree <- tree(Animal~Head_4+Head_5+Head_9, data = dfTraining)# this dosent work because the variable head_4 is just a constant
myTree

myPred <- predict(myTree, dfTest, type = "class")
confTable <- table(myPred, dfTest$Animal)
confTable

ggplot(data = mydata, aes(x = seq_along(Head_9), y = Head_9)) + 
  geom_point(aes(color = Animal, shape = Animal))

# 
myTree <- tree(Animal~Head_4+Head_5+Head_9+Torso_7, data = dfTraining)# this works because the variable Torso_7 was added
myTree

myPred <- predict(myTree, dfTest, type = "class")
confTable <- table(myPred, dfTest$Animal)
confTable

ggplot(data = mydata, aes(x = seq_along(Torso_7), y = Torso_7)) + 
  geom_point(aes(color = Animal, shape = Animal))


# Excercise
myPlantData <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
myPlantData

# split the dataset 
countTraining <- round(nrow(myPlantData)*0.7)
countTraining
randomRows <- sample(1:nrow(myPlantData), size = countTraining, replace = FALSE)

dfTraining <- myPlantData[randomRows,]
dfTest <- myPlantData[-randomRows,]

summary(dfTraining$Species)
summary(dfTest$Species)

# create a decision tree
library(tree)
?tree

myTree <- tree(Species~., data = dfTraining)
myTree # has 105 in total of the population size, 

summary(myTree)
# Misclassification error rate: 0.0381 = 4 / 105 almost zero shows we peformed a good classification

# First Visualize the decision tree
plot(myTree)
text(myTree)

# Plot the complete dataset based on the variables that are actually used in the tree
# here -> petal.length
ggplot(data = myPlantData, aes(x = seq_along(Petal.Length), y = Petal.Length)) + 
  geom_point(aes(color = Species, shape = Species))


# Using decision tree for making predictions
myPred <- predict(myTree, dfTest, type = "class") # Gives you class with maximum probality 
myPred

# to get the probality of where each class belongs
myPred <- predict(myTree, dfTest)
head(myPred)

confTable <- table(myPred, dfTest$Species)
confTable # this confusion matrix show that our prediction is optimal and a perfect classifier which works perfectly on the training data and test data

# Using Petal.Width
# Plot the complete dataset based on the variables that are actually used in the tree
# here -> Petal.Width
ggplot(data = myPlantData, aes(x = seq_along(Petal.Width), y = Petal.Width)) + 
  geom_point(aes(color = Species, shape = Species))


# Using decision tree for making predictions
myPred <- predict(myTree, dfTest, type = "class")
myPred

confTable <- table(myPred, dfTest$Species)
confTable # this confusion matrix show that our prediction is optimal and a perfect classifier which works perfectly on the training data and test data

# Regression Tree ####
mydata <- read.table("bostonData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
mydata
summary(mydata)

# split the dataset 
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]

myTree <- tree(medv~., data = dfTraining)
summary(myTree)
summary(dfTraining$medv)
myTree
plot(myTree)
text(myTree)

# Prediction
myPred <- predict(myTree, dfTest)
head(myPred)

# Correlation coefficent is  the alternative of contingency tables for regression problems.
# High correlation -> good predictions
cor(myPred, dfTest$medv)














