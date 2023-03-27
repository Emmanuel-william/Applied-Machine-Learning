
# ~ tilde
#### RANDOM FOREST ####

mydata <- read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(mydata)

# Split dataset into training and test
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]

summary(dfTraining)

# Train the random forest
library(randomForest)

myForest <- randomForest(type~., data = dfTraining)
myForest
# OOB(out of back) estimate of  error rate: 33.5% is the average of errors of all trees, gotten from training all the trees
# class.error means the probality to get error when you predict.

summary(myForest) # not useful

# Using random forest for prediction
myPred <- predict(myForest, dfTest, type = "class")
confTable <- table(myPred, dfTest$type)
confTable 

truePos <- confTable[1,1]
trueNeg <- confTable[2,2]
falsePos <- confTable[1,2]
falseNeg <- confTable[2,1]

sensitivity <- truePos / (truePos+falseNeg)
sensitivity
# if there is no reoccurance evnt we were able to identify it in 0.8615385 % chance

specificity <- trueNeg / (trueNeg+falsePos)
specificity

# Exercise 
# Apply randomForest on the forestTypeTraining and ForestTypeTesting datasets
myTraining <- read.table("forestTypeTraining.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
myTesting <- read.table("forestTypeTesting.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)


# Train the random forest
library(randomForest)

myForest <- randomForest(class~., data = myTraining)
myForest

# Using random forest for prediction and create confusion matrix
myPred <- predict(myForest, myTesting, type = "class")
confTable <- table(myPred, myTesting$class)
confTable 

accuracyRandomForest <- sum(diag(confTable)) / sum(confTable)
accuracyRandomForest

# Accuracy of single decision tree
accuracyTree
# -> Here: random forest has only a minor increase in accuracy compared to a single decision tree

###########################################################################################################
# RANDOM FOREST CONTINUED ####
mydata <- read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(mydata)

# Split dataset into training and test
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]

summary(dfTraining)

# Train the random forest
library(randomForest)

myForest <- randomForest(type~., data = dfTraining)
myForest # OOB estimate of  error rate: 30% it is huge and a problem

# Using random forest for prediction and create confusion matrix
myPred <- predict(myForest, dfTest, type = "class")
confTable <- table(myPred, dfTest$type)
confTable

# Analyzing the random forest
# Error rates
# This returns the classification error for the different classes(as well as all the classes tigether when using only the first i-th trees to classify)
myForest$err.rate # this gives the error of OOB, recurrence-events and no reoccurance events

# To visualize the error rate 
# plot the error rates
plot(myForest, col = 1:3)
legend("topright", colnames(myForest$err.rate), col = 1:3, fill = 1:3, cex = 0.5)
# the middle line is the error rate, the first and third line is the 
# based on the plot only 250 tree is enough to get the optimal error rate

# with ggplot
plotValues <- data.frame("Tree" = seq(1, nrow(myForest$err.rate)), myForest$err.rate)
head(plotValues)

library(ggplot2)
ggplot(data = plotValues, aes(x = Tree)) +
  geom_line(aes(y = OOB), color = "black") +
  geom_line(aes(y = no.recurrence.events), color = "red") + 
  geom_line(aes(y = recurrence.events), color = "green") + 
  labs(y = "Error rate") + lims(y = c(0,1))

# Variable importance
myForest$importance
# this shows how much the gini decrease on average when using this variable for splitting
var_importance <- myForest$importance
sort(var_importance, decreasing = TRUE)

# to visualize variable importance
varImpPlot(myForest)

# Using random forest for prediction and create confusion matrix
myPred <- predict(myForest, dfTest, type = "class")
myPred
confTable <- table(myPred, dfTest$class)
confTable 

# Show how strong randomforest belive in the recurrence and non event or the probabilities that the variables belong to a certain class
myPred <- predict(myForest, dfTest, type = "vote")
head(myPred)

# ROC Plot - Receiver-Operator Characteristic ####

# First convert myPred ehich in this case a matrix to a dataframe
predictions <- as.data.frame(myPred)
head(predictions)

predictions$groundTruth = dfTest$type
head(predictions) # tjis shows where recurrence events occur

totalPositiveNumber <- nrow(subset(predictions, predictions$groundTruth=="no-recurrence-events"))
totalNegetiveNumber <- nrow(predictions) - totalPositiveNumber
totalPositiveNumber
totalNegetiveNumber

# Create two empty vectors to store the results
TPR <- c()
FPR <- c()

successthreshold = 0
for(i in 1:100){
  # Get the number of True Positve rate
  selectedRows = subset(predictions, predictions$`no-recurrence-events` >= successthreshold & predictions$groundTruth=="no-recurrence-events")
  TPR[i] = nrow(selectedRows) / totalPositiveNumber
  # Get the false positive rate
  selectedRows = subset(predictions, predictions$`no-recurrence-events` >= successthreshold & predictions$groundTruth=="recurrence-events")
  FPR[i] = nrow(selectedRows) / totalNegetiveNumber
  successthreshold = successthreshold + 0.01
}

plot(x = FPR, y = TPR, xlab = "FPR", ylab = "TPR") # when we perform this predictions from zero we asume there is no recurrence, therefore true positive
# but also when we consider all the data the probality of getting a false positive will increase as we go through all the data
lines(x = seq(0.1, by = 0.01), y = seq(0.1, by = 0.01), type = "l", col = "red" )









