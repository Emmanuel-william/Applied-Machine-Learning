
# Parameter optimization in RF ####
# Find the optimal number of mtry (number of variables) and ntree (number of trees) which maximize the ACC
#  Write a function that:
#   Given parameters mtry (number of variables) and ntree (number of trees)
#  Create a random forest with above parameters on dfTraining
#  Calculate the accuracy of the prediction on dfTest and return it
#  Write a function that:
#   Given parameter mtry
#  Loops over all values in the vector seq(100,1000,10) as ntree
#  Call the function above with mtry and ntree as parameters and return the ntree value and
# accuracy which achieve the highest accuracy (Hint: Use a data.frame to return multiple values)
#  Write a function that:
#   Loops over all values in the vector 1:9 as mtry
#  Call the function above with mtry as parameter and return the mtry and ntree values which
# achieve the highest accuracy as well as the accuracy
#  Call the function above

# solution
# Write three function in order of identity and optimal number of ntree" and "mtry"
mydata <- read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(mydata)

# Split the entire dataset into training and test data
# First
countTraining <- round(nrow(mydata)*0.7)
countTraining
randomRows <- sample(1:nrow(mydata), size = countTraining, replace = FALSE)

# Second, now define the two data sets
dfTraining <- mydata[randomRows,]
dfTest <- mydata[-randomRows,]

summary(dfTraining)
summary(dfTest)

# Calcurating the accuracy
calcAccuracy <- function(mtryParam, ntreeParam){
  myForest = randomForest(type~., data = dfTraining, mtry = mtryParam, ntree = ntreeParam)
  #print(myForest)
  myPred = predict(myForest, dfTest, type = "class")
  confTable = table(myPred, dfTest$type)
  acc = sum(diag(confTable)) / sum(confTable)
  return(acc)
}

# calcAccuracy(9, 112) #9 and 112 was randomly choosen to test out the training data

# Calculating for optimal nTree values
optNtree <- function(mtryParam) {
  ntreeVec = seq(100, 1000, 100) # this show the possible ntree values the random forest tries out at each point
  accVec = c()
  for(i in 1:length(ntreeVec)){
    accVec[i] = calcAccuracy(mtryParam, ntreeVec[i])
  }
  maxIndex = which.max(accVec) # Determines the location, i.e., index of the (first) minimum or maximum of a numeric (or logical) vector.
  result = data.frame(ntree = ntreeVec[maxIndex], acc = accVec[maxIndex])
  return(result)
  #print(accVec)
}

# optNtree(9) # 

# Calculating for optimal Mtry values
optMtry <- function(){
  mtryVec = c(1:9)
  accVec = c()
  ntreeVec = c()
  for(i in 1:length(mtryVec)){
    result = optNtree(mtryVec[i])
    accVec[i] = result$acc
    ntreeVec[i] = result$ntree
  }
  maxIndex = which.max(accVec)
  result = data.frame(ntree = ntreeVec[maxIndex], mtry = mtryVec[maxIndex], acc = accVec[maxIndex])
  return(result)
}

optMtry()
# Multiple runs of optMtry can return different optimal parameter combinations, due to random sampling of data 
# and variables in random forest the performance of the random forest can ofter be slightly different.

forestInv <- read.table("forest.inventory.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)
forestInv







