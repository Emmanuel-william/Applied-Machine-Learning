
mydata <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
mydata

df1 <- mydata[,"Species"]
df2 <- mydata[, c(1,2,3,4)]

summary(df2)

library(FactoMineR)

res.PCA = PCA(df2, scale.unit = TRUE) # the closer the variables are to the horizontal line the more important/ influencial they are to the PCA
summary(res.PCA)
# eigenValues : how much of the variables

print(res.PCA) # Results for the Principal Component Analysis (PCA), The analysis was performed on 150 individuals, described by 4 variables

# Interpreting the PCA
eigenValues = res.PCA$eig
eigenValues # Proportion of variance explaned by the PCs

# Visulizing this information on scree plot
fviz_screeplot(res.PCA) # Visulization of the importance of the PC

# The correlation/contribution of the variables to the PCs is called loading score

res.PCA$var$coord

# creating the above variables plot
fviz_pca_var(res.PCA) # showing us which variables are important for the first two PCs
fviz_pca_var(res.PCA, axes = c(3,4))# by default PCs choose the default first and second most important PCs, we can set it to axes

# Graph of the individuals on the PCs
fviz_pca_ind(res.PCA)

# shows the individuals as well as the importance of the variables
fviz_pca_biplot(res.PCA)

# TO Write a PCA report automatically
library(FactoInvestigate)
Investigate(res.PCA, document = c("html_document")) # or we can use 
Investigate(res.PCA, document = c("word_document"))
# to use pdf document use the packages pdflatex
install.packages("pdflatex")
library(pdflatex)
Investigate(res.PCA, document = c("pdf_document")) # document changes the output type word, odf and html are possible

#######################################################################################################
# Classification with SVM's ####

mydata <- read.table("catsData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(mydata)

# Split the dataset into training set and a test set
# Define 70% as the training data and the 30% as the test data
# Calculating the training data of 70%
countTraining = round(nrow(mydata)*0.7)
countTraining # we 101 training data and 43 for testing

# selecting random rows for the training dataset
randomRows = sample(1:nrow(mydata), countTraining, replace = FALSE)
randomRows

# Training dataframe
dfTraining = mydata[randomRows,]
dim(dfTraining)

# Assigning the test data into a dataframe
dfTest = mydata[-randomRows,]
dim(dfTest)

# Setting the parameters that you want to predict eg SEX
library(e1071)
mySVM = svm(Sex~., data = dfTraining)
summary(mySVM)
# Number of support vecter: 62(35 27) which explains number of class information used to determine the best hyperplane
plot(mySVM, data = dfTraining) # crosses are the support vectors(SEX) while circles are the normal data points
# x uses svm to define the best hyperplane

# Use the test data to evaluate the model performance
myPred <- predict(mySVM, dfTest)
myPred

# Create a confusion matrix by comparing the predicted support vectors and the test
confTable <- table(myPred, dfTest$Sex)
confTable

truePositive = confTable[1,1]
falsePositive = confTable[1,2]
falseNegetive = confTable[2,1]
trueNegative = confTable[2,2]

# Define functions to calculate: 
# the sensitivity
# the specificity
# the precision
# the accuracy

calcSens = function(truePosValues, falseNegValues){
  sens = truePosValues / (truePosValues+falseNegValues)
  return(sens)
}

calcSpec = function(trueNegValues, falsePosValues){
  spec = trueNegValues / (trueNegValues+falsePosValues)
  return(spec)
}

calcPrec = function(truePosValues, falsePosValues){
  prec = truePosValues / (truePosValues+falsePosValues)
  return(prec)
}

calcSAcc = function(truePosValues, trueNegValues, falsPosValue, falseNegValues){
  acc = (truePosValues + trueNegValues) / (truePositive+trueNegetive+falsePositive+falseNegetive)
  return(acc)
}

calcSens(truePositive,falseNegetive)
calcSpec(trueNegative,falsePositive)
calcPrec(truePositive,falsePositive)
calcSAcc(truePositive,trueNegative,falsePositive,falseNegetive)
# it shows how our classifier accurately predict .... of the value

## Excercise plant data svm ####
mydata <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

summary(mydata)

# dive the dataset into training and test
countTraining = round(nrow(mydata)*0.7)
countTraining
randomRows = sample(1:nrow(mydata), countTraining, replace = FALSE)
dfTraining = mydata[randomRows,]
dfTest = mydata[-randomRows,]

summary(dfTraining)
summary(dfTest)

# create  A Svm  for classifying the species
mySVM = svm(Species~., data = dfTraining )

summary(mySVM)

# Visualization
# more than two dimensions: fix other dimensions. by applying the slice function
plot(mySVM, data = dfTraining, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
# The points are plotted according to their values in Peta.Width and Petal.Length
#The values of Sepal.Width and Sepal.Length are fixed to 3 respectively 4.
# and this tells us that when petal length is 5 and petal width is 2.5 it is been predicted to be viginica

myPred = predict(mySVM, dfTest)
confTable = table(myPred, dfTest$Species)
confTable

# We can still calculate the accuracy
# we can use accuracy function and ignore other functions because our data has 3 variables and not 2
diag(confTable) # These are the correct classification and shows the number in the diagonal line
ACC = sum(diag(confTable)) / sum(confTable) # Number of correct classification / Number of all classifications
ACC # 0.9777778 show the percentage of accuracy of the data set that has been classified.









































