#Exercises 28.02
plantData <- read.table("plantData_NoNA.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
size=c("small", "medium")

rndSample=sample(size, nrow(plantData), replace = T)

plantData$size = rndSample

plantData$comb = interaction(plantData$Species, plantData$size)

summary(plantData)
#Exercise: scatterplot with condition
# Step1: Normalize the Petal.Length values with 
# min-max normalization
# normalize function 
# Step2: Draw a scatter plot using normalized values
# in your plot if  0.5<normalized values<0.75 
# the corresponding points are highlighted by green
# otherwise the points color is red
# ==> You will need geom_point and scale_colour_manual function.

normPetalLength <- (plantData$Petal.Length - min(plantData$Petal.Length)) / (max(plantData$Petal.Length) - 
                                                                               min(plantData$Petal.Length))
summary(normPetalLength)

plantData$nPetalLength <- normPetalLength # Adding a new column to plantData
summary(plantData)

ggplot(data = plantData, aes(y = nPetalLength, x = seq_along(nPetalLength))) + 
  geom_point(aes(color = (nPetalLength > 0.5 & nPetalLength < 0.75))) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "green")) +
  labs(x = "Index", y = "Normalized Petal.Length")







#x_norm = (x - min(x)) / (max(x)-min(x)) 
#min - max normalization -> changes the scale to a 0 - 1 scale


#Exercise: scatterplot with several lines
# Step1: Normalize the values of Sepal length and width as well as Petal length width 
# using min-max normalization
# Step2: Save them in different vectors
# Step3: using seq_along function create a vector for Petal length
# Step4: Create a new data frame which contains all vectors created in previous steps
# Step5: Draw a scatter plot which visualizes all columns of new data frame in one plot
# Step6: Order the normalized values and draw your plot again.

normPetalLength <- (plantData$Petal.Length - min(plantData$Petal.Length)) /
  (max(plantData$Petal.Length) - min(plantData$Petal.Length))

normPetalWidth <- (plantData$Petal.Width - min(plantData$Petal.Width)) /
  (max(plantData$Petal.Width) - min(plantData$Petal.Width))

normSepalLength <- (plantData$Sepal.Length - min(plantData$Sepal.Length)) /
  (max(plantData$Sepal.Length) - min(plantData$Sepal.Length))

normSepalWidth <- (plantData$Sepal.Width - min(plantData$Sepal.Width)) /
  (max(plantData$Sepal.Width) - min(plantData$Sepal.Width))

indexVec <- seq_along(normPetalLength)

df <- data.frame(indexVec, normPetalLength, normPetalWidth, normSepalLength, normSepalWidth)
summary(df)

plot_unsortted <- ggplot(data = df, aes(x = indexVec)) + 
  geom_point(aes(y = normPetalLength, col = "PetalLength")) +
  geom_point(aes(y = normPetalWidth, col = "PetalWidth")) + 
  geom_point(aes(y = normSepalLength, col = "SepalLength")) +
  geom_point(aes(y = normSepalWidth, col = "SepalWidth")) + 
  scale_color_manual(values = c("PetalLength" = "blue", "PetalWidth" = "green", "SepalLength" = "yellow", 
                                "SepalWidth" = "orange"))
plot_unsorted

# Sorting the data values (Sorts the actual values)
plot_sorted <- ggplot(data = df, aes(x = indexVec)) + 
  geom_point(aes(y = sort(normPetalLength), col = "PetalLength")) +
  geom_point(aes(y = sort(normPetalWidth), col = "PetalWidth")) + 
  geom_point(aes(y = sort(normSepalLength), col = "SepalLength")) +
  geom_point(aes(y = sort(normSepalWidth), col = "SepalWidth")) + 
  scale_color_manual(values = c("PetalLength" = "blue", "PetalWidth" = "green", "SepalLength" = "yellow", 
                                "SepalWidth" = "orange"))
plot_sorted

# order gives the indices of the sorted values

library(gridExtra)
# Excercise : 
# Write a function that should create random subsets with different sample size from plantData with replacement
# Further, It should normalize the Petal.Width and Petal.Length values with min-max normalization
# It should draw multiple graphics on a page
# You will need “gridExtra” package and grid.arrange function
# Graphic 1: a scatter plot & use red, black and orange colors for the points according to species &
# shape the points of each species (the function is shape used with aes)
# Graphic 2: a histogram for Petal.Length & use red, black and orange colors for bars according to species
# Graphic 3: a density plot for Petal.Length with red, black and orange colors
# Graphic 4: a pie chart for plant sizes with red, and orange colors
# Graphic 5 -6: box plots with red, black and orange colors according to
# plant sizes and Petal.Width as well as Petal.Length
# split them in columns according to Species and highlight the outliers
# Save the graphics in a png file, its name is given by the user

creatPlots <- function(sampleSize, figureName){
  # create random subset of plantData
  randomNumbers = sample(1:nrow(plantData), sampleSize, replace = TRUE)
  rndData = plantData[randomNumbers,]
  # normalize petal.width
  rndData$Petal.Width = (rndData$Petal.Width-min(rndData$Petal.Width)) / 
    (max(rndData$Petal.Width) - min((rndData$Petal.Width)))
  # normalize petal.length
  rndData$Petal.Length = (rndData$Petal.Length-min(rndData$Petal.Length)) / 
    (max(rndData$Petal.Length) - min((rndData$Petal.Length)))
  # Graphic 1 : scatter plot
  p1 <- ggplot(data = rndData, aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(aes(color = Species, shape = Species)) +
    scale_color_manual(values = c("red", "black", "orange"))
  # Graphic 2 : Histogram
  p2 <- ggplot(data = rndData, aes(x = Petal.Length)) + 
    geom_histogram(binwidth = 0.2, aes(fill = Species)) + # with binwidth it always shows the same data with different resolution.
    scale_fill_manual(values = c("red", "black", "orange"))
  # Graphic 3 : Density plot
  p3 <- ggplot(data = rndData, aes(x = Petal.Length)) + 
    geom_density(aes(fill = Species)) + 
    scale_fill_manual(values = c("red", "black", "orange"))
  # Graphic 4 : pie chart
  p4 <- ggplot(data = rndData, aes(x = "", fill = size)) + geom_bar() + coord_polar(theta = "y" ) +
    scale_fill_manual(values = c("red", "orange"))
  # Graphic 5: box plot for petal.width
  p5 <- ggplot(data = rndData, aes(x = size, y = Petal.Width)) +
    geom_boxplot(aes(fill = Species), outlier.color = "green") + 
    scale_fill_manual(values = c("red", "black", "orange")) +
    facet_grid(~Species)
  # Graphic 6 : box plot for petal.length
  p6 <- ggplot(data = rndData, aes(x = size, y = Petal.Length)) +
    geom_boxplot(aes(fill = Species), outlier.color = "green") + 
    scale_fill_manual(values = c("red", "black", "orange")) +
    facet_grid(~Species)
  combinedPlot <- grid.arrange(p1,p2,p3,p4,p5,p6)
  ggsave(file = figureName, combinedPlot, height = 5, width = 5)
  #summary(rndData)
}
creatPlots(10000, "combinedPlot.png")








