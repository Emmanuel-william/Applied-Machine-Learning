
# Day 2 : ggplot2 ####
library(ggplot2)

plantData <- read.table("plantData_NoNa.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
summary(plantData)

# Histogram ####
ggplot(data = plantData, aes(x = Sepal.Length)) + geom_histogram()

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_histogram(binwidth = 2)

histPlot <- ggplot(data = plantData, aes(x = Sepal.Length)) + geom_histogram(binwidth = 1, aes(fill = Species))

# You can save plot in a variable 
histPlot

histPlot + scale_fill_manual(values = c("black", "red", "gold"))

# Density plot ####
ggplot(data = plantData, aes(x = Sepal.Length)) + geom_density()

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_density(aes(color = Species))

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_density(aes(fill = Species)) # Problem of overlaping density areas

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_density(aes(fill = Species), alpha = 0.5) # alpha can help in 
# visualizing overlapping regions 

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_density(aes(fill = Species), alpha = 0.5) + 
  scale_fill_manual(values = c("yellow", "orange", "green"))

# Barplot ####
ggplot(data = plantData, aes(x = Species)) + geom_bar()

ggplot(data = plantData, aes(x = Species)) + geom_bar(aes(fill = Species))

# Pie chart #### 
ggplot(data = plantData, aes(x = "", fill = Species)) + geom_bar()
# This creates a single bar filled according to species distribution

ggplot(data = plantData, aes(x = "", fill = Species)) + geom_bar() + coord_polar(theta = "y")
# coord_polar(theta = "y) transforms a single bar into a pie chart

ggplot(data = plantData, aes(x = Species, y = Petal.Length)) + geom_col()
# the colums represent the sum of the petal.length values of all plants belonging to respective species

# Excercise
# Create random subsets with 75 sample size from plantData without replacement and draw the graphics barplot, 
# Creating subset
randomNumbers = sample(1:nrow(plantData), 75, replace = F)
randomNumbers

newData = plantData[randomNumbers,]
summary(newData)

# Barplot
ggplot(data = newData, aes(x = Species)) + geom_bar()

ggplot(data = newData, aes(x = Species)) + geom_bar(aes(fill = Species))

# Pie chart #### 
ggplot(data = newData, aes(x = "", fill = Species)) + geom_bar()
# This creates a single bar filled according to species distribution

ggplot(data = newData, aes(x = "", fill = Species)) + geom_bar() + coord_polar(theta = "y")
# coord_polar(theta = "y) transforms a single bar into a pie chart

ggplot(data = newData, aes(x = Species, y = Petal.Length)) + geom_col()
# the colums represent the sum of the petal.length values of all plants belonging to respective species

# Histogram ####
ggplot(data = newData, aes(x = Petal.Length)) + geom_histogram()

ggplot(data = newData, aes(x = Petal.Length)) + geom_histogram(binwidth = 2)

histPlot <- ggplot(data = newData, aes(x = Petal.Length)) + geom_histogram(binwidth = 1, aes(fill = Species))

# You can save plot in a variable 
histPlot

histPlot + scale_fill_manual(values = c("black", "red", "gold"))

# Excercise 2
# Write a function that should create random subsets with different sample without replacement. Futher draw the
# graphic(scatterplot, histogram or pie chart)
randomSubsets = function(dataSet, sampleSize, replacement, graphic){
  randomNumbers = sample(1:nrow(dataSet), sampleSize, replace = replacement)
  newData = dataSet[randomNumbers,]
  
  if(graphic == "scatterplot"){
    return(ggplot(data = newData, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point(aes(colour = Species)))
  }else if(graphic == "histogram"){
      return(ggplot(data = newData, aes(x = Sepal.Length)) + geom_histogram(binwidth = 1, aes(fill = Species)))
  } else if(graphic == "pie chart"){
    return(ggplot(data = newData, aes(x = "", fill = Species)) + geom_bar() + coord_polar(theta = "y"))
    }
}

randomSubsets(plantData, 75, FALSE, "piechart")
randomSubsets(plantData, 100, TRUE, "histogram")
randomSubsets(plantData, 1000, TRUE, "scatterplot")

# line plot ####
ggplot(data = plantData, aes(y = Sepal.Length, x = seq_along(Sepal.Length))) + 
  geom_line()# this generates a single line 

ggplot(data = plantData, aes(y = Sepal.Length, x = seq_along(Sepal.Length))) + # seq_along takes the row indices into account.
  geom_line(aes(group = Species)) # this generates a line for each species

ggplot(data = plantData, aes(y = Sepal.Length, x = seq_along(Sepal.Length))) + 
  geom_line(aes(group = Species, color = Species, linetype = Species)) + geom_point()
# geom_Point adds the corresponding points
# this generates a line for each species with unique color and line stye

# boxplot
ggplot(data = plantData, aes(x = Species, y = Sepal.Length)) + geom_boxplot()

ggplot(data = plantData, aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = Species), outlier.colour = "red", outlier.shape = 8, outlier.size = 4)

plotBox <- ggplot(data = plantData, aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = Species), outlier.colour = "red", outlier.shape = 8, outlier.size = 4)

plotBoxFlipped <- plotBox + coord_flip()

plotBoxFlipped + theme(legend.position = "top")

# Excercise : create a vector which contains the variable "small" and "medium" for plant sizes
# using sample function and plant size vector, create a new vector with 150 random variables
# insert this vector into planData dataframe as a new colum size
# Using interaction function  
# Create a boxbox with respect to the combined variable and sepal.length
plantSize <- c("small", "medium")
plantSize
randomSize = sample(plantSize, 150, replace = TRUE)
randomSize

plantData$size = randomSize
head(plantData)

interaction(plantData$Species, plantData$size)
plantData$comb <- interaction(plantData$Species, plantData$size)
head(plantData)

# boxplot of combined variable
# according to combined variable
ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = comb), outlier.colour = "red", outlier.shape = 8, outlier.size = 3) 

# according to size
ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = size), outlier.colour = "red", outlier.shape = 8, outlier.size = 3) 

# according to species
ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = Species), outlier.colour = "red", outlier.shape = 8, outlier.size = 3) +
   guides(fill = "none") # Removes legend that has been created for fill aesthetic

# lims() allows us to restrict the axis to specific interval
ggplot(data = plantData, aes(x = seq_along(Sepal.Length), y = Sepal.Length, shape = Species)) + 
  geom_point(aes(color = Species)) + lims(y = c(6,7), x = c(50,100))

ggplot(data = plantData, aes(x = seq_along(Sepal.Length), y = Sepal.Length, shape = Species)) + 
  geom_point(aes(color = Species)) + lims(y = c(6,7), x = c(50,100)) +
  labs(x = "Index", y = "Length of Sepal", title = "Title", subtitle = "Sub", caption = "Caption")

# Faceting a boxplot
ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + geom_boxplot(aes(fill = Species)) + 
  facet_grid(Species~.) # we splitted the plot into different rows according to the species

ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + geom_boxplot(aes(fill = Species)) + 
  facet_grid(~Species) # we split the plot into different colums according to the species

p <- ggplot(data = plantData, aes(x = comb, y = Sepal.Length)) + geom_boxplot(aes(fill = Species)) + 
  facet_grid(~Species)

ggsave("myPlot.jpg", plot = p)# to save a plot

#################################################################################################
# k-means clustering ####
plantData <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(plantData)

# dividing the data frame becos we can only do the clustering with a continous variable.
df1 <- plantData[, "Species"]
df2 <- plantData[, c(1,2,3,4)]
summary(df2)

# Before k-means we need to normalize the data
# k-means works based on euclidian distance
# Highly different variable ranges could result in misleading results

# Min-max normalization
# (x - min(x)) / (max(x) - min(x)) # formula to reduce every numerical or continous variable to min of 0 and max of 1
df2$Sepal.Length <- (df2$Sepal.Length-min(df2$Sepal.Length))/(max(df2$Sepal.Length)-min(df2$Sepal.Length))
summary(df2)

# normalize function 
normalize = function(x){
  normalizedVector = (x - min(x)) / (max(x) - min(x))
  return(normalizedVector)
}

df2$Sepal.Width = normalize(df2$Sepal.Width)

# loop over the columns in the data frame
for(column in 1 : ncol(df2)){
  df2[,column] = normalize(df2[,column])
}
summary(df2)

clusterResult <- kmeans(df2, centers = 3) # apply k-means algorithm with k=3
clusterResult

clusterResult$centers # centers of the clusters

clusterResult$size # give numbers of points in each cluster

clusterResult$cluster # gives assignments to the clusters

# visualize the clustering result
par(mfrow = c(1,2))

# plot to see how sepal.length and sepal.width data points have been distributed in cluster 
plot(df2[c(1,2)], col = clusterResult$cluster)

# plot to see how sepal.length and sepal.width data points have been distributed acc 
plot(df2[c(1,2)], col = df1)

# plot to see how sepal.length and sepal.width data points have been distributed in cluster 
plot(df2[c(3,4)], col = clusterResult$cluster)

# plot to see how sepal.length and sepal.width data points have been distributed in cluster 
plot(df2[c(3,4)], col = df1)
















