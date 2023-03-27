
# when the mean is calculated for the x and y axis, a new cluster center will emerge, and the mean iteration 
# continues until convergence

# k-means clustering ####
plantData <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(plantData)

# dividing the data frame becos we can only do the clustering with a continous variable.
df1 <- plantData[, "Species"]
df2 <- plantData[, c(1,2,3,4)]
summary(df2)

df2_DifferentScales = df2
df2_DifferentScales$Sepal.Length = df2_DifferentScales$Sepal.Length*10000
summary(df2_DifferentScales)

# normalize function 
normalize = function(x){
  normalizedVector = (x - min(x)) / (max(x) - min(x))
  return(normalizedVector)
}
# N:B normalization affects variance, 

# loop over the columns in the data frame
for(column in 1 : ncol(df2_DifferentScales)){
  df2_DifferentScales[,column] = normalize(df2_DifferentScales[,column])
}
summary(df2_DifferentScales) # Always normalize your data if you work with distance based 

clusterResult <- kmeans(df2_DifferentScales, centers = 3) # calculate the distance of the sepal lenght alone
clusterResult$cluster # Assignment of objects to the clusters
clusterResult$centers # The location of the cluster centers
clusterResult$size # Number of objects belonging to each cluster

par(mfrow = c(1,2))
plot(df2_DifferentScales[c(1,2)], col = clusterResult$cluster)
plot(df2_DifferentScales[c(1,2)], col = df1)# Different scale of sepal.length causes it to strongly influence 

plot(df2_DifferentScales[c(3,4)], col = clusterResult$cluster)
plot(df2_DifferentScales[c(3,4)], col = df1)# Other variable are basically ignored

library(ggplot2)
p1 <- ggplot(data = df2_DifferentScales, aes(x =Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = as.factor(clusterResult$cluster))) # as.factor() converts numbers to category

p2 <- ggplot(data = df2_DifferentScales, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = df1))

p3 <- ggplot(data = df2_DifferentScales, aes(x =Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = as.factor(clusterResult$cluster))) # as.factor() converts numbers to category

p4 <- ggplot(data = df2_DifferentScales, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(col = df1))

grid.arrange(p1, p2, p3, p4)

clusterResult = kmeans(df2_DifferentScales, centers = 10)
p1 <- ggplot(data = df2_DifferentScales, aes(x =Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = as.factor(clusterResult$cluster))) # as.factor() converts numbers to category

p2 <- ggplot(data = df2_DifferentScales, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = df1))

p3 <- ggplot(data = df2_DifferentScales, aes(x =Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = as.factor(clusterResult$cluster))) # as.factor() converts numbers to category

p4 <- ggplot(data = df2_DifferentScales, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(col = df1))

grid.arrange(p1, p2, p3, p4)

# k-means elbow method
# within-cluster sum of squares
clusterResult$withinss # a good kmean center will try to minimize the sum of squares

#for k = 1,2, ... 20
# Apply k-means clustering algorithm for each k-value
# calculate the total within-cluster sum of squares (sum of withinss values)
# look at the resulting curve

wss <- c()

for(k in 1:20){
  clusterResult <- kmeans(df2_DifferentScales, centers = k)
  wss[k] = sum(clusterResult$withinss)
}

(wss)

par(mfrow = c(1,1))
plot(x = c(1:20), y = wss, type = "b", xlab = "Number of cluster", ylab = "Within clusters sum of squares")

#ggplot
plotValues <- data.frame(x = 1:length(wss), values = wss)
ggplot(data = plotValues, aes(x = x, y = values)) + geom_point() + geom_line()

# k-means clustering with the factoExtra package

# factoExtra
# Powerful package for clustering
# Analysis + visualization
# main function: eclust -> Enhanced clustering analysis
library(factoextra)

clusterResult = eclust(df2_DifferentScales, "kmeans", nstart = 25) # to avoid locla optima
# nstart for multiple initial cluster assignment (here 25)
# can prevent bad local optima
# the normal kmeans function also has the parameter nstart
# the kmeans cluster value (3) was chosen by itself

# the same plot can be created for the default kmeans function
clusterResult = kmeans(df2_DifferentScales, centers = 5)
fviz_cluster(clusterResult, df2_DifferentScales)

fviz_cluster(clusterResult, data = df2_DifferentScales, geom = "point", ellipse.type = "norm") + 
  theme_bw() + labs(title = "Result of clustering wth 5 clusters")
# theme_bw changes the background of the plot.

# You can use elbow method or gap statistic method to identify the optimal number of clusters

# creates an elbow plot based on the within-cluster sum of squares
fviz_nbclust(df2_DifferentScales, kmeans, method = "wss") + geom_vline(xintercept = 3) + 
  labs(subtitle = "Elbow method") # Add a verticle line at the specified point

fviz_nbclust(df2_DifferentScales, kmeans, method = "gap_stat")


# Hierachical clustering ####
mydata <- read.table("animalData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Inspect the dataset
# What information does it contain ?
dim(mydata)
summary(mydata)
str(mydata)
View(mydata)

# Processes the dataset 

# Divide my data in two data frames as df1 and df2
df1 <- mydata[, "animal"]
df2 <- mydata[, 1:85]

summary(df2)
summary(df1)

# Compute distance matrix
distMat <- dist(df2)

# Perform hierachical clustering using the hclust() function

# Apply hierarchical clustering with mean linkage clustering
clusterResult <- hclust(distMat, method = "average")

# visualization 
plot(clusterResult)

# Using labels, we can add the class attribute "animals" to the plot
plot(clusterResult, labels = df1)

# Labels at the same level
plot(clusterResult, labels = df1, hang = -1)

# Plotting with ggdendro
library(ggdendro)

ggdendrogram(clusterResult)

# For adding meaningful labels we need an extra step at the begining before the clustering
rownames(df2)
rownames(df2) = df1
rownames(df2)

# Compute the distance matrix again and perform hierarchical clustering
distMat <- dist(df2)

clusterResult <- hclust(distMat, method = "average")

ggdendrogram(clusterResult)

# Option to present the labels horizontally
ggdendrogram(clusterResult, rotate = TRUE)

# Plotting using dendrogram objects

hcd <- as.dendrogram(clusterResult)

plot(hcd, type = "triangle")

# Pylogenetic trees
library(ape)

plot(as.phylo(clusterResult)) # plot as basic tree

plot(as.phylo(clusterResult), type = "cladogram") # cladogram

plot(as.phylo(clusterResult), type = "unrooted") # Unrooted

plot(as.phylo(clusterResult), type = "fan") # fan

# Hierarchical clustering with factoextra

clusterResult <- eclust(df2, "hclust")

# Used to create visualization of dendrograms
# Automatically suggests a threshold where to stop clustering
fviz_dend(clusterResult)
fviz_dend(clusterResult, rect = TRUE) # draws rectangle around the groups

fviz_dend(clusterResult, rect = TRUE, horiz = TRUE)

# change the type of plot
fviz_dend(clusterResult, rect = TRUE, type = "circular")
fviz_dend(clusterResult, rect = TRUE, type = "phylogenic")

