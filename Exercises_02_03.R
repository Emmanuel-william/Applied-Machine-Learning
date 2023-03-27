#Exercises 02.03

#k-means Exercise####
# Read in seedsData.csv
seeds <- read.table("seedsData.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
head(seeds)
seeds
dim(seeds)
str(seeds)
summary(seeds)

# Normalize the variables
#  Divide bean_data in two data frames as df1 and df2
#  df1 contains the attribute “Class”
#  df2 contains the remaining information
df1 <- as.factor(seeds[, "Variety"])
df2 <- seeds[, c(1:7)]
summary(df2)
summary(df1)
#  Normalize the values in df2 between 0 and 1 using our own function  
# normalize function                                                    
normalize = function(x){
  normalizedVector = (x - min(x)) / (max(x) - min(x))
  return(normalizedVector)
}

# loop over the columns in the data frame
for (column in 1:ncol(df2)) {
  df2[,column] = normalize(df2[,column])
}

summary(df2)

# Perform k-means clustering for k = 1 to 10 with kmeans()
# Start by creating an empty vector
wss <- c()

for(k in 1:10){
  clusterSeeds = kmeans(df2, centers = k)
  wss[k] = sum(clusterSeeds$withinss) # at position K we want to store the sum of within-cluster sum of squares
}
wss # show the decrease of within sum of squares in the k cluster from 1 to 10

plot(x = c(1:10), y = wss, type = "b", xlab = "Number of cluster", ylab = "Within clusters sum of squares")

# Elbow method - to identify the optimal number of clusters
#ggplot
plotValues <- data.frame(x = 1:length(wss), values = wss) # or (k = c(1:10), withinss = wss)
ggplot(data = plotValues, aes(x = x, y = values)) + geom_point() + geom_line()
# This elbow method suggests to use 3 clusters, which is seen when the number starts to bend and the rest of the length is no longer significant

clusterSeeds = kmeans(df2, centers = 3) 

# Visualize clustering with optimal number of clusters and compare it to the true classes
#   - Create histograms for the variable Area
# Color based on variety
histPlot1 <- ggplot(data = df2, aes(x = Area)) + geom_histogram(aes(fill = df1))
histPlot1
histPlot1 + scale_fill_manual(values = c("black", "red", "gold"))

# Color based on clustering
histPlot2 <- ggplot(data = df2, aes(x = Area)) + geom_histogram(aes(fill = as.factor(clusterSeeds$cluster)))
histPlot2
library(gridExtra)
grid.arrange(histPlot1, histPlot2)


#   - Create scatterplots for the variables Length_Kernel and Length_Groove
# Color based on Variety
scatterPlot1 <- ggplot(data = df2, aes(x = Length_Kernel, y = Length_Groove)) +
  geom_point(aes(color=df1)) + 
  labs(x = "Length Kernel", y = "Length Groove", title = "Length Kernel - Lengths Groove of Seeds") 
  #theme(legend.position = "right")
scatterPlot1

#  Color based on clustering
scatterplot2 <- ggplot(data = df2, aes(x = Length_Kernel, y = Length_Groove)) +
  geom_point(color=as.factor(clusterSeeds$cluster)) + 
  labs(x = "Length Kernel", y = "Length Groove", title = "Length Kernel - Lengths Groove of Seeds") 
#theme(legend.position = "right")
scatterplot2

grid.arrange(scatterPlot1, scatterplot2)

# Perform the same analysis with eclust()
clusterSeeds = eclust(df2, "kmeans") # adding dim 1 and dim 2, it shows that the data nearly explains more than 90% variation of the data

# Alternativly on how to shows the cluster results in a different manner
fviz_cluster(clusterSeeds, data = df2, geom = c("point")) # only shows symbols
fviz_cluster(clusterSeeds, data = df2, geom = c("text")) # only shows text

# Optimal number of clusters using gap statistic
fviz_gap_stat(clusterSeeds$gap_stat)
clusterSeeds$cluster # Assignment of individuals to the clusters


#Hierarchical Clustering Exercise####
# Read in plantData_NoNA.csv
plantData <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
plantData
str(plantData)
summary(plantData)

# Perform hierarchical clustering with hclust()
# Processes the dataset 

# Divide my data in two data frames as df1 and df2
df1.1 <- plantData[, "Species"]
df2.1 <- plantData[, 1:4]
rownames(df2.1) = paste(df1.1, "_", 1:150) # Assigning a species name to row 
rownames(df2.1)

summary(df2.1)
summary(df1.1)

# Compute distance matrix
distMatrix <- dist(df2.1)
distMatrix

# Perform hierachical clustering using the hclust() function
# Apply hierarchical clustering with mean linkage clustering
clusterR <- hclust(distMatrix, method = "average")

# visualization 
plot(clusterR)

clusterR = eclust(df2.1, "hclust")

fviz_dend(clusterR, rect = TRUE)

fviz_dend(clusterR, type = "circular", cex = 0.5)

fviz_dend(clusterR, type = "phylogenic", cex = 0.5)

#Exercise: Do the exercise on the last slide of Day4_Slides.pdf
# Import the data file called “ArtificialGen.csv” into the variable mydata
# Process the dataset
#  Inspect the data and check for missing values - No NA's

artGen <- read.table("ArtificialGen.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
artGen
str(artGen)
dim(artGen) # 100 rows and 301 columns
summary(artGen)
which(artGen == "NA")

#  Divide mydata in two data frames as df1 and df2
#  df1 contains the attribute “AnimalClass”
#  df2 contains the remaining information
df1.2 <- artGen[, "AnimalClass"]
df2.2 <- artGen[, 2:301]
summary(df1.2)
summary(df2.2)

#  Apply principal component analysis(PCA)
resPca <- prcomp(df2.2, scale. = TRUE) # because our vectors are not scaled so set them to have same variable and variance or standardized
summary(resPca) # Explained variance/variation of the components

resPca$rotation # the loading scores for the principle components

resPca$x # PCA coordinates or the positions of the plants

# PCA plot - Graph of the individual (here plants)
plot(x = resPca$x[,1], y = resPca$x[,2], col = df1, xlab = "PC1", ylab = "PC2")

# Creating a data frame to visualize data in ggplot (NB) = ggplot always use dataframe to take on the plot
plotValues = data.frame(classInfo = df1.2, pc1 = resPca$x[,1], pc2 = resPca$x[,2])
head(plotValues)

ggplot(data = plotValues, aes(x= pc1, y = pc2)) + geom_point(aes(color = classInfo))

# Graph of individuals
fviz_pca_ind(resPca) # Principal component analysis (PCA) reduces the dimensionality of multivariate data, 
# to two or three that can be visualized graphically with minimal loss of information. fviz_pca() provides 
# ggplot2-based elegant visualization of PCA outputs from: i) prcomp and princomp [in built-in R stats],

#  Analyze the results of PCA
#  Draw a 2-D plot of the elements using the first two principal components
#  Calculate how much percentage of variation each principal component accounts for and plot it
# Explaining the variances 
pca.var = resPca$sdev^2
pca.var
pca.var.per = pca.var / sum(pca.var) * 100
pca.var.per

# Scree plot to visualize importanceof PCs
barplot(pca.var.per, xlab = "Principle Component", ylab = "Percent Variation")

# this fviz_eig calculate the variance automatically
fviz_eig(resPca) # By default it show only top 10 PCs
# we canchange that to 
fviz_eig(resPca, ncp = 20) # ncp determines how many species are shown

# to determine which variable contribute much to the PCs
# loading plots 
fviz_pca_var(resPca)

fviz_contrib(resPca, choice = "var", axes = 1) # you see how the 300 variables affect the PCs 1
fviz_contrib(resPca, choice = "var", axes = 1, top = 20) # top 20 genes that determine PC1
fviz_contrib(resPca, choice = "var", axes = 2, top = 20) # top 20 genes that determine PC2

resPca = PCA(df2.2, scale.unit = TRUE)
Investigate(resPca, document = "html_document")
#  Determine which variables have the largest effect on the first two principal components using the loading scores










