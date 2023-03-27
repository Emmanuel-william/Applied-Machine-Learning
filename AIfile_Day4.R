# Day 4: k-means and Hierarchical clustering

# Image segmentation with kmeans clustering
library(jpeg)
myimage <- readJPEG("bird.jpg")
dim(myimage)
imgDim <- dim(myimage)
imgDim
#1 cordinate is height
# 2 cordinate is length
# 3 cordinate is combination of (RGB values) color

# plot the image with plot(as.raster())
plot(as.raster(myimage))

# create a data frame to visualize the image with ggplot
imgRGB = data.frame(
  x = rep(1:imgDim[2], each = imgDim[1]),
  y = rep(imgDim[1]:1, imgDim[2]),
  R = as.vector(myimage[,,1]),
  G = as.vector(myimage[,,2]),
  B = as.vector(myimage[,,3])
)

dim(imgRGB)

ggplot(data = imgRGB, aes(x = x, y = y)) + geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) + 
  labs(x = "X-Coordinate", y = "Y-Coordinate", title = "Colorful bird")

# Cluster the pixel values for k = 2 clusters
clusterResult <- kmeans(imgRGB[, c("R", "G", "B")], centers = 2)
clusterResult$cluster # not so informative instead
clusterResult$centers

clusterColors <- rgb(clusterResult$centers[clusterResult$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + geom_point(colour = clusterColors) + 
  labs(x = "X-Coordinate", y = "Y-Coordinate", title = "k-means clustering of 2 colors")

# Cluster the pixel values for k = 3 clusters
clusterResult <- kmeans(imgRGB[, c("R", "G", "B")], centers = 3)
clusterResult$cluster # not so informative instead
clusterResult$centers

clusterColors <- rgb(clusterResult$centers[clusterResult$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + geom_point(colour = clusterColors) + 
  labs(x = "X-Coordinate", y = "Y-Coordinate", title = "k-means clustering of 3 colors")


# Cluster the pixel values for k = 150 clusters
clusterResult <- kmeans(imgRGB[, c("R", "G", "B")], centers = 150)
clusterResult$cluster # not so informative instead
clusterResult$centers

clusterColors <- rgb(clusterResult$centers[clusterResult$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + geom_point(colour = clusterColors) + 
  labs(x = "X-Coordinate", y = "Y-Coordinate", title = "k-means clustering of 150 colors")

# Excerscise 1
# Image segmentation with kmeans clustering
library(jpeg)
myimage2 <- readJPEG("cows_on_meadow.jpg")
dim(myimage2)
imgDim2 <- dim(myimage2) # 
imgDim2

# plot the image with plot(as.raster())
plot(as.raster(myimage2))

# create a data frame to visualize the image with ggplot
imgRGB2 = data.frame(
  x = rep(1:imgDim2[2], each = imgDim2[1]),
  y = rep(imgDim2[1]:1, imgDim2[2]),
  R = as.vector(myimage2[,,1]),
  G = as.vector(myimage2[,,2]),
  B = as.vector(myimage2[,,3])
)

head(imgRGB2)
dim(imgRGB2)

ggplot(data = imgRGB2, aes(x = x, y = y)) + geom_point(colour = rgb(imgRGB2[c("R", "G", "B")])) + 
  labs(x = "X-Coordinate", y = "Y-Coordinate", title = "Cows for Easter")

for (k in 1:5) {
  clusterResult4 = kmeans(imgRGB2[, c("R", "G", "B")], centers = k)
  clusterColors2 = rgb(clusterResult4$centers[clusterResult4$cluster,]) # rgb here converts the centers to a real color
  myPlot = ggplot(data = imgRGB2, aes(x = x, y = y)) + 
    geom_point(colour = clusterColors2) + 
    labs(x = "X-Coordinate", y = "Y-Coordinate", title = paste("Cows for Easter - k-means clustering with "))
  ggsave(paste("Cowplot_", k, ".jpeg"), myPlot)
  
}

########### PRINCIPLE COMPONENT ANALYSIS ###########
mydata <- read.table("plantData_NoNa.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
mydata

df1 <- mydata[,"Species"]
df2 <- mydata[, c(1,2,3,4)]

summary(df2)
# ?prcomp
res.pca <- prcomp(df2, scale. = TRUE) # because our vectors are not scaled set them to have same variable and variance
summary(res.pca) # Explained variance/variation by 

res.pca$rotation # the loading scores for the principle components

res.pca$x # PCA coordinates or the positions of the plants

# PCA plot - Graph of the individual (here plants)
plot(x = res.pca$x[,1], y = res.pca$x[,2], col = df1, xlab = "PC1", ylab = "PC2")

# ggplot, we need a data frame

pca.data = data.frame(Species = df1, X = res.pca$x[,1], Y = res.pca$x[,2])
head(pca.data)

ggplot(data = pca.data, aes(x = X, y = Y, color = Species)) +
  geom_point() + labs(x = "Principle Component 1", y = "Principle Component 2")

# Scree Plot
pca.var = res.pca$sdev^2 # prcomp only gives root values therefore we need to square it 
pca.var

# Transform the aabsolute values to percentage
pca.var.per = pca.var / sum(pca.var) * 100
pca.var.per

# Scree plot using barplot function
barplot(pca.var.per, xlab = "Principle Component", ylab = "Percent Variation")

# Using ggplot
ggplot(data = data.frame(Percentage = pca.var.per), aes(x = seq_along(pca.var.per), y = Percentage)) + 
  geom_col() +  labs(x = "Principle Component", y = "Percent Variation")

# Inspect the loading scores 
# Which attribute have the largest effect on the first two PC's?
# Loading scores of PC1
res.pca$rotation
pc1_loading_scores = res.pca$rotation[,1]
pc1_loading_scores

# Transform to absolute values to sort them according to effect size
pc1_attribute_scores = abs(pc1_loading_scores)
pc1_attribute_scores_ranked = sort(pc1_attribute_scores, decreasing = TRUE)
pc1_attribute_scores_ranked

# Loading scores of PC2
pc2_loading_scores = res.pca$rotation[,2]
pc2_loading_scores
pc2_attribute_scores = abs(pc2_loading_scores)
pc2_attribute_scores_ranked = sort(pc2_attribute_scores, decreasing = TRUE)
pc2_attribute_scores_ranked

# Excurse Normalization vs Standardization
df2_Normal = df2
for(i in 1:ncol(df2_Normal)){
  df2_Normal[,i] = normalize(df2_Normal[,i])
}

summary(df2_Normal)
var(df2_Normal)

# This normalization neither shifts the mean to 0 nor does it scale the variance 1
df2_Stand = scale(df2)
summary(df2_Stand)
var(df2_Stand)

# PCA with factoextra
library(factoextra)

# Scree plot
fviz_eig(res.pca)

# PCA plot with factoextra
# ?fvz_pca_ind

# Graph of individuals. Individuals with similar attributes are grouped together.
fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2", # "Cosinous 2 metrix" Color the points by quality of their representation 
             gradient.cols = c("red", "green", "blue"),
             repel = F, # Avoid text overlapping
             axes = 3:4)
# Cosinus 2(cos2) : tells us how well are the individuals by the first two PCs?
#           small values indicate that other PCs are more important for the location of these individuals

# visualization of loading scores
res.pca$rotation
fviz_pca_var(res.pca)

# How much do the variable contribute to the PCs?
fviz_pca_var(res.pca, 
             col.var = "contrib", # Color by contribution to PC
             gradient.cols = c("blue", "brown", "orange"))

# Variances of how much they contribute to the principle component
fviz_pca_biplot(res.pca, repel = F,
                col.var = "blue",
                col.ind = "grey")

# Contributions of the variable to the principle component 
res.pca$rotation

# contribution of variables on PC1
fviz_contrib(res.pca, choice = "var", axes = 1) # red line shows the expected values if the variable 
# contribution are uniform (100/4 = 25)

# contribution of variable on PC2
fviz_contrib(res.pca, choice = "var", axes = 2)

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2)

# contributiin of the individuals to the principle components 
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 20)
# top = only show the best x individual with the highest contribution

















