# Loading the seed data
seed <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', header=F)
seed <- seed[,1:7]
colnames(seed) <- c("area", "perimeter","campactness", "length", "width", "asymmetry", "groovelength")

# Scaling the data
seed <- scale(seed) 

# Calculate the pairwise distance
# install.packages("factoextra")
library(factoextra)
distance <- get_dist(seed)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#### K-Means Cluster Analysis ####
fit <- kmeans(seed, centers = 2, nstart = 25) # 2 clusters solution with 25 different initial configurations
# Display number of observations in each cluster
table(fit$cluster)

# The kmeans object that has a lot of components 
fit

# Visualization of k-means clusters
fviz_cluster(fit, data = seed)

k3 <- kmeans(seed, centers = 3, nstart = 25)
k4 <- kmeans(seed, centers = 4, nstart = 25)
k5 <- kmeans(seed, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(fit, geom = "point", data = seed) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = seed) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = seed) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = seed) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

library(fpc)
plotcluster(seed, fit$cluster)

# See exactly which items are in 1st group (Not run)
seed[fit$cluster==1,]

# get cluster means for scaled data
aggregate(seed,by=list(fit$cluster),FUN=mean)

# or alternatively, use the output of kmeans
fit$centers

# Determine number of clusters
# Approach 1
wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# Approach 2
prediction.strength(seed, Gmin=2, Gmax=15, M=10,cutoff=0.8)

# Approach 3
d = dist(seed, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(seed, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn   
}
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')

#### Hierarchial Clustering ####

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
seed.dist=dist(seed)
#Obtain clusters using the Wards method
seed.hclust=hclust(seed.dist, method="ward")
plot(seed.hclust)

#Cut dendrogram at the 3 clusters level and obtain cluster membership
seed.3clust = cutree(seed.hclust,k=3)

#See exactly which item are in third group (Not run)
seed[seed.3clust==3,]

# get cluster means for raw data
# Centroid Plot against 1st 2 discriminant functions
# Load the fpc library needed for plotcluster function
library(fpc)
#plotcluster(ZooFood, fit$cluster)
plotcluster(seed, seed.3clust)

#### Model Based Cluster Analysis ####
library(mclust)
mclust_result = Mclust(seed)
summary(mclust_result)
plot(mclust_result)
