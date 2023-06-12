# Iris Data Load
rm(list=ls())

library(MASS)
data(iris)
attach(iris)

# # Train-Test Data Split
# library(dplyr)
# set.seed(15214626)
# sample_index <- sample(nrow(iris),nrow(iris)*0.80)
# iris1 <- iris[sample_index,]
# iris2 <- iris[-sample_index,]
# iris1 %>%
#   group_by(Species) %>%
#   count()

iris1 <- iris1[,1:4]

# Stratified Sampling
iris3 <- iris %>%
  group_by(Species) %>%
  sample_n(size = 40)


#### K-Means Cluster Analysis ####
fit <- kmeans(iris1, centers = 3, nstart = 25) # 2 clusters solution with 25 different initial configurations
# Display number of observations in each cluster
table(fit$cluster)

# The kmeans object that has a lot of components 
fit

# within group sum of squares
fit$tot.withinss

# get cluster means for scaled data
aggregate(iris1,by=list(fit$cluster),FUN=mean)

# Prediction strength approach to choose number of clusters
prediction.strength(iris1, Gmin=2, Gmax=15, M=10,cutoff=0.8)

#### Hierarchial Clustering ####

#Wards Method or Hierarchical clustering
#Calculate the distance matrix
iris1.dist=dist(iris1)
#Obtain clusters using the Wards method
iris1.hclust=hclust(iris1.dist, method="ward")
plot(iris1.hclust)
