
rm(list=ls())

library(fpc)

####### hierarchical clustering #######
setwd("C:/Users/goela/Downloads")
utilities.df <- read.csv("Utilities.csv")
utilities.df
head(utilities.df)

#Q: What is the goal for clustering?
#Q: Shall I scale the data for clustering?

# look at relationship between just two variables (2D)
plot(Fuel_Cost ~ Sales, data = utilities.df, xlim = c(0, 20000), ylim = c(0, 2.5))
#Q: What do you observe? Any grouping? If so, how many groups (?k)

text(Fuel_Cost ~ Sales, labels = Company, data = utilities.df, pos = 4, 
     cex = 0.8)

## Company name is very much like name or an ID. Will not be used as one of the variables for clustering. 
# set row names to the Company column
row.names(utilities.df) <- utilities.df[, 1]

# remove the Company column
utilities.df <- utilities.df[, -1]
utilities.df

# Standardize input variables for clustering
## Note: Scaling is often times necessary for clustering
utilities.df.norm <- utilities.df
cols <- colnames(utilities.df)
for (i in cols) {
  utilities.df.norm[[i]] <- (utilities.df.norm[[i]] - mean(utilities.df[[i]])) / (sd(utilities.df[[i]]))
}
utilities.df.norm

# look at relationship between two STANDARDIZED variables (2D)
plot(Fuel_Cost ~ Sales, data = utilities.df.norm)
#Q: What do you observe? Any grouping? If so, how many groups (?k)

# compute normalized distances
dist.norm <- dist(utilities.df.norm, 
                  method = "euclidean")
dist.norm

# compute normalized distances, with only fuel_cost, sales
dist.norm <- dist(utilities.df.norm[c("Sales","Fuel_Cost")], 
                  method = "euclidean")
dist.norm

# in hclust() set argument method =
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(dist.norm,             # distance matrix
              method = "single")     # single linkage (minimum distance)
plot(hc1,               # plot a dendrogram of the clustering results
     hang = -1,         # cause the x-axis labels to hang below the 0 line
     ann = FALSE)       # turn off annotation (plot and axis titles)
abline(h=0.75, col = "red")


# compute normalized distances, with all variables
dist.norm <- dist(utilities.df.norm, 
                  method = "euclidean")
dist.norm

# in hclust() set argument method =
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(dist.norm,             # distance matrix
              method = "single")     # single linkage (minimum distance)
plot(hc1,               # plot a dendrogram of the clustering results
     hang = -1,         # cause the x-axis labels to hang below the 0 line
     ann = FALSE)       # turn off annotation (plot and axis titles)
abline(h=3.5, col = "red")
abline(h=2.6, col = "green")

## computing cluster membership by "cutting" the dendrogram
# single linkage
memb <- cutree(hc1, k = 6)
memb


####### k-means clustering #######

# run kmeans algorithm
km <- kmeans(utilities.df.norm,   # z-scores
             6)                   # number of clusters

# show cluster membership
km$cluster

# centroids
km$centers

# within-cluster sum of squares
km$withinss

# cluster size
km$size

## End Lecture Clustering, Utility Example ##


