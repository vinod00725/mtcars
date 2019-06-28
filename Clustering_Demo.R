rm(list=ls(all=TRUE))

#Consider mtacrs data of R-datasets
data(mtcars)
mydata <- data.frame(mtcars)
mydata <- na.omit(mydata) # listwise deletion of missing
summary(mydata)
str(mydata)

# standardize variables 
mydata <- scale(mydata) 
# simple distance calculation between HOnda Civic and Camaro Z28
# x <-mydata["Honda Civic",] 
# y <- mydata["Camaro Z28",] 
# dist(rbind(x, y)) 
# # distance between Camaroz28 and Firebird
# z <- mydata["Pontiac Firebird",] 
# dist(rbind(y, z))
# summary(mydata)

###-------------------------    Hierarchical Clustering     ------------------------###
# Ward's method 
# distance matrix euclidean
d <- dist(mydata,method = "euclidean") 
d
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram

groups <- cutree(fit, k=6) # cut tree into 5 clusters
groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=6, border="red") 
mydata_clusters=data.frame(mydata,groups)

###-------------------------    K- means Clustering     ------------------------###

# K-Means Cluster Analysis with k = 5
fit <- kmeans(mydata, 5) # 5 cluster solution
fit$withinss
fit$betweenss
#study the mdoel
fit$cluster
fit$tot.withinss

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster label to the actual data frame
mydata <- data.frame(mydata, 
                     fit$cluster)
write.csv(mydata,"kmeans_2.csv")
head(mydata)

# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}

# Ploting the within sum of square error for different clusters
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- mtcars[sample(1:nrow(mtcars),1),]
closest.cluster <- function(x) {
  cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}

# Predicting which cluster the new data point belongs to based on the distance.
closest.cluster(test_datapoint)

# Checking the cluster stability
# Building the clusters on all data
fit1 <- kmeans(mydata, 4)
# Getting the cluster numbers
x <- fit1$cluster
# Building the clusters on 90 % of data
fit2 <- kmeans(mydata[1:28,], 4)
# Getting the cluster numbers
y <- fit2$cluster

# install.packages("factoextra")
library(factoextra)
# Visualize the clisters based on first 2 pricipal components
fviz_cluster(fit1, data = mydata)

# Loading the required libraries
library(dtwclust)
# Checking whether the same data points are falling into same cluster 
# when we cluster on all the data or 90% of data.
randIndex(x[1:28], y)
library(mclust)
# Checking whether the same data points are falling into same cluster 
# when we cluster on all the data or 90% of data.
adjustedRandIndex(x[1:28], y)
