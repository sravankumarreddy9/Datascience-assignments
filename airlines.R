library(readxl)
library(ggplot2)
library(cluster)
airlines_data <- read_excel("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\clustering\\EastWestAirlines.xlsx", sheet = "data")
#since we don't need id we will remove first column
airlines_data <- airlines_data[-1]
#check for na values in the dataset
sum(is.na(airlines_data))
summary(airlines_data)
#to scale free the data
normalized_data <- scale(airlines_data)

#calculating the distance 
dist <- dist(normalized_data, method = "euclidean")
str(dist)
fitted_values <- hclust(dist, method = "complete")
plot(fitted_values, hang = -1)
# If we use hierarical clustering for large dataset dendograph is not clear
rect.hclust(fitted_values,plot(fitted_values,hang=-1),k=8,border="blue")
groups <- cutree(fitted_values,k=5)
clusters <- as.matrix(groups)
final <- data.frame(airlines_data,clusters)
View(final)
aggregate(airlines_data,by=list(final$clusters),mean)

#As the data sample is large we will use the k means clustering

#K means clustering
set.seed(20)
k_means <- kmeans(normalized_data,5)
str(k_means)
withss <- sapply(1:12,
                 function(k) {
                   kmeans(normalized_data, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:12, withss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
clust_output <- kmeans(airlines_data, centers = 6)
k_means$centers
clust_output$centers
#storing the clustering data to the dataframe
airlinesdata <- cbind(airlines_data, k_means$cluster)
View(airlinesdata)
aggregate(airlinesdata[,1:12],by= list(airlinesdata$ 'k_means$cluster'),mean)
clara <- clara(normalized_data,5) 
clusplot(clara)
#clusters using kselection method
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=2)
install.packages("kselection")
library(kselection)
k_value <- kselection(normalized_data, parallel = TRUE, k_threshold = 0.9)
k_value

