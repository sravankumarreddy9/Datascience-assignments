library(cluster)
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")
library(fpc)
install.packages("eclust")
library("eclust")
library("cluster")


library(NbClust)
wine_data <- read.csv(file.choose())
pca <- princomp(wine_data[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca)
plot(pca)
biplot(pca)
clust = NbClust(wine_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")
#Among all indices:                                                
#* 6 proposed 2 as the best number of clusters 
#* 5 proposed 3 as the best number of clusters 
#* 1 proposed 4 as the best number of clusters 
#* 7 proposed 7 as the best number of clusters 
#* 1 proposed 8 as the best number of clusters 
#* 4 proposed 10 as the best number of clusters 
 # * According to the majority rule, the best number of clusters is  7 #
fviz_nbclust(clust) + theme_minimal()
#optimal number of clusters = 7
#heriarchy clustering
hclust.complete = eclust(wine_data, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 
#k means clustering
k_means= (wine_data, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")
#cluster analysis for pca components.
wine_data.pca = wine_data[,2:14]

clust_num = NbClust(wine_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")
#best number of clusters = 7
fviz_nbclust(clust_num) + theme_minimal()
#optimal number of clusters = 7
#K means clustering for pca components
k_val = eclust(wine_data, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(k_val, geom = "point", frame.type = "norm")
#pca applied for all variables 90% of the data is inferred from the first 7 variables. 
#From the graphs for 13 and 7 variables number of clusters formed are 7.
#heirarchy clustering for original dataset
wine_data <- wine_data[-1]
normalized_heriarchy <- scale(wine_data)
dist <- dist(normalized_heriarchy, method = "euclidean")
fit <- hclust(dist, method="complete")
fit
plot(fit) 
groups <- cutree(fit, k=7)
membership<-as.matrix(groups)
final <- data.frame(wine_data, membership)
View(final)
write.csv(final, file="final.csv",row.names = F)
aggregate(wine_data[,-1],by=list(final$membership),mean)
getwd()
#optimal number of clusters for original dataset is 7
#k means clustering for original dataset
set.seed(20)
k_means <- kmeans(normalized_heriarchy,5)
str(k_means)
withss <- sapply(1:13,
                 function(k) {
                   kmeans(normalized_heriarchy, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:13, withss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")
clust_output <- kmeans(wine_data, centers = 6)
k_means$centers
clust_output$centers
#storing the clustering data to the dataframe
wine_data <- cbind(wine_data, k_means$cluster)
View(wine_data)
aggregate(wine_data[,1:13],by= list(wine_data$ 'k_means$cluster'),mean)
clara <- clara(wine_data,7) 
clusplot(clara)
#clusters using kselection method
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=2)
install.packages("kselection")
library(kselection)
k_value <- kselection(normalized_heriarchy, parallel = TRUE, k_threshold = 0.9)
k_value
#final observations 
#optimal no of clusters considering pca is 7
#heirarchal clustering original dataset is 7
#k means clustering original dataset is 5