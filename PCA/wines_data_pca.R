library(cluster)
library(factoextra)
wines_data <- read.csv(file.choose())
#removing the first column
wines_data <- wines_data[-1]
attach(wines_data)
#EDA
cor(wines_data)
#Applying princomp function
pca <- princomp(wines_data, cor = TRUE, covmat = NULL)
str(pca)
summary(pca)
loadings(pca)
#graph for principal components inportance
plot(pca)
#comp1 having highest importance
biplot(pca)
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")
#top 3 PCA scores which represents the whole data
pca$scores[,1:3]
#combining the pca scores to the dataset using cbind function
wines_data<-cbind(wines_data,pca$scores[,1:3]) 
View(wines_data)
#Hierarchial clustering
#preparing data for clustering considering only pca scores 
cluster <- wines_data[,8:10]
#normalizing the data for scale free
normalized_data <- scale(cluster)
#calculating the distance 
dist <- dist(normalized_data, method = "euclidean")
fit <- hclust(dist, method = "complete")
#for dendogram graph
plot(fit)
rect.hclust(fit, k=7, border = "red")
#cutting the dendogram with 7 clusters  
groups <- cutree(fit, 7)
#cluster numbering
membership <- as.matrix(groups)
#binding cluster membership to the original dataset
final <- cbind(membership, wines_data )
View(aggregate(final[, -c(2,16:18)], by=list(membership), FUN=mean))
write.csv(final,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()
library(plyr)
#choosing the dataset which consists with all combined pca and membership
wines_data <- read.csv(file.choose())
str(wines_data)
#normalizing the data consists only pca scores 
normalized_data <- scale(wines_data[,15:17])
#determine number of clusters by scree plot
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
#to find the no of cluster looking into elbow curve it is 7
title(sub = "K-Means Clustering Scree-Plot")
fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE)
library(eclust)
fviz_cluster(fit, geom = "point", frame.type = "norm")
final2<- data.frame(fit$cluster,wines_data)
aggregate(wines_data[,2:17], by=list(fit$cluster), FUN=mean)
table(fit$cluster)
