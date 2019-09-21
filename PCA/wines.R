library(readr)
wines_data <- read.csv(file.choose())
wines_data<- wines_data[-1]
attach(wines_data)
cor(wines_data)
pca <- princomp(wines_data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pca)
summary(pca)
str(pca)
loadings(pca)
plot(pca)
biplot(pca)
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")
pca$scores[,1:3]
wines_data<-cbind(wines_data,pca$scores[,1:3])
cluster_data<-wines_data[,14:16]

# Normalizing the data 
normalized_data<-scale(cluster_data) # Scale function is used to normalize data
dist1<-dist(normalized_data,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,wines_data) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,14:16)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="wines_data",row.names = F,col.names = F)
getwd()
