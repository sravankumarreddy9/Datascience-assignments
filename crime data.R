library(readr)
c_data <- read.csv(file.choose())
#normalizing continous columns to bring them under same scale
#removing the first column and selecting remaining all columns
normalized_data <- scale(c_data[,2:5])
#calculate the distance of the points for checking the variances for clustering analysis
d <- dist(normalized_data, method = "euclidean")
#hclust is hierarchy clustering it will check the variances and reduce the variances 
fit <- hclust(d, method = "complete")
#checking for dendograph 
plot(fit)
#To put in order
plot(fit, hang = -1)
#creating rectangle boxes to divide the data
#selecting k as 5 
rect.hclust(fit, k=5, border = "blue")
groups <- cutree(fit, k=5)
rank <- as.matrix(groups)
final <- data.frame(c_data, rank)
View(final)
write.csv(final, file = "final c_data.csv", row.names = F)
getwd()
aggregate(c_data[,-1],by=list(final$rank),mean)

#checking for different k 
rect.hclust(fit, k=9, border = "red")
groups1 <- cutree(fit, k=9)
rank1 <- as.matrix(groups1)
final1 <- data.frame(c_data, rank1)
View(final1)
write.csv(final, file = "final1 c_data.csv", row.names = F)
aggregate(c_data[,-1], by=list(final1$rank1), mean)
