library(readr)
library(caret)
library(pROC)
library(mlbench)
library(lattice)
zoo_data <- read.csv(file.choose())
str(zoo_data)
zoo <- zoo_data[,2:18]
str(zoo)
zoo$hair <- as.factor(zoo$hair)
zoo$feathers <- as.factor(zoo$feathers)
zoo$eggs <- as.factor(zoo$eggs)
zoo$milk <- as.factor(zoo$milk)
zoo$airborne <- as.factor(zoo$airborne)
zoo$aquatic <- as.factor(zoo$aquatic)
zoo$predator <- as.factor(zoo$predator)
zoo$toothed <- as.factor(zoo$toothed)
zoo$backbone <- as.factor(zoo$backbone)
zoo$breathes <- as.factor(zoo$breathes)
zoo$venomous <- as.factor(zoo$venomous)
zoo$fins <- as.factor(zoo$fins)
zoo$legs <- as.factor(zoo$legs)
zoo$tail <- as.factor(zoo$tail)
zoo$domestic <- as.factor(zoo$domestic)
zoo$catsize <- as.factor(zoo$catsize)
zoo$type <- as.factor(zoo$type)

set.seed(123)
#partitioning the data
ind <- sample(2,nrow(zoo), replace = T, prob = c(0.7,0.3))
train <- zoo[ind==1,]
test <- zoo[ind==2,]
#KNN model 
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,trControl = trcontrol, preProc = c("center","scale"))                          
fit
#optimal k value is 5
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$type)
#Accuracy is 89.6%
