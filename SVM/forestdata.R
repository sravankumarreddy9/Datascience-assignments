library(readr)
library(dplyr)
install.packages("e1071")
library(el071)
install.packages("kernlab")
library(kernlab)
library(caret)
forest_data <- read.csv(file.choose())
attach(forest_data)
hist(forest_data$area)
rug(forest_data$area)
#transform area into the new response variable y
forest_data <- mutate(forest_data, y=log(area + 1))
hist(forest_data$y)
#normalize the data 
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}
forest_data$temp <- normalise(forest_data$temp)
forest_data$rain <- normalise(forest_data$rain)
forest_data$RH <- normalise(forest_data$RH)
forest_data$wind <- normalise(forest_data$wind)
#classification
sum(forest_data$area >= 5)
forest_data$size_category <- NULL
#splitting the fires unevenly
forest_data$size_category <- factor(ifelse(forest_data$area < 5, 1, 0), labels = c("small", "large"))
#splitting the data
train <- sample(x = nrow(forest_data), size = 400, replace = FALSE)
#methods 
m.poly <- ksvm(size_category ~ temp + RH + wind + rain, data = forest_data[train, ], kernel = "polydot", C=1)
m.poly
m.rad <- ksvm(size_category~temp+RH+wind+rain, data=forest_data[train, ], kernel="rbfdot", C=1)
m.rad
m.tan <- ksvm(size_category~temp+RH+wind+rain, data=forest_data[train, ], kernel="tanhdot", C=1)
m.tan
#testing with training data
pred <- predict(m.rad, newdata = forest_data[-train, ], type="response")
table(pred, forest_data[-train, "size_category"])
confusionMatrix(table(pred, forest_data[-train, "size_category"]), positive = "small")
"""""""""""""""""""""""""""""""""""""""""
Confusion Matrix and Statistics

       
pred    small large
  small     2     0
  large    36    79
                                          
               Accuracy : 0.6923          
                 95% CI : (0.6003, 0.7743)
    No Information Rate : 0.6752          
    P-Value [Acc > NIR] : 0.3877          
                                          
                  Kappa : 0.0698 """"""""""""""""""" 
                  
""""""""""""""""""""""""""""""""""""""""        
Mcnemar'sTest P-Value : 5.433e-09       

Sensitivity : 0.05263         
Specificity : 1.00000         
Pos Pred Value : 1.00000         
Neg Pred Value : 0.68696         
Prevalence : 0.32479         
Detection Rate : 0.01709         
Detection Prevalence : 0.01709         
Balanced Accuracy : 0.52632         

'Positive' Class : small""""""""
