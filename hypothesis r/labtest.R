library(readr)
lab<-read.csv(file.choose())
stacked_data=stack(lab)
summary(stacked_data)
library(nortest)
ad.test(stacked_data$values)
#p value is slightly greater than 0.05 hence it follows normal distribution.

Anovaresults<-aov(values ~ ind, data = stacked_data)
summary(Anovaresults)
#here p value is less than 0.05 hence we accept the alternative hypothesis.
#H0: There is no difference of TAT reports.
#Ha: There is differnce between atleast two labs or completely different.