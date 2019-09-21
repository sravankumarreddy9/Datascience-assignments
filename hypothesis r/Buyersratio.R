library(readr)
buyers<-read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\hypothesis testing\\BuyerRatio.csv")
males <- c(50, 142, 131, 70)
females <-c(435,1523, 1356, 750)
shapiro.test(males)
#p = 0.34 > 0.05 data is normally distributed
shapiro.test(females)
#p = 0.54 > 0.05 data is normally distributed
 
chisq.test(males, females)
# p = 0.21 > 0.05 hence accept null hypothesis
#H0 : All proportions are equal. Male and Female buyers are same across regions
#Ha = All proportions are not equal. Male and Female buyers are not same across regions

