library(readr)
customer<-read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\hypothesis testing\\customer order form.csv")
Stacked_values<-stack(customer)
chisq.test(Stacked_values$values, Stacked_values$ind)
#p = 0.2771 > 0.05 hence accpet the null hypothesis
#H0 : Errors are eqaul in centres
#Ha : Errors are not equal in centres