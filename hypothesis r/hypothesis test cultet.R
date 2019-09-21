library(readr)

cutlets_hyp<-read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\hypothesis testing\\Cutlets.csv")

#normality test

shapiro.test(cutlets_hyp$cutlet_a)

#for unit a  p value is 0.32 > 0.05 so p high null fly it follows normal distribution.

shapiro.test(cutlets_hyp$cutlet_b)

#for unit b p value is 0.52 > 0.05 so p high null fly it follows normal distribution.
 var.test(cutlets_hyp$cutlet_a, cutlets_hyp$cutlet_b)
 
 #Here p value is 0.31 which is greater. p high null fly. variances are equal.
 #H0 = Accept the null hypothesis - Data are normal
 #Ha = Reject the null hypothesis - Data are not normal.
 # we perform two sample t-test where x is discrete and y is continous
  t.test(cutlets_hyp$cutlet_a, cutlets_hyp$cutlet_b, alternative = "two.sided", conf.level = 0.95, correction=TRUE)
# p value is 0.47 which is greater than 0.05 We accept null hypothesis.
# H0 = Average of diameters of cutlets unit a is equal to average of diameters of cutlets unit b.
# Ha = Average of unit a is not equal to Average of unit b.
  
  