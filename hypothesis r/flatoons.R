flatoons<-read.csv(file.choose())
view(flatoons)
attach(flatoons)
table(Weekdays, Weekend)
chisq.test(table(Weekdays, Weekend))
prop.test(x=c(47,113), n=c(287,167), conf.level = 0.95, alternative = "two.sided", correct = FALSE)
prop.test(x=c(47,113), n=c(287,167), conf.level = 0.95, alternative = "less", correct = FALSE)      
#p = 0.67 > 0.05 hence accept null hypothesis
#H0 : Proportion of Males in weekdays is less than weekends.
#Ha : Proportion of Males in weekdays is greater than weekends.