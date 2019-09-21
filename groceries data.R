library(arules)
library(arulesViz)
groceries_data <- read.transactions(file.choose(), format = "basket")
inspect(groceries_data[1:10])
class(groceries_data)
itemFrequencyPlot(groceries_data,topN=20)
groceries_rules<-apriori(groceries_data,parameter = list(support = 0.002,confidence = 0.50,minlen=2))
#set of rules has been decreasing if we increase the minlen so two is fine.
#for minlen =2 set of rules = 101, for 3 it is 55, for 4 it is 3 for 5 it is 0.
groceries_rules
rules_conf <- sort(groceries_rules, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules, by = "lift")
inspect(rules_lift)
head(quality(groceries_rules))
inspect(groceries_rules)

plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")

#for checking different support, confidence and minlen values to check the change of set of rules
groceries_rules1 <-apriori(groceries_data,parameter = list(support = 0.001,confidence = 0.60,minlen=2))
groceries_rules1
#for support 0.001 confidence 0.60 and minlen =2 set of rules is 254
rules_conf <- sort(groceries_rules1, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules1, by = "lift")
inspect(rules_lift)

plot(groceries_rules1,method = "scatterplot")
plot(groceries_rules1,method = "grouped")
plot(groceries_rules1,method = "graph")

groceries_rules2 <-apriori(groceries_data,parameter = list(support = 0.005,confidence = 0.80,minlen=2))
groceries_rules2
#for support 0.005, condidence 0.80, minlen = 2 set of rules is 14
rules_conf <- sort(groceries_rules2, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules2, by = "lift")
inspect(rules_lift)

plot(groceries_rules2,method = "scatterplot")
plot(groceries_rules2,method = "grouped")
plot(groceries_rules2,method = "graph")

groceries_rules3 <-apriori(groceries_data,parameter = list(support = 0.001,confidence = 0.60,minlen=3))
groceries_rules3
#for minlen 3 it is 166 rules
rules_conf <- sort(groceries_rules3, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules3, by = "lift")
inspect(rules_lift)

plot(groceries_rules3,method = "scatterplot")
plot(groceries_rules3,method = "grouped")
plot(groceries_rules3,method = "graph")

groceries_rules4 <-apriori(groceries_data,parameter = list(support = 0.001,confidence = 0.60,minlen=4))
groceries_rules4
#for minlen 4 it is 36 rules
rules_conf <- sort(groceries_rules4, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules4, by = "lift")
inspect(rules_lift)

plot(groceries_rules4,method = "scatterplot")
plot(groceries_rules4,method = "grouped")
plot(groceries_rules4,method = "graph")

groceries_rules5 <-apriori(groceries_data,parameter = list(support = 0.001,confidence = 0.90,minlen=5))
groceries_rules5
#for minlen 5 it is 0 rules
rules_conf <- sort(groceries_rules5, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules5, by = "lift")
inspect(rules_lift)

plot(groceries_rules5,method = "scatterplot")
plot(groceries_rules5,method = "grouped")
plot(groceries_rules5,method = "graph")
#if support increases there is decrease in the set of rules for minlen = 5 set of rules become 0.
groceries_rules6 <-apriori(groceries_data,parameter = list(support = 0.1,confidence = 0.80,minlen=2))
groceries_rules6
#it is 0 set of rules
rules_conf <- sort(groceries_rules6, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(groceries_rules6, by = "lift")
inspect(rules_lift)

plot(groceries_rules6,method = "scatterplot")
plot(groceries_rules6,method = "grouped")
plot(groceries_rules6,method = "graph")