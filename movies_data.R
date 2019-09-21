library(readr)
movies_data <- read.csv(file.choose())
library(arules)
library(arulesViz)
class(movies_data)
rules <- apriori(as.matrix(movies_data[1:10],parameter=list(support=0.01, confidence = 0.60,minlen=2)))
rules
#set of rules is 77 for support 0.01, confidence 0.60, minlen 2
rules_conf <- sort(rules, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules, by = "lift")

plot(rules, method = "scatterplot")
plot(rules, method = "grouped")
plot(rules, method = "graph")


#seeing for different support confidence and minlen to check for any change in set of rules
rules1 <- apriori(as.matrix(movies_data[1:10],parameter=list(support=0.2, confidence = 0.80,minlen=3)))
rules1
#still it is 77 rules 
rules_conf <- sort(rules1, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules1, by = "lift")

plot(rules1, method = "scatterplot")
plot(rules1, method = "grouped")
plot(rules1, method = "graph")

rules2 <- apriori(as.matrix(movies_data[1:10],parameter=list(support=0.9, confidence = 0.95,minlen=5)))
rules2
#still it is 77 rules
rules_conf <- sort(rules2, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules2, by = "lift")

plot(rules2, method = "scatterplot")
plot(rules2, method = "grouped")
plot(rules2, method = "graph")

rules3 <- apriori(as.matrix(movies_data[1:10],parameter=list(support=0.4, confidence = 0.90,minlen=6)))
rules3
#still it is 77 rules for different minlen values.
rules_conf <- sort(rules3, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules3, by = "lift")

plot(rules3, method = "scatterplot")
plot(rules3, method = "grouped")
plot(rules31, method = "graph")