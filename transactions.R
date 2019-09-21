library(readr)
library(arules)
library(arulesViz)

transactions<-read.transactions(file.choose(),format="basket")
inspect(transactions[1:10])
class(transactions)
itemFrequencyPlot(transactions,topN=20)
transactions_rules<-apriori(transactions,parameter = list(support = 0.00002,confidence = 0.50,minlen=2))
#set of rules is 3506 for above support and minlen values
rules_conf <- sort(transactions_rules, by = "confidence")
inspect(transactions_rules)
rules_lift <- sort(transactions_rules, by = "lift")
inspect(transactions_rules)

plot(transactions_rules,method = "scatterplot")
plot(transactions_rules,method = "grouped")
plot(transactions_rules,method = "graph")

#checking for difference support and minlen values
transactions_rules<-apriori(transactions,parameter = list(support = 0.00002,confidence = 0.50,minlen=3))
#set of rules is 43 for above support and minlen values
rules_conf <- sort(transactions_rules, by = "confidence")
inspect(transactions_rules)
rules_lift <- sort(transactions_rules, by = "lift")
inspect(transactions_rules)

plot(transactions_rules,method = "scatterplot")
plot(transactions_rules,method = "grouped")
plot(transactions_rules,method = "graph")

transactions_rules<-apriori(transactions,parameter = list(support = 0.0002,confidence = 0.90,minlen=2))
#set of rules is 1553 for above support and minlen values 
#if minlen value increases set of rules decreases 
rules_conf <- sort(transactions_rules, by = "confidence")
inspect(transactions_rules)
rules_lift <- sort(transactions_rules, by = "lift")
inspect(transactions_rules)

plot(transactions_rules,method = "scatterplot")
plot(transactions_rules,method = "grouped")
plot(transactions_rules,method = "graph")

transactions_rules<-apriori(transactions,parameter = list(support = 0.002,confidence = 0.50,minlen=2))
#set of rules is 37 for above support and minlen values
#if the support increases set of rules decreases.
rules_conf <- sort(transactions_rules, by = "confidence")
inspect(transactions_rules)
rules_lift <- sort(transactions_rules, by = "lift")
inspect(transactions_rules)

plot(transactions_rules,method = "scatterplot")
plot(transactions_rules,method = "grouped")
plot(transactions_rules,method = "graph")