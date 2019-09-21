library(readr)
books_data <- read.csv(file.choose())

library(arules)
library(arulesViz)

class(books_data)

rules <- apriori(as.matrix(books_data[1:11],parameter=list(support=0.02, confidence = 0.60,minlen=2)))
rules
#set of rules is 7 if minlen = 2
rules_conf <- sort(rules, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules, by = "lift")
inspect(rules_lift)

head(quality(rules))
plot(rules, method = "scatterplot")
plot(rules, method = "grouped")
plot(rules, method = "graph")

rules1 <- apriori(as.matrix(books_data[1:11],parameter=list(support=0.3, confidence = 0.70,minlen=3)))
rules1
rules_conf <- sort(rules1, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules1, by = "lift")
inspect(rules_lift)

plot(rules1, method = "scatterplot")
plot(rules1, method = "grouped")
plot(rules1, method = "graph")

rules2 <- apriori(as.matrix(books_data[1:11],parameter=list(support=0.5, confidence = 0.90,minlen=4)))
rules2
#set of rules is 7 if minlen = 4
rules_conf <- sort(rules2, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules2, by = "lift")
inspect(rules_lift)

plot(rules2, method = "scatterplot")
plot(rules2, method = "grouped")
plot(rules2, method = "graph")


rules3 <- apriori(as.matrix(books_data[1:11],parameter=list(support=6, confidence = 0.99,minlen=9)))
rules3
#set of rules is 7 if minlen = 9
rules_conf <- sort(rules3, by = "confidence")
inspect(rules_conf)
rules_lift <- sort(rules3, by = "lift")
inspect(rules_lift)

plot(rules3, method = "scatterplot")
plot(rules3, method = "grouped")
plot(rules3, method = "graph")


# there is no change in the set of rules if we change support, confidence and minlen values.
