library(readr)
books_data <- read.csv(file.choose())

library(arules)
library(arulesViz)

class(books_data)

rules <- apriori(as.matrix(books_data[1:11],parameter=list(support=0.02, confidence = 0.95,minlen=2)))
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
