library(arules)
library(arulesViz)

instacart_products = read.transactions(choose.files())
str(instacart_products)

itemFrequencyPlot(instacart_products, topN = 10, type = "relative")

rules = apriori(instacart_products, parameter = list(supp = 0.001, conf = 0.8))

options(digits = 10)
inspect(rules[1:6])

rules = sort(rules, by="confidence", decreasing = TRUE)
rules
 

plot(rules, method="graph")


