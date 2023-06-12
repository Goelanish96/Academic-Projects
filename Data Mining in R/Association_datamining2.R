# Market Basket Analysis

#### Retail Transaction Data ####
# install.packages("arules")
# install.packages('arulesViz')
library(arules)
library('arulesViz')
data("Groceries")

#run summary report
summary(Groceries)

# size() can specify size of items in transactions
x = Groceries[size(Groceries) > 30]
inspect(x)

# itemFrequencyPlot() shows the frequency for items
itemFrequencyPlot(Groceries, support = 0.1, cex.names=0.8)

# Run the apriori algorithm
basket_rules <- apriori(Groceries,parameter = list(sup = 0.003, conf = 0.5,target="rules"))
summary(basket_rules)

# Check the generated rules using inspect
inspect(head(basket_rules))

#Basket rules of size greater than 4
inspect(subset(basket_rules, size(basket_rules)>4))
inspect(subset(basket_rules, lift>5))

yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt" & lift>3.5)
inspect(yogurt.rhs)

meat_lhs <- subset(basket_rules, subset = lhs %in% "meat" & lift>2)
inspect(meat_lhs)

# Visualize the rules
plot(basket_rules)
plot(basket_rules, interactive=TRUE)

# Graph Based Visualization
plot(head(sort(basket_rules, by="lift"), 10), method = "graph")

# Cluster Association Rules
plot(basket_rules, method="grouped")

#### Cincinnati Zoo ####

# Loading Data
TransFood <- read.csv('https://xiaoruizhu.github.io/Data-Mining-R/data/food_4_association.csv')
TransFood <- TransFood[, -1]
# Find out elements that are not equal to 0 or 1 and change them to 1.
Others <- which(!(as.matrix(TransFood) ==1 | as.matrix(TransFood) ==0), arr.ind=T )
TransFood[Others] <- 1
TransFood <- as(as.matrix(TransFood), "transactions")

dim(TransFood)
summary(TransFood)

# Setting Rules and Inspecting
basket_rules <- apriori(TransFood,parameter = list(sup = 0.005, conf = 0.95,target="rules"))
summary(basket_rules)
inspect(head(basket_rules))
