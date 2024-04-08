# Assignment 4
# Isaiah Martinez
# Comp 541 - Data Mining
# Ms. Lord
# 4/21/24

library(arules)

#import dataset
#Note: there are no NA values
data(Groceries)

#define variables for min support and confidence
min_supp <- 0.02
min_conf <- 0.2

#convert data to a transactions object
transactions <- as(Groceries, "transactions")

#A priori alg
rules <- apriori(transactions, list(support = min_supp, confidence = min_conf))

#print first 10 association rules
top10Rules <- head(rules, 10)
inspect(top10Rules)