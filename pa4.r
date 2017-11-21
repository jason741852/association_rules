require('arules')
require('lattice')
library('datasets')
require('ggplot2')
require('reshape2')
require('arulesViz')



#------------------------------- Load dataset -------------------------------#
## Need to parse the summary as vectors to get the number of items per transaction distribution for the histogram
df <- data(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
x <- capture.output(summary(Groceries))[13]
x <- paste(capture.output(summary(Groceries))[13], capture.output(summary(Groceries))[15], sep=" ")
x <- strsplit(x,"\\s+")[[1]]
x <- x[-1]

y <- paste(capture.output(summary(Groceries))[14], capture.output(summary(Groceries))[16], sep=" ")
y <- strsplit(y,"\\s+")[[1]]

x <- as.numeric(x)
y <- as.numeric(y)
x <- data.frame(x)
y <- data.frame(y)
df <- data.frame(x,y)

#------------------------------- plot frequency -------------------------------#
barplot(df$y, ylim=c(0,2500), xlim=c(0,32), names.arg=df$x, las=2, xlab = "Number of items in a transaction", ylab="Frequency")


#------------------------------- get numbers of itemset with support >= 0.001 -------------------------------#
rules <- apriori(Groceries, parameter = list(target='frequent', supp = 0.001, conf=0))

rules <- apriori(Groceries, parameter = list(target="maximally frequent itemsets", supp = 0.001, conf=0))

rules <- apriori(Groceries, parameter = list(target="closed", supp = 0.001, conf=0))


# #------------------------------- get numbers of itemset with support >= 0.01 -------------------------------#
rules <- apriori(Groceries, parameter = list(target='frequent', supp = 0.01, conf=0))

rules <- apriori(Groceries, parameter = list(target="maximally frequent itemsets", supp = 0.01, conf=0))

rules <- apriori(Groceries, parameter = list(target="closed", supp = 0.01, conf=0))


#------------------------------- get top 10 itemsets with the highest support -------------------------------#
rules <- apriori(Groceries, parameter = list(target='frequent', supp = 0.01, conf=0)) # support value doesn't matter
options(digits=2)
inspect(sort(rules, decreasing = TRUE, na.last = NA, by = "support")[1:10])


#------------------------------- get itemset with support >= 0.01 and confidence >= 0.9 -------------------------------#
rules <- apriori(Groceries, parameter = list(confidence=.9, supp = 0.01))
inspect(rules)
# o itemsets

# #------------------------------- adjust confidence of itemset with support >= 0.01 until we get 10 itemset -------------------------------#
rules <- apriori(Groceries, parameter = list(confidence=.5175, supp = 0.01))
inspect(rules)


# ------------------------------- plot itemset with support >= 0.01 and confidence >= 0.5 ------------------------------- #
rules <- apriori(Groceries, parameter = list(confidence=.5, supp = 0.01))
whole_milk_itemset <- inspect(sort(subset(rules, subset=rhs %in% 'whole milk', by = 'lift',
                    decreasing = T)))

# what plot does he want?
