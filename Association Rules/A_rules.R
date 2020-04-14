#Different set of rule values for Groceries Dataset using apriori algorithm.

groceries_data <- read.transactions(file.choose(),format = "basket")
View(groceries_data)
summary(groceries_data)
str(groceries_data)
class(groceries_data)

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#to see most frequent items
FrequentItem <- eclat(groceries_data,parameter = list(support=0.07,maxlen=15))
inspect(FrequentItem)
itemFrequencyPlot(groceries_data, topN=10, type="absolute", main="Item Frequency")

rules <- apriori(groceries_data,parameter = list(support=0.002,confidence=0.5,minlen=2,maxlen=5))
rules
inspect(head(sort(rules,by="lift"),n=15))
inspect(tail(sort(rules,by="lift"),n=15))
plot(rules)
quality(head(rules))
plot(rules, method = "grouped",control = list(cex=0.90))
plot(rules,method = "scatterplot",control = list(cex=0.90))
plot(rules,method = "graph",control = list(cex=0.90))

rules1 <- apriori(groceries_data,parameter = list(support=0.001,confidence=0.5,minlen=3,maxlen=5))
rules1
rules_conf <- sort (rules1, by="confidence", decreasing=T)
inspect(head(rules_conf))
rules_lift <- sort(rules1,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules1, method = "grouped",control = list(cex=0.90))
plot(rules1,method = "scatterplot",control = list(cex=0.90))
plot(rules1,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules1, rules1)) > 1) 
length(subsetRules)  
rules1 <- rules1[-subsetRules] 
rules1

subsetRules1 <- which(colSums(is.subset(rules, rules)) > 1) 
length(subsetRules1)  
rules <- rules[-subsetRules1] 
rules

#Different set of rule values for my_movies Dataset using apriori algorithm.
mymovies <- read.transactions(file.choose(),format = "basket")
View(mymovies)
summary(mymovies)
str(mymovies)
class(mymovies)

#to see most frequent items
FrequentItem <- eclat(mymovies,parameter = list(support=0.07,maxlen=15))
inspect(FrequentItem)
itemFrequencyPlot(mymovies, topN=5, type="absolute", main="Item Frequency")
dev.off()
rules3 <- apriori(mymovies,parameter = list(support=0.002,confidence=0.5,minlen=2,maxlen=5))
rules3
inspect(head(sort(rules3,by="lift"),n=15))
inspect(tail(sort(rules3,by="lift"),n=15))
plot(rules3)
quality(head(rules3))
plot(rules3, method = "grouped",control = list(cex=0.90))
plot(rules3,method = "scatterplot",control = list(cex=0.90))
plot(rules3,method = "graph",control = list(cex=0.90))

mymovies1 <- read.csv(file.choose())
names(mymovies1)
summary(mymovies1)
attach(mymovies1)
sd(Sixth.Sense)
sd(Gladiator)
sd(LOTR1)
sd(Harry.Potter1)
sd(Patriot)
sd(LOTR2)
sd(Harry.Potter2)
sd(LOTR)
sd(Braveheart)
sd(Green.Mile)

var(Sixth.Sense)
var(Gladiator)
var(LOTR1)
var(Harry.Potter1)
var(Patriot)
var(LOTR2)
var(Harry.Potter2)
var(LOTR)
var(Braveheart)
var(Green.Mile)
library(moments)
skewness(Sixth.Sense)
skewness(Gladiator)
skewness(LOTR1)
skewness(Harry.Potter1)
skewness(Patriot)
skewness(LOTR2)
skewness(Harry.Potter2)
skewness(LOTR)
skewness(Braveheart)
skewness(Green.Mile)

kurtosis(Sixth.Sense)
kurtosis(Gladiator)
kurtosis(LOTR1)
kurtosis(Harry.Potter1)
kurtosis(Patriot)
kurtosis(LOTR2)
kurtosis(Harry.Potter2)
kurtosis(LOTR)
kurtosis(Braveheart)
kurtosis(Green.Mile)

rules2 <- apriori(as.matrix(mymovies1[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules2
rules_conf <- sort (rules2, by="confidence", decreasing=T)
inspect(head(rules_conf))
rules_lift <- sort(rules2,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules2, method = "grouped",control = list(cex=0.90))
plot(rules2,method = "scatterplot",control = list(cex=0.90))
plot(rules2,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules2, rules2)) > 1) 
length(subsetRules)  
rules2 <- rules2[-subsetRules] 
rules2

subsetRules1 <- which(colSums(is.subset(rules3, rules)) > 1) 
length(subsetRules1)  
rules3 <- rules3[-subsetRules1] 
rules3

#Different set of rule values for book Dataset using apriori algorithm.
book <- read.csv(file.choose())
View(book)
summary(book)
str(book)
class(book)
names(book)
attach(book)
sd(ChildBks)
sd(YouthBks)
sd(CookBks)
sd(DoItYBks)
sd(RefBks)
sd(ArtBks)
sd(GeogBks)
sd(ItalCook)
sd(ItalAtlas)
sd(ItalArt)
sd(Florence)
var(book)

skewness(ChildBks)
skewness(YouthBks)
skewness(CookBks)
skewness(DoItYBks)
skewness(RefBks)
skewness(ArtBks)
skewness(GeogBks)
skewness(ItalCook)
skewness(ItalAtlas)
skewness(ItalArt)
skewness(Florence)

kurtosis(ChildBks)
kurtosis(YouthBks)
kurtosis(CookBks)
kurtosis(DoItYBks)
kurtosis(RefBks)
kurtosis(ArtBks)
kurtosis(GeogBks)
kurtosis(ItalCook)
kurtosis(ItalAtlas)
kurtosis(ItalArt)
kurtosis(Florence)

rules4 <- apriori(as.matrix(book,parameter=list(support=0.02, confidence = 0.5,minlen=5)))
rules4
rules_confidence <- sort (rules4, by="confidence", decreasing=T)
inspect(head(rules_confidence))
rules_lift <- sort(rules4,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules4, method = "grouped",control = list(cex=0.90))
plot(rules4,method = "scatterplot",control = list(cex=0.90))
plot(rules4,method = "graph",control = list(cex=0.90))

rules5 <- apriori(as.matrix(book,parameter=list(support=0.05, confidence = 0.7,minlen=6)))
rules5
rules_confidence <- sort (rules5, by="confidence", decreasing=T)
inspect(head(rules_confidence))
rules_lift <- sort(rules5,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules5, method = "grouped",control = list(cex=0.90))
plot(rules5,method = "scatterplot",control = list(cex=0.90))
plot(rules5,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules4, rules4)) > 1) 
length(subsetRules)  
rules4 <- rules4[-subsetRules] 
rules4

subsetRules <- which(colSums(is.subset(rules5, rules5)) > 1) 
length(subsetRules)  
rules5 <- rules5[-subsetRules] 
rules5

