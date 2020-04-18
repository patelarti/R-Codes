Glass <- read.csv(file.choose())
View(Glass)
str(Glass)
attach(Glass)
class(Glass)
names(Glass)
table(Glass$Type)
Glass$type = as.factor(Glass$Type)
str(Glass)

sd(RI)
sd(Na)
sd(Mg)
sd(Al)
sd(Si)
sd(K)
sd(Ca)
sd(Ba)
sd(Fe)
sd(Type)

var(RI)
var(Na)
var(Mg)
var(Al)
var(Si)
var(K)
var(Ca)
var(Ba)
var(Fe)
var(Type)

plot(RI)
plot(Na)
plot(Mg)
plot(Al)
plot(Si)
plot(K)
plot(Ca)
plot(Ba)
plot(Fe)
plot(Type)

boxplot(RI)
boxplot(Na)
boxplot(Mg)
boxplot(Al)
boxplot(Si)
boxplot(K)
boxplot(Ca)
boxplot(Ba)
boxplot(Fe)
boxplot(Type)

hist(RI)
hist(Na)
hist(Mg)
hist(Al)
hist(Si)
hist(K)
hist(Ca)
hist(Ba)
hist(Fe)
hist(Type)

library(moments)
skewness(RI)
skewness(Na)
skewness(Mg)
skewness(Al)
skewness(Si)
skewness(K)
skewness(Ca)
skewness(Ba)
skewness(Fe)
skewness(Type)

kurtosis(RI)
kurtosis(Na)
kurtosis(Mg)
kurtosis(Al)
kurtosis(Si)
kurtosis(K)
kurtosis(Ca)
kurtosis(Ba)
kurtosis(Fe)
kurtosis(Type)

round(prop.table(table(Glass$Type))*100,1)
summary(Glass[c("RI","Na","Mg")])

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_norm<- as.data.frame(lapply(Glass[1:9], norm))
View(glass_norm)
summary(glass_norm[c("RI","Na","Mg")])

#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_norm), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_norm[ind==1,]
glass_test <-  glass_norm[ind==2,]


#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(Glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- Glass[ind1==1,10]
glass_test_labels <-  Glass[ind1==2,10]

install.packages("class")
library("class")
glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels) #68.42
CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 

glass_test_pred1 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
table(glass_test_pred1,glass_test_labels)
mean(glass_test_pred1==glass_test_labels) #63.15
CrossTable(x=glass_test_labels,y=glass_test_pred1,prop.chisq = FALSE) 

glass_test_pred2 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred2,glass_test_labels)
mean(glass_test_pred2==glass_test_labels) #66.6
CrossTable(x=glass_test_labels,y=glass_test_pred2,prop.chisq = FALSE) 

glass_test_pred3 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=1)
table(glass_test_pred3,glass_test_labels)
mean(glass_test_pred3==glass_test_labels) #68.42
CrossTable(x=glass_test_labels,y=glass_test_pred3,prop.chisq = FALSE) 

library(gmodels)

plot(glass_test_pred)
plot(glass_test_pred1)
plot(glass_test_pred2)
plot(glass_test_pred3)


#another method to perfom KNN
library(caret)

glass <- read.csv(file.choose())
glass$Type[glass$Type==1] <- 'Type1'
glass$Type[glass$Type==2] <- 'Type2'
glass$Type[glass$Type==3] <- 'Type3'
glass$Type[glass$Type==4] <- 'Type4'
glass$Type[glass$Type==5] <- 'Type5'
glass$Type[glass$Type==6] <- 'Type6'
glass$Type[glass$Type==7] <- 'Type7'
str(glass)
glass$Type <- as.factor(glass$Type) # Factorize the Type in Glass dataset
View(glass)


set.seed(123)
ind <- sample(2,nrow(glass), replace = T, prob = c(0.7,0.3))
train <- glass[ind==1,]
test <- glass[ind==2,]


# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3) # for repeats=5 accuracy is 66.67
set.seed(222)
fit <- train(Type ~., data = train, method = 'knn', tuneLength = 20,trControl = trcontrol, preProc = c("center","scale"))
fit 

plot(fit)

pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$Type) #68.42
