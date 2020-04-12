#Classify whether application accepted or not using Logistic regression
CreditCard <- read.csv(file.choose())
View(CreditCard)
colnames(CreditCard)
class(CreditCard)
summary(CreditCard)
str(CreditCard)
dim(CreditCard)
library(plyr)

CreditCard$card <- ifelse(CreditCard$card=="yes",1,0)
CreditCard$owner <- ifelse(CreditCard$owner=="yes",1,0)
CreditCard$selfemp <- ifelse(CreditCard$selfemp=="yes",1,0)

CreditCard <- CreditCard[,-1]
View(CreditCard)
var(CreditCard)

boxplot(CreditCard$card,horizontal = T)
boxplot(CreditCard$age,horizontal = T)
boxplot(CreditCard$income,horizontal = T)
boxplot(CreditCard$share,horizontal = T)
boxplot(CreditCard$expenditure,horizontal = T)
boxplot(CreditCard$owner,horizontal = T)
boxplot(CreditCard$dependents,horizontal = T)
boxplot(CreditCard$months,horizontal = T)
boxplot(CreditCard$active,horizontal = T)

hist(CreditCard$card)
hist(CreditCard$age)
hist(CreditCard$income)
hist(CreditCard$share)
hist(CreditCard$expenditure)
hist(CreditCard$owner)
hist(CreditCard$dependents)
hist(CreditCard$months)
hist(CreditCard$active)
attach(CreditCard)

library(moments)
skewness(card)
skewness(age)
skewness(income)
skewness(share)
skewness(expenditure)
skewness(owner)
skewness(dependents)
skewness(months)
skewness(active)


CreditCard_model <- glm(card~.,data = CreditCard,family = "binomial")
summary(CreditCard_model)
prob <- predict(CreditCard_model,type="response")

final <- cbind(CreditCard, prob)
confusion_matrix <- table(prob>0.5, CreditCard$card)
table(prob>0.5)
confusion_matrix
Accuracy1 <- sum(diag(confusion_matrix)/sum(confusion_matrix))
Accuracy1 
View(CreditCard)

model2 <- glm(card~factor(reports)+factor(income)+factor(share)+factor(expenditure)+owner+age+selfemp+dependents+months+majorcards+active,data = CreditCard,family = "binomial")
summary(model2)
exp(coef(model2))
prob1 <- predict(model2,type = "response")
final1 <- cbind(CreditCard,prob1)
confusion_matrix1 <- table(prob1>0.5,CreditCard$card)
table(prob1>0.5)
confusion_matrix1
Accuracy2 <- sum(diag(confusion_matrix1)/sum(confusion_matrix1))
Accuracy2

threshold=0.5
predicted_values<-ifelse(predict(model2,type="response")>threshold,1,0)
actual_values<-CreditCard$card
conf_matrix<-table(predicted_values,actual_values)
conf_matrix
library(caret)
specificity(conf_matrix)
sensitivity(conf_matrix)

predicted_values1<-ifelse(predict(CreditCard_model,type="response")>threshold,1,0)
actual_values1<-CreditCard$card
conf_matrix1<-table(predicted_values1,actual_values1)
conf_matrix1
library(caret)
specificity(conf_matrix1)
sensitivity(conf_matrix1)

##Roc curve
library(ROCR)
rocpred <- prediction(prob1,CreditCard$card)
rocpref <- performance(rocpred,"tpr","fpr")
plot(rocpref,colorize=T,text.adj=c(-0.2,1.7))
