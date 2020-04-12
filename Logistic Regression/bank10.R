#The classification goal is to predict if the client will subscribe a term deposit (variable y).
library(readxl)
bank <- read.csv(file.choose())
View(bank)
colnames(bank)
class(bank)
summary(bank)
str(bank)
dim(bank)
bank <- bank[,-9]
bank <- bank[,-15]
library(plyr)
bank$default <- ifelse(bank$default=="yes",1,0)
bank$housing <- ifelse(bank$housing=="yes",1,0)
bank$loan <- ifelse(bank$loan=="yes",1,0)
View(bank)
qqnorm(bank$age)
qqnorm(bank$default)
qqnorm(bank$balance)
qqnorm(bank$housing)
qqnorm(bank$loan)
qqnorm(bank$duration)
qqnorm(bank$campaign)
qqnorm(bank$pdays)
qqnorm(bank$previous)

hist(bank$age)
hist(bank$default)
hist(bank$balance)
hist(bank$housing)
hist(bank$loan)
hist(bank$duration)
hist(bank$campaign)
hist(bank$pdays)
hist(bank$previous)

library(moments)
skewness(bank$age)
skewness(bank$default)
skewness(bank$balance)
skewness(bank$housing)
skewness(bank$loan)
skewness(bank$duration)
skewness(bank$campaign)
skewness(bank$pdays)
skewness(bank$previous)


fit1 <- glm(loan~housing+default+campaign+pdays+previous+y+balance+duration,data = bank,family = "binomial")
summary(fit1)
prob1 <- predict(fit1,type = "response")
logit1 <- glm(loan~factor(housing)+factor(default)+factor(campaign)+factor(pdays)+balance+duration,family= binomial,data=bank)
summary(logit1)
exp(coef(logit1))
library(MASS)
library(car)

stepAIC(fit1)

prob <- predict(logit1,type = c("response"),bank)                          
prob
final_y <- cbind(bank, prob)
confusion_y <- table(prob>0.5, bank$y)
table(prob>0.5)
confusion_y
Accuracy1 <- sum(diag(confusion_y)/sum(confusion_y))
Accuracy1 

threshold=0.5
predicted_values<-ifelse(predict(fit1,type="response")>threshold,1,0)
actual_values<-fit1$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

library(caret)
specificity(conf_matrix)
sensitivity(conf_matrix)
Accuracy <- sum(diag(conf_matrix)/sum(conf_matrix))
Accuracy
#changing threshold
threshold=0.8
predicted_values<-ifelse(predict(fit1,type="response")>threshold,1,0)
actual_values<-fit1$y
conf_matrix1<-table(predicted_values,actual_values)
conf_matrix1

Accuracy2<- sum(diag(conf_matrix1)/sum(conf_matrix1))
Accuracy2
##Roc curve
library(ROCR)
rocpred <- prediction(prob1,bank$loan)
rocpref <- performance(rocpred,"tpr","fpr")
plot(rocpref,colorize=T,text.adj=c(-0.2,1.7))
