install.packages("kernlab")
library(kernlab)
library(caret)
library(plyr)
FF <- read.csv(file.choose())
View(FF)
class(FF)
str(FF)
names(FF)
attach(FF)
hist(area)
rug(area)
FF1 <- mutate(FF, y = log(area + 1))
attach(FF1)
hist(y)
summary(FF)


# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
FF1$temp = normalize(FF$temp)
FF1$RH   = normalize(FF$RH)
FF1$wind = normalize(FF$wind)
FF1$rain = normalize(FF$rain)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FF), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF1[ind==1,]
FF_test  <- FF1[ind==2,]
# to train model
library(e1071)
# Building model 


model1<-ksvm(size_category~temp+rain+wind+RH,data= FF_train,kernel = "vanilladot")
model1
Area_pred <- predict(model1, FF_test)
mean(Area_pred==FF_train$size_category) #75.20
summary(model1)
table(Area_pred,FF_test$size_category)

agreement <- Area_pred == FF_test$size_category
table(agreement)

prop.table(table(agreement))

# Different types of kernels 

# kernel = rfdot 
model_rfdot<-ksvm(size_category~.,data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
summary(model_rfdot)
mean(pred_rfdot==FF_test$size_category) # 90.41

# kernel = vanilladot
model_vanilla<-ksvm(size_category~.,data= FF_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=FF_test)
summary(model_vanilla)
mean(pred_vanilla==FF_test$size_category) # 97.94


# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH,data= FF_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=FF_test)
summary(model_besseldot)
mean(pred_bessel==FF_test$size_category) # 67.80

# kernel = polydot
model_poly<-ksvm(size_category~.,data= FF_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = FF_test)
summary(model_poly)
mean(pred_poly==FF_test$size_category) # 86.3

# kernel = tanhdot
model_tanh<-ksvm(size_category~.,data= FF_train,kernel = "tanhdot")
pred_tanh<-predict(model_tanh,newdata = FF_test)
summary(model_tanh)
mean(pred_tanh==FF_test$size_category) # 97.94

# kernel = laplacedot
model_laplace<-ksvm(size_category~temp+rain+wind+RH,data= FF_train,kernel = "laplacedot")
pred_laplace<-predict(model_poly,newdata = FF_test)
summary(model_laplace)
mean(pred_laplace==FF_test$size_category) # 94.97

# kernel = splinedot
model_splinedot<-ksvm(size_category~.,data= FF_train,kernel = "splinedot")
pred_splinedot<-predict(model_splinedot,newdata = FF_test)
summary(model_splinedot)
mean(pred_splinedot==FF_test$size_category) # 62.32

# kernel = anovadot
model_anovadot<-ksvm(size_category~.,data= FF_train,kernel = "anovadot")
pred_anovadot<-predict(model_anovadot,newdata = FF_test)
summary(model_anovadot)
mean(pred_anovadot==FF_test$size_category) # 90.41

svm.opt <- svm(size_category ~., data=FF_train, kernel='radial',gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svm.opt, FF_train, decision.values=T))$decision.values
pred1 <- predict(svm.opt,newdata=FF_test)
mean(pred1==FF_test$size_category) # 67.80

svm.fit2 <- svm(y ~., data=FF1, kernel = 'linear', cost=0.1, scale=FALSE)
plot(svm.fit2, FF1)