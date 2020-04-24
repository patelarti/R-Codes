#Prepare a model for strength of concrete data using Neural Networks
concrete <- read.csv(file.choose())
str(concrete)
class(concrete)
names(concrete)
attach(concrete)
summary(concrete) 

library(neuralnet)
library(nnet)
install.packages("NeuralNetTools")
library(NeuralNetTools)

hist(cement, prob = T, breaks = 30)
lines(density(cement))
summary(cement)

hist(slag, prob = T, breaks = 30)
lines(density(slag))
summary(slag)

hist(ash, prob = T, breaks = 30)
lines(density(ash))
summary(ash)

hist(water, prob = T, breaks = 30)
lines(density(water))
summary(water)

hist(superplastic, prob = T, breaks = 30)
lines(density(superplastic))
summary(superplastic)

hist(coarseagg, prob = T, breaks = 30)
lines(density(coarseagg))
summary(coarseagg)

hist(fineagg, prob = T, breaks = 30)
lines(density(fineagg))
summary(fineagg)

hist(strength, prob = T, breaks = 30)
lines(density(strength))
summary(strength)

names(concrete)
sd(cement)
sd(slag)
sd(ash)
sd(water)
sd(superplastic)
sd(coarseagg)
sd(fineagg)
sd(age)
sd(strength)

var(cement)
var(slag)
var(ash)
var(water)
var(superplastic)
var(coarseagg)
var(fineagg)
var(age)
var(strength)

library(moments)
skewness(cement)
skewness(slag)
skewness(ash)
skewness(water)
skewness(superplastic)
skewness(coarseagg)
skewness(fineagg)
skewness(age)
skewness(strength)

kurtosis(cement)
kurtosis(slag)
kurtosis(ash)
kurtosis(water)
kurtosis(superplastic)
kurtosis(coarseagg)
kurtosis(fineagg)
kurtosis(age)
kurtosis(strength)

# Apply Normalization 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength)

summary(strength) 

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_norm[ind==1,]
concrete_test  <- concrete_norm[ind==2,]


# Creating a neural network model on training data

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)

plot(concrete_model, rep = "best")
summary(concrete_model)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength,concrete_test$strength)

str_max <- max(concrete$strength)
str_min <- min(concrete$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(ActualStrength_pred)

# Improve the model performance :
set.seed(12345)
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = 5)
plot(concrete_model2, rep = "best")
summary(concrete_model2)

model_results2<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)

plot(predicted_strength,concrete_test$strength)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)


concrete_model3 <- neuralnet(strength~.,data= concrete_train,hidden = 7)
plot(concrete_model3, rep = "best")
summary(concrete_model3)

model_results3<-compute(concrete_model3,concrete_test[1:8])
predicted_strength3<-model_results3$net.result
cor(predicted_strength3,concrete_test$strength)

plot(predicted_strength,concrete_test$strength)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model3, alpha = 0.6)
