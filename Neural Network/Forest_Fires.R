#PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

forestfires <- read.csv(file.choose())
library(caret)
str(forestfires)
View(forestfires)

library(plyr)
forestfires$size_category <- as.numeric(revalue(forestfires$size_category,c("small"="0", "large"="1")))
forestfires$month <- as.numeric(revalue(forestfires$month,c('jan'='0','feb'='1','mar'='2','apr'='3','may'='4','jun'='5','jul'='6','aug'='7','sep'='8','oct'='9','nov'='10','dec'='11')))
forestfires$day <- as.numeric(revalue(forestfires$day,c('mon'='0','tue'='1','wed'='2','thu'='3','fri'='4','sat'='5','sun'='6')))
forestfires <- as.data.frame(forestfires)
attach(forestfires)
names(forestfires)
sd(month)
sd(day)
sd(FFMC)
sd(DMC)
sd(DC)
sd(ISI)
sd(temp)
sd(RH)
sd(wind)
sd(rain)
sd(area)

var(month)
var(day)
var(FFMC)
var(DMC)
var(DC)
var(ISI)
var(temp)
var(RH)
var(wind)
var(rain)
var(area)

library(moments)
skewness(month)
skewness(day)
skewness(FFMC)
skewness(DMC)
skewness(DC)
skewness(ISI)
skewness(temp)
skewness(RH)
skewness(wind)
skewness(rain)
skewness(area)

kurtosis(month)
kurtosis(day)
kurtosis(FFMC)
kurtosis(DMC)
kurtosis(DC)
kurtosis(ISI)
kurtosis(temp)
kurtosis(RH)
kurtosis(wind)
kurtosis(rain)
kurtosis(area)

qqnorm(month)
qqnorm(day)
qqnorm(FFMC)
qqnorm(DMC)
qqnorm(DC)
qqnorm(ISI)
qqnorm(temp)
qqnorm(RH)
qqnorm(wind)
qqnorm(rain)
qqnorm(area)

hist(month)
hist(day)
hist(FFMC)
hist(DMC)
hist(DC)
hist(ISI)
hist(temp)
hist(RH)
hist(wind)
hist(rain)
hist(area)

boxplot(month,horizontal = T)
boxplot(day,horizontal = T)
boxplot(FFMC,horizontal = T)
boxplot(DMC,horizontal = T)
boxplot(DC,horizontal = T)
boxplot(ISI,horizontal = T)
boxplot(temp,horizontal = T)
boxplot(RH,horizontal = T)
boxplot(wind,horizontal = T)
boxplot(rain,horizontal = T)
boxplot(area,horizontal = T)

pairs(forestfires)
cor(forestfires)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires,FUN=normalize))
summary(forestfires_norm)
summary(forestfires$area)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfires_norm), replace = TRUE, prob = c(0.7,0.3))
forestfires_train <- forestfires_norm[ind==1,]
forestfires_test  <- forestfires_norm[ind==2,]
# Creating a neural network model on training data
library(neuralnet)

forestfires_model <- neuralnet(formula = area ~ temp+DMC+wind
                               +ISI+RH+DC,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model, rep = "best")
summary(forestfires_model)

set.seed(12323)
model_results <- compute(forestfires_model,forestfires_test[1:12])
predicted_area <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_area,forestfires_test$area)
str_max <- max(forestfires$area)
str_min <- min(forestfires$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

set.seed(12345)
forestfires_model2 <- neuralnet(area~temp+DMC+wind+ISI+RH+DC,data = forestfires_train,hidden = 2)
plot(forestfires_model2 ,rep = "best")
summary(forestfires_model2)
model_results2<-compute(forestfires_model2,forestfires_test[1:12])
predicted_area2<-model_results2$net.result
cor(predicted_area2,forestfires_test$area)
plot(predicted_area2,forestfires_test$area)
par(mar = numeric(4), family = 'mono')
library(NeuralNetTools)
plotnet(forestfires_model2, alpha = 0.6)

set.seed(12345)
forestfires_model3 <- neuralnet(area~temp+DMC+wind+ISI+RH+DC,data = forestfires_train,hidden = 4)
plot(forestfires_model3 ,rep = "best")
summary(forestfires_model3)
model_results3<-compute(forestfires_model3,forestfires_test[1:12])
predicted_area3<-model_results3$net.result
cor(predicted_area3,forestfires_test$area)
plot(predicted_area3,forestfires_test$area)
par(mar = numeric(4), family = 'mono')
plotnet(forestfires_model3, alpha = 0.6)

set.seed(12345)
forestfires_model4 <- neuralnet(area~temp+DMC+wind+ISI+RH+DC,data = forestfires_train,hidden = 5)
plot(forestfires_model4 ,rep = "best")
summary(forestfires_model4)
model_results4<-compute(forestfires_model4,forestfires_test[1:12])
predicted_area4<-model_results4$net.result
cor(predicted_area4,forestfires_test$area)
plot(predicted_area4,forestfires_test$area)
par(mar = numeric(4), family = 'mono')
plotnet(forestfires_model4, alpha = 0.6)
