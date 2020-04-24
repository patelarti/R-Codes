#Build a Neural Network model for 50_startups data to predict profit 

startups <- read.csv(file.choose())
summary(startups)
attach(startups)
View(startups)
class(startups)
names(startups)

sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)
var(startups)
library(moments)
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)
kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)

qqnorm(R.D.Spend)
qqnorm(Administration)
qqnorm(Marketing.Spend)
qqnorm(Profit)
hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Profit)
boxplot(startups)
barplot(R.D.Spend,Profit)
barplot(Administration,Profit)
barplot(Marketing.Spend,Profit)
stem(Administration)

library(plyr)
State <- revalue(State,c("New York"="0", "California"="1", "Florida"="2"))
startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
startups <- as.data.frame(startups)
attach(startups)
View(startups)
plot(RD_Spend,Profit)
plot(Administration,Profit)
plot(Marketing_Spend,Profit)
plot(State,Profit)
pairs(startups) #Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
cor(startups)

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(startups,FUN=normalize))
summary(Startups_norm$Profit)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]


# Creating a neural network model on training data
library(neuralnet)
library(NeuralNetTools)

startups_model <- neuralnet(Profit~RD_Spend+Administration+Marketing_Spend+State,data = Startups_train)
str(startups_model)
plot(startups_model, rep = "best")
summary(startups_model)
par(mar = numeric(4), family = 'mono')
plotnet(startups_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

str_max <- max(startups$Profit)
str_min <- min(startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

set.seed(12345)
Startups_model2 <- neuralnet(Profit~RD_Spend+Administration+Marketing_Spend+State,data = Startups_train,hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)

model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)
par(mar = numeric(4), family = 'mono')
plotnet(Startups_model2, alpha = 0.6)

set.seed(12345)
Startups_model3 <- neuralnet(Profit~RD_Spend+Administration+Marketing_Spend+State,data = Startups_train,hidden = 4)
plot(Startups_model3 ,rep = "best")
summary(Startups_model2)

model_results3<-compute(Startups_model3,startups_test[1:4])
predicted_Profit3<-model_results3$net.result
cor(predicted_Profit3,startups_test$Profit)
plot(predicted_Profit3,startups_test$Profit)
par(mar = numeric(4), family = 'mono')
plotnet(Startups_model3, alpha = 0.6)
