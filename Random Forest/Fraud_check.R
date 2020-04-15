library(randomForest)
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
summary(FraudCheck)
attach(FraudCheck)

hist(Taxable.Income,xlim = c(0,100000),breaks=c(seq(40,60,80)), col = c("light blue","black"))

mean(Taxable.Income)
Risky_Good  <-  ifelse(Taxable.Income<30000, "No", "Yes")
FD <- data.frame(FraudCheck, Risky_Good)
str(FD)
table(FD$Risky_Good)

set.seed(123)
ind <- sample(2, nrow(FD), replace = TRUE, prob = c(0.7,0.3))
train <- FD[ind==1,]
test  <- FD[ind==2,]
set.seed(213)

#Building a random forest model on training data
fit_forest <- randomForest(Risky_Good~., data=train)
fit_forest
attributes(fit_forest)
names(fit_forest)

#trining Accuracy
mean(Risky_Good==predict(fit_forest,train))

#prediction model for training data
pred <- predict(fit_forest,train)
pred
confusionMatrix(pred,train$Risky_Good)

#prediction model for testing data
pred1 <- predict(fit_forest,test)
pred1
confusionMatrix(pred1,test$Risky_Good)

# Error Rate in Random Forest Model :
plot(fit_forest,lwd=1)
legend("topright", colnames(fit_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
fit_forest1 <- randomForest(Risky_Good~., data=train,ntree=300,mtry=3,importance=T,proximity=T)
fit_forest1
attributes(fit_forest1)


#trining Accuracy
mean(Risky_Good==predict(fit_forest1,train))

# train data prediction using the Tuned RF1 model
pred2 <- predict(fit_forest1,train)
pred2
confusionMatrix(pred2,train$Risky_Good)

# test data prediction using the Tuned RF1 model
pred3 <- predict(fit_forest1,test)
pred3
confusionMatrix(pred3,test$Risky_Good)

hist(treesize(fit_forest1),main = "No of Nodes for the trees", col ="light blue")
# Variable Importance :

varImpPlot(fit_forest1)
varImpPlot(fit_forest1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# Quantitative values 
importance(fit_forest1)

varUsed(fit_forest1)
varUsed(fit_forest)

# Partial Dependence Plot 
partialPlot(fit_forest, train, Price, "Yes")

# Extract single tree from the forest :
getTree(fit_forest1, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(fit_forest1, FD$Risky_Good)
