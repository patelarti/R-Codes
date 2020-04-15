library(randomForest)
CompanyData <- read.csv(file.choose())
View(CompanyData)
summary(CompanyData)
attach(CompanyData)

hist(Sales,main = "Sales",col = c("black","white"),xlim = c(0,20))

mean(Sales)
High  <-  ifelse(Sales<10, "No", "Yes")
CD <- data.frame(CompanyData[2:11], High)
str(CD)
table(CD$High)
 
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)

#Building a random forest model on training data
fit_forest <- randomForest(High~., data=train)
fit_forest
attributes(fit_forest)
names(fit_forest)

#trining Accuracy
mean(High==predict(fit_forest,train))

#prediction model for training data
pred <- predict(fit_forest,train)
pred
confusionMatrix(pred,train$High)

#prediction model for testing data
pred1 <- predict(fit_forest,test)
pred1
confusionMatrix(pred1,test$High)

# Error Rate in Random Forest Model :
plot(fit_forest,lwd=2)
legend("topright", colnames(fit_forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

fit_forest1 <- randomForest(High~., data=train,ntree=300,mtry=3,importance=T,proximity=T)
fit_forest1
attributes(fit_forest1)


#trining Accuracy
mean(High==predict(fit_forest1,train))

# train data prediction using the Tuned RF1 model
pred2 <- predict(fit_forest1,train)
pred2
confusionMatrix(pred2,train$High)

# test data prediction using the Tuned RF1 model
pred3 <- predict(fit_forest1,test)
pred3
confusionMatrix(pred3,test$High)

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
MDSplot(fit_forest1, CD$High)
