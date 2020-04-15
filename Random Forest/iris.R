install.packages("randomForest")
library(randomForest)
data(iris)
View(iris)

# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)


# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Tune Random Forest Model mtry 
tune <- tuneRF(iris_train[,-5], iris_train[,5], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

fit_forest1 <- randomForest(Species~., data=iris_train, ntree = 140, mtry = 2, importance = TRUE,
                            proximity = TRUE)
fit_forest1
attributes(fit_forest1)

# train data prediction using the Tuned RF1 model
pred2 <- predict(fit_forest1,iris_train)
pred2
confusionMatrix(pred2,iris_train$Species)

# test data prediction using the Tuned RF1 model
pred3 <- predict(fit_forest1,iris_test)
pred3
confusionMatrix(pred3,iris_test$Species)

hist(treesize(fit_forest1),main = "No of Nodes for the trees", col ="light blue")
# Variable Importance :

varImpPlot(fit_forest1)
varImpPlot(fit_forest1 ,Sort = T, n.var = 4, main = "Top 4 -Variable Importance")

# Quantitative values 
importance(fit_forest1)

varUsed(fit_forest1)
varUsed(fit.forest)

# Partial Dependence Plot 
partialPlot(fit_forest1, iris_train, Petal.Length, "versicolor")
partialPlot(fit_forest1, iris_train, Petal.Length, "setosa")
partialPlot(fit_forest1, iris_train, Petal.Length, "virginica")

# Extract single tree from the forest :
getTree(fit_forest1, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(fit_forest1, iris$Species)


