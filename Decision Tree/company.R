CompanyData <- read.csv(file.choose())
View(CompanyData)
names(CompanyData)
attach(CompanyData)
hist(Sales)
mean(Sales)
High  <-  ifelse(Sales<mean(Sales), "No", "Yes")
CD <- data.frame(CompanyData, High)

CD_train <- CD[1:300,]
CD_test <- CD[301:400,]

# Building model on training data
CD_train_c50 <- C5.0(CD_train[,-12],CD_train$High)
plot(CD_train_c50)
# Training accuracy
pred_train <- predict(CD_train_c50,CD_train)

mean(High==pred_train) # 90.5% Accuracy

library(caret)
confusionMatrix(pred_train,CD_train$High)

predc5.0_test <- predict(CD_train_c50,newdata=CD_test) # predicting on test data
mean(predc5.0_test==High) # 73.75% accuracy 
confusionMatrix(predc5.0_test,CD_test$High)
library(gmodels)
# Cross tablez
CrossTable(CD_test$High,predc5.0_test)


install.packages("party")
library(party)
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(op_tree)

plot(op_tree)

# Building a model on training data 
library(tree)
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)

plot(cd_tree)
text(cd_tree,pretty = 0)

### Evaluate the Model

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)

summary(CD_test$High)

mean(pred_tree$final==CD$High)

CrossTable(CD_test$High,pred_tree$final)

confusionMatrix(CD_test$High,pred_tree$final)

