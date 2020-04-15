FraudCheck <- read.csv(file.choose())
View(FraudCheck)
names(FraudCheck)
attach(FraudCheck)

hist(Taxable.Income)
mean(Taxable.Income)
Risky_Good <-  ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC <-  data.frame(FraudCheck,Risky_Good)

FC_train <- FC[1:450,]
FC_test <- FC[451:600,]

# Building model on training data
FC_train_c50 <- C5.0(FC_train[,-7],FC_train$Risky_Good)
plot(FC_train_c50)
# Training accuracy
pred_train <- predict(FC_train_c50,FC_train)

mean(Risky_Good==pred_train) # 91.5% Accuracy

library(caret)
confusionMatrix(pred_train,FC_train$Risky_Good)

predc5.0_test <- predict(FC_train_c50,newdata=FC_test) # predicting on test data
mean(predc5.0_test==Risky_Good) # 76% accuracy 
confusionMatrix(predc5.0_test,FC_test$Risky_Good)
library(gmodels)
# Cross tablez
CrossTable(FC_test$Risky_Good,predc5.0_test)

#Using Party Function 
library(party)
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)

plot(opall_tree)

# using the training Data 

op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 85.3 %
CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df)
