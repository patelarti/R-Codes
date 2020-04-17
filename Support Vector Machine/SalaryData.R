library(kernlab)
library(caret)
train_salary <- read.csv(file.choose())
test_salary <- read.csv(file.choose())
str(train_salary)
str(test_salary)
View(train_salary)
View(test_salary)
names(train_salary)
names(test_salary)
train_salary$educationno <- as.factor(train_salary$educationno)
test_salary$educationno <- as.factor(test_salary$educationno)
class(train_salary)
class(test_salary)
summary(train_salary)
summary(test_salary)
#Visualization 
# Plot and ggplot 
ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$age, fill = train_salary$Salary))+geom_boxplot()

plot(train_salary$workclass,train_salary$Salary)
plot(train_salary$educationno,train_salary$Salary)
plot(train_salary$maritalstatus,train_salary$Salary)
plot(train_salary$occupation,train_salary$Salary)
plot(train_salary$relationship,train_salary$Salary)
plot(train_salary$race,train_salary$Salary)
plot(train_salary$sex,train_salary$Salary)
ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$capitalgain, fill = train_salary$Salary)) +geom_boxplot()
ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$capitalloss, fill = train_salary$Salary)) +geom_boxplot()
ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$hoursperweek, fill = train_salary$Salary)) +geom_boxplot()
plot(train_salary$native,train_salary$Salary)

#Density Plot 

ggplot(data=train_salary,aes(x = train_salary$age, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$workclass, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$education, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$educationno, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$maritalstatus, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$occupation, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$sex, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$relationship, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$race, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$capitalgain, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$capitalloss, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$hoursperweek, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train_salary,aes(x = train_salary$native, fill = train_salary$Salary)) +geom_density(alpha = 0.9, color = 'Violet')

# Building model 


model<-ksvm(train_salary$Salary~., data= train_salary, kernel = "vanilladot")
model
Salary_prediction <- predict(model, test_salary)
table(Salary_prediction,test_salary$Salary)
agreement <- Salary_prediction == test_salary$Salary
table(agreement)
prop.table(table(agreement))

model_rfdot<-ksvm(train_salary$Salary~., data= train_salary,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_salary)
mean(pred_rfdot==test_salary$Salary) #85.20

model_vanilla<-ksvm(train_salary$Salary~., data= train_salary,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_salary)
mean(pred_vanilla==test_salary$Salary) # 84.64

model_tanhdot<-ksvm(train_salary$Salary~., data= train_salary,kernel = "tanhdot")
pred_tanhdot<-predict(model_tanhdot,newdata=test_salary)
mean(pred_tanhdot==test_salary$Salary) # 63.87

model_polydot<-ksvm(train_salary$Salary~., data= train_salary,kernel = "polydot")
pred_polydot<-predict(model_polydot,newdata=test_salary)
mean(pred_polydot==test_salary$Salary) # 84.61

model_laplacedot<-ksvm(train_salary$Salary~., data= train_salary,kernel = "laplacedot")
pred_laplacedot<-predict(model_laplacedot,newdata=test_salary)
mean(pred_laplacedot==test_salary$Salary) # 85.09

model_splinedot<-ksvm(train_salary$Salary~., data= train_salary,kernel = "splinedot")
pred_splinedot<-predict(model_splinedot,newdata=test_salary)
mean(pred_splinedot==test_salary$Salary) # 74.62
