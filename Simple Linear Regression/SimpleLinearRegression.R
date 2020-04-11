## 1) Calories_consumed-> predict weight gained using calories consumed ##
Calories_consumed <- read.csv(file.choose())
View(Calories_consumed)
attach(Calories_consumed)
plot(Calories_consumed)
summary(Calories_consumed)
cor(Weight.gained..grams.,Calories.Consumed)

reg <- lm(Weight.gained..grams.~Calories.Consumed,data = Calories_consumed)
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
plot(reg)

reg_log <- lm(Weight.gained..grams.~log(Calories.Consumed),data = Calories_consumed)
summary(reg_log)
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")
plot(reg_log)

reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed),data = Calories_consumed)
summary(reg_sqrt)
confint(reg_sqrt,level = 0.95)
predict(reg_sqrt,interval = "predict")

reg_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed,data = Calories_consumed)
summary(reg_exp)
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")

a <- sqrt(log(Calories.Consumed))
reg_1 <- lm(log(Weight.gained..grams.)~a,data = Calories_consumed)
summary(reg_1)
confint(reg_1,level = 0.95)
predict(reg_1,interval = "predict")


##2) Delivery_time -> Predict delivery time using sorting time ##


Delivery_time <- read.csv(file.choose())
names(Delivery_time)
attach(Delivery_time)
summary(Delivery_time)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)
colnames(Delivery_time) <- c("DeliveryTime","SortingTime")

model1 <- lm(DeliveryTime~SortingTime,data = Delivery_time)
summary(model1)
confint(model1,level = 0.95)
predict(model1,interval = "predict")
plot(model1)

model2 <- lm(log(DeliveryTime)~log(SortingTime),data = Delivery_time)
summary(model2)
confint(model2,level = 0.95)
predict(model2,interval = "predict")
plot(model2)

influenceIndexPlot(model1)
model3 <- lm(DeliveryTime ~ SortingTime, data = Delivery_time[c(-5,-9,-21),])
summary(model3)
plot(model3)


##3) Emp_data -> Build a prediction model for Churn_out_rate ##

Emp_data <- read.csv(file.choose())
summary(Emp_data)
attach(Emp_data)

Churn_out_rate_model1 <- lm(Churn_out_rate~Salary_hike,data = Emp_data)
summary(Churn_out_rate_model1)
confint(Churn_out_rate_model1,level = 0.95)
predict(Churn_out_rate_model1,interval = "predict")
plot(Churn_out_rate_model1)

Churn_out_rate_model2 <- lm(Churn_out_rate~log(Salary_hike),data = Emp_data)
summary(Churn_out_rate_model2)
confint(Churn_out_rate_model2,level = 0.95)
predict(Churn_out_rate_model2,interval = "predict")
plot(Churn_out_rate_model2)

Churn_out_rate_model3 <- lm(log(Churn_out_rate)~log(Salary_hike),data = Emp_data)
summary(Churn_out_rate_model3)
confint(Churn_out_rate_model3,level = 0.95)
predict(Churn_out_rate_model3,interval = "predict")
plot(Churn_out_rate_model3)


## 4) Salary_hike -> Build a prediction model for Salary_hike ##


Salary_hike <- read.csv(file.choose())
summary(Salary_hike)
names(Salary_hike)
attach(Salary_hike)

Salary_hike_model <- lm(Salary~YearsExperience,data = Salary_hike)
summary(Salary_hike_model)
confint(Salary_hike_model,level = 0.95)
predict(Salary_hike_model,interval = "predict")
plot(Salary_hike_model)

Salary_hike_model1 <- lm(Salary~log(YearsExperience),data = Salary_hike)
summary(Salary_hike_model1)
confint(Salary_hike_model1,level = 0.95)
predict(Salary_hike_model1,interval = "predict")
plot(Salary_hike_model1)

Salary_hike_model2 <- lm(log(Salary)~YearsExperience,data = Salary_hike)
summary(Salary_hike_model2)
confint(Salary_hike_model2,level = 0.95)
predict(Salary_hike_model2,interval = "predict")
plot(Salary_hike_model2)
