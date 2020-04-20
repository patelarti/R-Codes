#####1)Forecast the CocaCola prices data set. Prepare a document for each model explaining 
#####how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.

library(readxl)
Cocacola <- read_excel(file.choose()) 
View(Cocacola)
attach(Cocacola)
class(Cocacola)
summary(Cocacola)
plot(Cocacola$Sales,type="o")

Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

CocacolaData["n"]<- 1:42
View(CocacolaData)
#CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
#CocacolaData["n_square"]<-CocacolaData["n"]*CocacolaData["n"]
attach(CocacolaData)

train<-CocacolaData[1:36,]

test<-CocacolaData[37:40,]

# Linear Model 

linear_model<-lm(Sales~n,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 

# Exponential 

expo_model<-lm(log(Sales)~n,data=train)
summary(expo_model)#Adjusted R-squared:  0.8017 

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo #524.7351

# Quadratic 

Quad_model<-lm(Sales~n+I(n^2),data=train)
summary(Quad_model)#Adjusted R2 - 85.96 %
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 434.7185 

# Additive Seasonality 

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.03346
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1785.135

# Additive Seasonality with Linear

Add_sea_Linear_model<-lm(Sales~n+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.8761 
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 534.6979 

#Additive Seasonality with Quadratic

Add_sea_Quad_model<-lm(Sales~n+I(n^2)+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)#Adjusted R-squared:  0.9549
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 236.7075 

# Multiplicative Seasonality

multi_sea_model<-lm(log(Sales)~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.05006
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1871.203

# Multiplicative Seasonality Linear trend

multi_add_sea_model<-lm(log(Sales)~n+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.8986
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 335.1026 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~n+I(n^2)+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values

View(new_model_fin)

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",type="o")
View(Final)

##----Cocacola Sales Prediction using Auto Arima Forecast---##
# Using Arima Model - 
Cocacola1<-read_excel(file.choose()) # read the Cocacola data
Cocacola1 <- Cocacola1$Sales
Cocacola1 <- as.ts(Cocacola1)
View(Cocacola1)
class(Cocacola1)

Cocacola2 <- ts(Cocacola1,start=c(1986,1),end=c(1995,6),frequency=4)

start(Cocacola2)

end(Cocacola2)
class(Cocacola2)
sum(is.na(Cocacola2))
summary(Cocacola2)
View(Cocacola2)

decomdata<- decompose(Cocacola2, "multiplicative")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Cocacola2)
abline(reg=lm(Cocacola2~time(Cocacola2)))
cycle(Cocacola2)
# Boxplot by Cycle
boxplot(Cocacola2~cycle(Cocacola1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))
install.packages("forecast")
library(forecast)
library(tseries)
# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Cocacola2)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.

auto.arima(Cocacola2, ic = "aic", trace = TRUE)
plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
plot(Pass_Forecast)

# Test your final model

Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")
#--------------------------------------------------------------------

####2)Airlines Passengers data set. Prepare a document for each model explaining 
####how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.

Airlines<-read_excel(file.choose()) 
View(Airlines)
plot(Airlines$Passengers,type="o")
# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)
AirlinesData["t"]<- 1:96
View(AirlinesData)
attach(AirlinesData)

train<-AirlinesData[1:84,]
test<-AirlinesData[85:96,]

# Linear Mode
linear_model<-lm(Passengers~t,data=train)
summary(linear_model) #Adjusted R-squared:  0.7898 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924

# Exponential 
expo_model<-lm(log(Passengers)~t,data=train)
summary(expo_model)#Adjusted R-squared:  0.8218 
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736  

# Quadratic 
Quad_model<-lm(Passengers~t+I(t^2),data=train)
summary(Quad_model)#Adjusted R-squared:  0.7912
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189 

# Additive Seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.04015
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

# Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.9475
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896 

# Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Passengers~t+I(t^2)+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model) #Adjusted R-squared:  0.9524 
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082 

#Multiplicative Seasonality 
multi_sea_model<-lm(log(Passengers)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.02568
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

# Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log(Passengers)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.9723
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917 

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
new_model<-lm(log(Passengers)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log(Passengers),order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

###Airline Passengers using Auto Arima Forecast

Airlines1<-read_excel(file.choose()) # read the Airlines data
Airlines1 <- Airlines1$Passengers
Airlines1 <- as.ts(Airlines)
View(Airlines1)
class(Airlines1)

Airlines2 <- ts(Airlines1,start=c(1995,1),end=c(2002,12),frequency=12)

start(Airlines2)
end(Airlines2)
class(Airlines2)
sum(is.na(Airlines2))
summary(Airlines2)
View(Airlines2)

decomdata<- decompose(Airlines2, "multiplicative") ##decomdata<- decompose(Airlines2, "additive")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Airlines2)
abline(reg=lm(Airlines2~time(Airlines2)))
cycle(Airlines2)
# Boxplot by Cycle
boxplot(Airlines2~cycle(Airlines1,xlab = "Date", ylab = "Passenger Number(100's)",main = "Monthly Boxplot of passengers from 1995 to 2002"))
library(forecast)
# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Airlines2)
Newmodel
auto.arima(Airlines2, ic = "aic", trace = TRUE)
# tseries evaluation

plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
pacf(ts(Newmodel$residuals),main = 'PACF Residual')
# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
plot(Pass_Forecast)
# Test your final model

Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")

###plastic sales data set. Prepare a document for each model explaining 
###how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.
library(readxl)

Plastics<-read.csv(file.choose())
View(Plastics)  
plot(Plastics$Sales,type="o")

# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticsdata<-cbind(Plastics,X)
View(Plastics)
colnames(Plastics)
Plasticsdata["t"]<- 1:60

attach(Plasticsdata)

train<-Plasticsdata[1:48,]

test<-Plasticsdata[49:60,]

# Linear Model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)#Adjusted R-squared:  0.3159 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 260.9378 

# Exponential
expo_model<-lm(log(Sales)~t,data=train)
summary(expo_model)#Adjusted R-squared:  0.3025
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.6938  

# Quadratic
Quad_model<-lm(Sales~t+I(t^2),data=train)
summary(Quad_model)#Adjusted R-squared:  0.3048
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297.4067 

# Additive Seasonality 
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.6985
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.6027 

#Additive Seasonality with Linear 
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.9645 
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536 

# Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+I(t^2)+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)#Adjusted R-squared:  0.9768
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 218.1939 

# Multiplicative Seasonality
multi_sea_model<-lm(log(Sales)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.728
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.6543

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.9751 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 160.6833 

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
new_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticsdata)
new_model_pred<-data.frame(predict(new_model,newdata=Plasticsdata,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

Month <- as.data.frame(Plasticsdata$Month)

Final <- as.data.frame(cbind(Month,Plasticsdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",type="s")
View(Final)

####Plastics Sales Prediction using Auto Arima Forecast

Plastics<-read.csv(file.choose()) # read the Plastics data
Plastics <- Plastics$Sales
Plastics <- as.ts(Plastics)
View(Plastics)
class(Plastics)
library(tseries)
Plastics1 <- ts(Plastics,start=c(1986,1),end=c(1995,6),frequency=4)
start(Plastics1)
end(Plastics1)
class(Plastics1)
sum(is.na(Plastics1))
summary(Plastics1)
View(Plastics1)

decomdata<- decompose(Plastics1, "multiplicative")# decomdata<- decompose(Plastics1, "additive")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Plastics1)
abline(reg=lm(Plastics1~time(Plastics1)))
cycle(Plastics1)

# Boxplot by Cycle
boxplot(Plastics1~cycle(Plastics1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))
library(tseries)
library(forecast)

# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Plastics1)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.
auto.arima(Plastics1, ic = "aic", trace = TRUE)

# tseries evaluation
plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
pacf(ts(Newmodel$residuals),main = 'PACF Residual')
# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
plot(Pass_Forecast)

# Test your final model
Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")

