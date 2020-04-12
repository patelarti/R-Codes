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

startups_model <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(startups_model)
confint(startups_model,level = 0.95)
predict(startups_model,interval="predict")
plot(startups_model)
startups_model1 <- lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+State)
summary(startups_model1)
confint(startups_model1,level = 0.95)
predict(startups_model1,interval="predict")

startups_model2 <- lm(Profit~RD_Spend+log(Administration))
summary(startups_model2)
confint(startups_model2,level = 0.95)
predict(startups_model2,interval="predict")

library(mvinfluence)
influence.measures(startups_model)
influenceIndexPlot(startups_model,id.n =3 )
influencePlot(startups_model,id.n=3)

## Regression after deleting the 49th and 50th observation, which is influential observation

# Logarthimic Transformation 
startups_model3<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=startups[-c(49,50),])
summary(startups_model3)
confint(startups_model3,level = 0.95)
predict(startups_model3,interval = "predict")

startups_model4<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=startups[-c(49,50),])
summary(startups_model4)
confint(startups_model4,level = 0.95)
predict(startups_model4,interval="predict")

startups_model_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=startups[-c(49,50),])
summary(startups_model_exp)
confint(startups_model_exp,level = 0.95)
predict(startups_model_exp,interval="predict")

startups_model_exp1<-lm(log(Profit)~RD_Spend+Marketing_Spend+State,data=startups[-c(49,50),])
summary(startups_model_exp1)
confint(startups_model_exp1,level = 0.95)
predict(startups_model_exp1,interval="predict")

startups_model_quad<-lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)+Marketing_Spend+I(Marketing_Spend^2)+State+I(State),data=startups[-c(49,50),])
summary(startups_model_quad)
confint(startups_model_quad,level = 0.95)
predict(startups_model_quad,interval = "predict")

startups_model_quad1<-lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2),data=startups[-c(49,50),])
summary(startups_model_quad1)
confint(startups_model_quad1,level = 0.95)
predict(startups_model_quad1,interval = "predict")

startups_model_poly <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)+
                            State+I(State^2)+I(State^3),data=startups[-c(49,50),])
summary(startups_model_poly) 
confint(startups_model_poly,level = 0.95)
predict(startups_model_poly,interval="predict")

startups_model_poly1<- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3),data=startups[-c(49,50),])
summary(startups_model_poly1)
confint(startups_model_poly1,level = 0.95)
predict(startups_model_poly1,interval="predict")
vif(startups_model1)

avPlots(startups_model1,id.n=2,id.cex=3)
