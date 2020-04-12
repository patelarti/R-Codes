computer <-read.csv(file.choose())
attach(computer)
View(computer)
summary(computer)
library(plyr)

computer$cd <- as.numeric(revalue(computer$cd,c("yes"=1, "no"=0)))
computer$multi <- as.numeric(revalue(computer$multi,c("yes"=1, "no"=0)))
computer$premium <- as.numeric(revalue(computer$premium,c("yes"=1, "no"=0)))
View(computer)
var(computer$cd)
qqnorm(price)
qqline(price)
qqnorm(speed)
qqline(speed)
qqnorm(hd)
qqline(hd)
qqnorm(ram)
qqline(ram)
qqnorm(screen)
qqnorm(ads)

boxplot(price,horizontal = TRUE)
boxplot(speed)
boxplot(hd)
boxplot(ram)
boxplot(screen)
boxplot(ads)
hist(price)
hist(speed)
hist(hd)
hist(ram)
hist(screen)
hist(ads)
hist(multi)

boxplot(price)$out
boxplot(price,plot = FALSE)$out
outliers<-boxplot(price,plot = FALSE)$out 
print(outliers)
computer[which(price %in% outliers),]
computer <-computer[-which(price %in% outliers),]
boxplot(price)


boxplot(hd)$out
boxplot(hd,plot = FALSE)$out
outliers<-boxplot(hd,plot = FALSE)$out 
print(outliers)
computer[which(hd %in% outliers),]
computer <-computer[-which(hd %in% outliers),]

boxplot(ram)$out
boxplot(ram,plot = FALSE)$out
outliers<-boxplot(ram,plot = FALSE)$out 
print(outliers)
computer[which(ram %in% outliers),]
computer <-computer[-which(ram %in% outliers),]

pairs(computer)
plot(price)
plot(speed)
plot(hd)
plot(ram)
plot(screen)
plot(cd)
plot(multi)
plot(premium)
plot(ads)
plot(trend)

cor(computer)
library(corpcor)
cor2pcor(cor(computer))
computer_model<- lm(price~speed+hd+ram+screen+trend+multi+ads+premium+cd)
summary(computer_model)
confint(computer_model,level = 0.95)
predict(computer_model,interval="predict")

library(mvinfluence)
library(car)
influence.measures(computer_model)
influenceIndexPlot(computer_model, id.n=3)
influencePlot(computer_model,id.n=3)

computer_model1<- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+log(trend)+log(multi)+log(ads)+log(premium)+log(cd),data = computer[-c(1441,1701),])
summary(computer_model1)
confint(computer_model1,level = 0.95)
predict(computer_model1,interval="predict")
plot(computer_model1)

computer_model2<- lm(price~log(speed)+log(hd)+log(ram)+screen+trend+multi+ads+premium+cd,data = computer[-c(1441,1701),])
summary(computer_model2)
confint(computer_model2,level = 0.95)
predict(computer_model2,interval="predict")

computer_model_exp<- lm(log(price)~speed+hd+ram+screen+trend+multi+ads+premium+cd)
summary(computer_model_exp)
confint(computer_model_exp,level = 0.95)
predict(computer_model_exp,interval="predict")

computer_model_quad<- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+ads+I(ads^2)+trend+I(trend))
summary(computer_model_quad)
confint(computer_model_quad,level = 0.95)
predict(computer_model_quad,interval="predict")

avPlots(computer_model1)
influencePlot(computer_model2)
vif(computer_model)
vif(computer_model2)
