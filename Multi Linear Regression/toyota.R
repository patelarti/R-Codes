toyota <- read.csv(file.choose())
View(toyota)
toyota<-toyota[,c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(toyota)
attach(toyota)
qqnorm(Price)
qqnorm(Age_08_04)
qqnorm(KM)
qqnorm(HP)
qqnorm(cc)
qqnorm(Doors)
qqnorm(Gears)
qqnorm(Quarterly_Tax)
qqnorm(Weight)
summary(toyota)
boxplot(Price)
boxplot(Age_08_04)
boxplot(KM)
boxplot(HP)
boxplot(cc)
boxplot(Doors)
boxplot(Gears)
boxplot(Quarterly_Tax)
boxplot(Weight)
hist(Price)
hist(Age_08_04)
hist(KM)
hist(HP)
hist(cc)
hist(Doors)
hist(Gears)
hist(Quarterly_Tax)
hist(Weight)
library(e1071)
skewness(Price)
skewness(Age_08_04)
skewness(KM)
skewness(HP)
skewness(cc)
skewness(Doors)
skewness(Gears)
skewness(Quarterly_Tax)
skewness(Weight)
skewness(Weight)

boxplot(Price)$out
boxplot(Price,plot = FALSE)$out
outliers <- boxplot(Price,plot = FALSE)$out
print(outliers)
toyota[which(toyota$Price %in% outliers),]
toyota <-toyota[-which(Price %in% outliers),]

boxplot(KM)$out
boxplot(KM,plot = FALSE)$out
outlier1 <- boxplot(KM,plot = FALSE)$out
print(outlier1)
toyota[which(KM %in% outliers),]
toyota <-toyota[-which(KM %in% outliers),]

boxplot(cc)$out
boxplot(cc,plot = FALSE)$out
outliers <- boxplot(cc,plot = FALSE)$out
print(outliers)
toyota[which(cc %in% outliers),]
toyota <-toyota[-which(toyota$cc %in% outliers),]

boxplot(Gears)$out
boxplot(Gears,plot = FALSE)$out
outliers <- boxplot(Gears,plot = FALSE)$out
print(outliers)
toyota[which(toyota$Gears %in% outliers),]
toyota <-toyota[-which(toyota$Gears %in% outliers),]

boxplot(Quarterly_Tax)$out
boxplot(Quarterly_Tax,plot = FALSE)$out
outliers <- boxplot(Quarterly_Tax,plot = FALSE)$out
print(outliers)
toyota[which(Quarterly_Tax %in% outliers),]
toyota <-toyota[-which(Quarterly_Tax %in% outliers),]

pairs(toyota)
cor(toyota)
library(corpcor)
cor2pcor(cor(toyota))

plot(Age_08_04,KM, col="dodgerblue4",pch=20)
plot(HP,cc, col="dodgerblue4",pch=20)


toyota_model1 <- lm(Price~ Age_08_04+KM+HP+Gears+cc+Doors+Quarterly_Tax+Weight)
summary(toyota_model1)
Confint(toyota_model1,level = 0.95)
predict(toyota_model1,interval = "predict")

layout(matrix(c(1,2,3,4),2,2))
plot(toyota_model1)

# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc) # Its significat to output

model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor)

model.car <- lm(Price ~ cc + Doors)
summary(model.car)


library(car)
influenceIndexPlot(toyota_model1,id.n=3)
influencePlot(toyota_model1,id.n=3)
toyota_model2 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Doors+Quarterly_Tax+Weight,data = toyota[-c(81),])
summary(toyota_model2)
influencePlot(toyota_model2)
influenceIndexPlot(toyota_model2) 
predict(toyota_model2)
vif(toyota_model1)
library(vcov)
avPlots(toyota_model2)
avPlots(toyota_model1)

toyota_model3 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = toyota[-c(81),])
summary(toyota_model3)
Confint(toyota_model3,level = 0.95)
predict(toyota_model3,interval = "predict")
plot(toyota_model3)

toyota_model4 <- lm(Price~log(Age_08_04)+log(KM)+log(HP)+log(cc)+log(Gears)+log(Quarterly_Tax)+log(Weight),data = toyota[-c(81),])
summary(toyota_model4)
Confint(toyota_model4,level = 0.95)
predict(toyota_model4,interval = "predict")

toyota_model_quad <- lm(Price~Age_08_04+I(Age_08_04)+KM+I(KM)+HP+I(HP)+cc+I(cc)+Gears+I(Gears)+Quarterly_Tax+I(Quarterly_Tax)+Weight+I(Weight),data = toyota[-c(81),])
summary(toyota_model_quad)
Confint(toyota_model_quad,level = 0.95)
predict(toyota_model_quad,interval = "predict")

library("MASS")
stepAIC(toyota_model_quad)
