## 2 proportion test
fantaloons <- read.csv(file.choose())
View(fantaloons)

attach(fantaloons)
tablef <- table(Weekdays, Weekend)
tablef

### As the data is character we cannot carry normality test and variance test.
## 2 proportion test
prop.test(x= c(66,47), n=c(233, 167), conf.level = 0.95, alternative = "greater")
## the P value=0.5. which is > 0.05. we fail to reject null. 
prop.test(x= c(66,47), n=c(233, 167), conf.level = 0.95, alternative = "two.sided")
## the p value = 1. which is > 0.05. we fail to reject null.
prop.test(x= c(66,47), n=c(233, 167), conf.level = 0.95, alternative = "less")
## the P value=0.5. which is > 0.05. we fail to reject null. 

# Hence the null = The Proportion of males walking in the store </= The proportion of 
## females walking in the store based on the day of the week.