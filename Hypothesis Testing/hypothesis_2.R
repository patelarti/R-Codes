# one way annova test
lab_tat <- read.csv(file.choose())
View(lab_tat)

attach(lab_tat)

# Normality test
shapiro.test(Laboratory.1)
# the p-value= 0.5508 > 0.05, we fail to reject null. the data is normal.
shapiro.test(Laboratory.2)
# the p-value= 0.8637 > 0.05, we fail to reject null. the data is normal.
shapiro.test(Laboratory.3)
# the p-value= 0.4205 > 0.05, we fail to reject null. the data is normal.
shapiro.test(Laboratory.4)
# the p-value= 0.6619 > 0.05, we fail to reject null. the data is normal.

# Variance test
var.test(Laboratory.1, Laboratory.2)
# the P-value= 0.1675 > 0.05, fail to reject null. equal variance
var.test(Laboratory.2, Laboratory.3)
# the P-value= 0.2742 > 0.05, fail to reject null. equal variance
var.test(Laboratory.3, Laboratory.4)
# the P-value= 0.3168 > 0.05, fail to reject null. equal variance

# Anova test
stacked_data <- stack(lab_tat)
View(stacked_data)
anova_result <- aov(values ~ ind, data = stacked_data)
summary(anova_result)
# the P-value = 2e-16 < 0.05. we reject null. the TAT of one supplier is different

