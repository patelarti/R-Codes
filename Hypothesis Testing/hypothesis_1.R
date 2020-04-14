# Hypothesis testing assignment. 2- Sample T test

cutlets <- read.csv(file.choose())
View(cutlets)

attach(cutlets)

# normality test
shapiro.test(Unit.A)
# the P value= 0.32 > 0.05.accepting null hypothesis. hence the data is normally distributed

shapiro.test(Unit.B)
# The P value= 0.5225 > 0.05.accepting null hypothesis. Hence the data is normally distributed

# Variance test
var.test(Unit.A, Unit.B)
# The P value= 0.3136> 0.05,accepting null hypothesis. hence the variance is equal.

# Two sample T-test
t.test(Unit.A, Unit.B, alternative = "two.sided", conf.level = 0.95, correct= TRUE)
# The P value = 0.4723>0.05, accepting null hypothesis. Hence the Two sample t- test is equal

t.test(Unit.A, Unit.B, alternative = "greater", conf.level = 0.95, correct= TRUE)
# The P value = 0.2362 >0.05, accepting null hypothesis. Hence the Two sample t-test is equal

t.test(Unit.A, Unit.B, alternative = "less", conf.level = 0.95, correct= TRUE)
# The P value = 0.7638 >0.05, accepting null hypothesis. Hence the Two sample t-test is equal