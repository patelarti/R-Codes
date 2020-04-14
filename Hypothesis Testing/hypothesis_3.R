## Chi- square test. buyer.ratio

buyer.ratio <- read.csv(file.choose())
View(buyer.ratio)

## stack the data.
stacked_data <- stack(buyer.ratio)
View(stacked_data)

attach(stacked_data)
table(ind, values)

## chi-square test
chisq.test(table(ind, values))
## p value = 0.2931 > 0.05, hence we fail to reject null. the proportions are equal.