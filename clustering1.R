crime_data <- read.csv(file.choose())
View(crime_data)
attach(crime_data)
ncol(crime_data)
names(crime_data)
crime_data1 <- crime_data[,2:5]
norm_crime_data1 <- scale(crime_data1)
distance <- dist(norm_crime_data1,method = "euclidean")
str(distance)
crime_clust <- hclust(distance,method = "complete")
plot(crime_clust,hang=-1)
rect.hclust(crime_clust,plot(crime_clust,hang=-1),k=4,border="blue")
group <- cutree(crime_clust,k=4)
crime_data_final <- cbind(crime_data,group)
View(crime_data_final)
aggregate(crime_data_final[,2:6],by=list(group),FUN = mean)


