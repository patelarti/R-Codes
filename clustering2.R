library(readxl)
EastWestAirlines <- read_xlsx(file.choose(),sheet = "data")
View(EastWestAirlines)
names(EastWestAirlines)
ncol(EastWestAirlines)
attach(EastWestAirlines)

EastWestAirlines_1 <- EastWestAirlines[,2:12]
norm_EastWestAirlines_1 <- scale(EastWestAirlines_1)

#hirerachical clutering
dist_airline <- dist(norm_EastWestAirlines_1,method = "euclidean")
str(dist_airline)
Airline_clust <- hclust(dist_airline,method = "complete")
plot(Airline_clust,hang=-1)
group_Airline <- cutree(Airline_clust,k=5)

EastWestAirlines_2 <- cbind(EastWestAirlines,group_Airline)
View(EastWestAirlines_2)

attach(EastWestAirlines_2)
aggregate(EastWestAirlines_2[,2:12],by=list(group_Airline),FUN = mean)

# K-MEANS CLustering

kmeans_airline <- kmeans(norm_EastWestAirlines_1,5)
str(kmeans_airline)
EastWestAirlines_2 <- cbind(EastWestAirlines_2,kmeans_airline$cluster)
names(EastWestAirlines_2)
View(EastWestAirlines_2)
aggregate(EastWestAirlines_2[,2:12],by=list(kmeans_airline$cluster),FUN = mean)
kmeans_airline$centers

library(cluster)
clusplot(clara(norm_EastWestAirlines_1,5))
clusplot(pam(norm_EastWestAirlines_1,5))
