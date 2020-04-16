library(recommenderlab)
library(caTools)
book <- read.csv(file.choose())
str(book)
names(book)
book <- book[,-1]
book <- book[,-1]
colnames(book) <- c("User","Title","Author","Publisher","Rating")
View(book)
hist(book$Rating)
plot(book$Rating)
head(book$Rating)
book_matrix <- as(book,'realRatingMatrix')
head(book_matrix)

book_model1 <- Recommender(book_matrix,method="POPULAR")
Recommend_book1 <- predict(book_model1,book_matrix[100:101],n=5)
as(Recommend_book1,"list")
book_mode2 <- Recommender(book_matrix,method="IBCF")
Recommend_book2 <- predict(book_model2,book_matrix[100:101],n=5)
as(Recommend_book2,"list")

book_model3 <- Recommender(book_matrix,method="UBCF")
Recommend_book3 <- predict(book_model3,book_matrix[1000:1001],n=1)
as(Recommend_book1,"list")

