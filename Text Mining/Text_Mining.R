###ONE:
##1) Extract tweets for any user (try choosing a user who has more tweets)
##2) Perform sentimental analysis on the tweets extracted from the above
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(lubridate)
library(scales)
library(dplyr)

text <- readLines(file.choose())
View(text)
length(text)
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- function(x,pattern) gsub(pattern ," ",x)
docs <- tm_map(docs,toSpace,"/")
docs <- tm_map(docs,toSpace,"@")
docs <- tm_map(docs,toSpace,"\\|")
docs <- tm_map(docs,tolower)
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removeWords,c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,stripWhitespace)
dtm <- TermDocumentMatrix(docs)
a <- as.matrix(dtm)
b <- sort(rowSums(a),decreasing = TRUE)
d <- data.frame(word=names(b),freq=b)
library(wordcloud)
library(wordcloud2)
wordcloud(words = d$word,freq = d$freq,min.freq = 0,max.words = 1000,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(1,"Dark2"))
wordcloud2(d,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

findFreqTerms(dtm,lowfreq = 8)
findAssocs(dtm,terms = "freedom",corlimit = 0.3)
head(d,10)
barplot(d[1:10,]$freq,las=2,names.arg = d[1:10,]$word,col = "lightblue",main = "Most frequent words",ylab = "word frequencies")

## build a term-document matrix
library(syuzhet)
mydata.dtm3 <- TermDocumentMatrix(docs)
mydata.dtm3
dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)
rowTotals <- apply(dtm, 1, sum)
dtm.new   <- dtm[rowTotals > 0, ]
lda <- LDA(dtm.new, 10) # find 10 topics
term <- terms(lda, 20) # first 5 terms of every topic
tops <- terms(lda)
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)

cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
par(family = "HiraKakuProN-W3")
plot(cls)

s <- get_nrc_sentiment(text)
head(s)
text[4]
get_nrc_sentiment('pretending')
get_nrc_sentiment('can learn')
barplot(colSums(s), las = 2.5, col = rainbow(10),ylab = 'Count',main= 'Sentiment scores')

##TWO:-------------------------------------------------------------------------------
##1) Extract reviews of any product from ecommerce website like snapdeal and amazon
##2) Perform sentimental analysis
#------------------------------------------------------------------------------------

# Amazon Reviews 

aurl <- "https://www.amazon.in/Immortals-Meluha-Shiva-Trilogy/dp/9380658745/ref=sr_1_1?dchild=1&keywords=meluha&qid=1585924294&sr=8-1#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"book.txt",row.names = F)
getwd()


aurl1 <- "https://www.amazon.in/Asus-S510UN-BQ256T-15-6-inch-i5-8250U-MX150-2GB/dp/B07DB71YGD/ref=sr_1_10?crid=1DV836QVR2QB7&dchild=1&keywords=laptop+i5+8th+generation+8gb+ram&qid=1585889746&s=computers&sprefix=lap%2Ccomputers%2C362&sr=8-10#customerReviews"
amazon_reviews1 <- NULL
for (i in 1:10){
  murl1 <- read_html(as.character(paste(aurl1,i,sep="=")))
  rev1 <- murl1 %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews1 <- c(amazon_reviews1,rev1)
}
write.table(amazon_reviews1,"laptop.txt",row.names = F)
getwd()

# Using While loop to get all reviews of book without using page number 
samp_url <- "https://www.amazon.in/Immortals-Meluha-Shiva-Trilogy/dp/9380658745/ref=sr_1_1?dchild=1&keywords=meluha&qid=1585924294&sr=8-1#customerReviews"
i=1
p=1
predator <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(samp_url,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  predator <- c(predator,rev)
  i <- i+1
  p=length(rev)
}

length(predator)
## Using While loop to get all reviews of laptop without using page number 

samp_url1 <- "https://www.amazon.in/Asus-S510UN-BQ256T-15-6-inch-i5-8250U-MX150-2GB/dp/B07DB71YGD/ref=sr_1_10?crid=1DV836QVR2QB7&dchild=1&keywords=laptop+i5+8th+generation+8gb+ram&qid=1585889746&s=computers&sprefix=lap%2Ccomputers%2C362&sr=8-10#customerReviews"
i=1
p=1
predator1 <- NULL
while(p>0){
  t_url1 <- read_html(as.character(paste(samp_url1,i,sep="=")))
  rev1 <- t_url1 %>%
    html_nodes(".review-text") %>%
    html_text()
  predator1 <- c(predator1,rev1)
  i <- i+1
  p=length(rev1)
}

length(predator1)


# Snapdeal reviews
surl_1 <- "https://www.snapdeal.com/product/logitech-x50-bluetooth-speakers-black/619404812342/reviews?page=2&sortBy=HELPFUL"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"speaker.txt",row.names = FALSE)
getwd()


surl_2 <- "https://www.snapdeal.com/product/motorola-grey-moto-e4-plus/626220502238/reviews?page=2&sortBy=HELPFUL"
snapdeal_reviews2 <- NULL
for (i in 1:30){
  surl2 <- read_html(as.character(paste(surl_2,sep=as.character(i))))
  srev2 <- surl2 %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews2 <- c(snapdeal_reviews2,srev2)
}
length(snapdeal_reviews2)
write.table(snapdeal_reviews2,"book1.txt",row.names = FALSE)
getwd()


#Extracting reviews from a travel website ###################

rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g303881-d3702032-Reviews-Clouds_Land_Munnar-Munnar_Idukki_District_Kerala.html#REVIEWS"
for(i in 1:10){
  url<-read_html(as.character(paste(url1,sep="")))
  ping<-url %>%
    html_nodes(".review-text") %>%
    html_text() 
  rev<-c(rev,ping)
}
length(rev)

##THREE:------------------------------------------------------------------------------------------
##1) Extract movie reviews for any movie from IMDB and perform sentimental analysis
##2) Extract anything you choose from the internet and do some research on how we extract using R
##Programming and perform sentimental analysis.
#-------------------------------------------------------------------------------------------------

# ############# IMDB reviews Extraction ################
 a<-10
 wonder_woman<-NULL
url1<-"http://www.imdb.com/title/tt0451279/reviews?start="
for(i in 0:22){
   url<-read_html(as.character(paste(url1,i*a,sep="")))
   wonder<-url %>%
     html_nodes("#tn15content div+ p") %>%
     html_text() 
   wonder_woman<-c(wonder_woman,wonder)
 }
 write.table(wonder_woman,file="wonder_woman.txt")
getwd()

# IMDBReviews #############################
url2 <- "https://www.imdb.com/title/tt1620680/reviews?ref_=tturv_ql_4"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(url2,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

write.table(IMDB_reviews,file="WrinkleInTime.txt")
getwd()


