#install.packages("tm")
#install.packages("wordcloud")
#install.packages("SnowballC") # for text stemming
#install.packages("RColorBrewer") # color palettes
#install.packages("syuzhet") # for sentiment analysis
#install.packages("ggplot2") # for plotting graphs
# LOADING PACKAGES
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

text <- readLines(file.choose())
textDoc <- Corpus(VectorSource(text))
textDoc

toSpace <- content_transformer((function (x, pattern)
  gsub(pattern, " ", x)))

#CLEANING UP THE TEXT FILE
#making everything lowercase
textDoc <- tm_map(textDoc, content_transformer(tolower))
#removing numbers
textDoc <- tm_map(textDoc, removeNumbers)
#removing english common stopwords
#textDoc <- tm_map(textDoc, removeWords, stopwords("english"))
#removing punctuation
textDoc <- tm_map(textDoc, removePunctuation)
#remove whitespaces
textDoc <- tm_map(textDoc, stripWhitespace)
#text stemming; reducing words to their root form
#textDoc <- tm_map(textDoc, stemDocument)

#BUILDING THE TERM DOCUMENT MATRIX
textDocDTM <- TermDocumentMatrix(textDoc)
myDTM <- as.matrix(textDocDTM)
#sort by decreasing value of frequency
myDTM2 <- sort(rowSums(myDTM), decreasing = TRUE)
myDTM3 <- data.frame(word = names(myDTM2), freq = myDTM2)
myDTM3
#display the five most frequent words
head(myDTM3, 5)

#GENERATE THE WORD CLOUD
set.seed(1234)
wordcloud(words = myDTM3$word, freq = myDTM3$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = .4,
          colors = brewer.pal(8, "Dark2"))

#SENTIMENT SCORE USING GET_SENTIMENT() FUNCTION
#please note that different methods may have different scales
syuzhetVector <- get_sentiment(text, method="syuzhet")
#see the first row of the vector
head(syuzhetVector)
#see summary statistics of the vector
summary(syuzhetVector)
#bing
bingVector <- get_sentiment(text, method="bing")
head(bingVector)
summary(bingVector)
#affin
afinnVector <- get_sentiment(text, method="afinn")
head(afinnVector)
summary(afinnVector)
#using the sign function to compare the first row of each vector
rbind(
  sign(head(syuzhetVector)),
  sign(head(bingVector)),
  sign(head(afinnVector))
)

#EMOTIONAL CLASSIFICATION WITH NRC WORD EMOTION ASSOCIATION
#LEXICON
#counts anger, anticipation, disgust, fear, joy, sadness,
#surprise, trust
x <- get_nrc_sentiment(text)
#returns the top 10 lines of the get_nrc_sentiment dataframe
head(x, 10)

#creating a chart to visualise the emotions in the survey text
barplot(
  sort(colSums(prop.table(x[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)