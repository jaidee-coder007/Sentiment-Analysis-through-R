#Author of Scripts: Jaideep Raikwar
# Tool used for Sentiment Analysis : version.string R version 3.3.1 (2016-06-21)
#****************************************START SCRIPT*****************************************
#R miningScripts:-
#load text mining libraries
library(tm)
library(ggplot2)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(graph)
library(Rgraphviz)
library(topicmodels)
library(lattice)
pacman::p_load(sentimentr)
library(sentimentr)
#load files into corpus

#READ FILE:-
#***********
setwd("//ecvdafs01/userdata1/ja305643/Desktop/Datascience/SentimentAnalysis/Sentiment_Analysis_of_abc_bank/")
reviewbnk <-read.csv("sampled_data1.csv",header=TRUE,stringsAsFactors = FALSE)
 
#TEXT CLEANING:-
#***************
#Create COPRPUS:-
#****************
#create corpus from vector
docs <- Corpus(VectorSource(reviewbnk$Comment))
#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))
 
#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
 
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
#writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)

#USE NLP techniques for feature Extraction
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#Check Term Frequency
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
#Show top 10 words frequency
head(freq,10)
#write.csv(freq[ord],”word_freq.csv”)

#TAKE BACKUP FOR SURE
#Before doing Graph visualization please take backup of your corpus for safety of original cleaned Corpus
docs1<-docs

# PLOT GRAPHS for VISUALIZATION
#Plot frequent words:-
topfreq <- subset(freq,freq >100)
df <- data.frame(term = names(topfreq), freq = topfreq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") + coord_flip() +
theme(axis.text=element_text(size=7)) 
#WordCloud of frequent words using Term Document matrix functionality through tm package:-
tdm <- TermDocumentMatrix(docs,control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 5,
random.order = F, colors = pal) 
#Create Network of Terms
plot(dtm, term = topfreq, corThreshold = 0.3, weighting = T)
#CREATE Document Term Matrix
dtm <- as.DocumentTermMatrix(tdm)

#TOPIC MODELLING
rowTotals <- apply(dtm , 1, sum)
dtm.new   <- dtm[rowTotals> 0, ]
lda <- LDA(dtm.new, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", ")) 


#PHASE-I SENTIMENT ANALYSIS:-
*****************************
reviews <- (reviewbnk$Comment)
polarity <- sentiment(reviews)
head(polarity)

#Check the polarity on sorted order
avgpolarity <-sentiment_by(polarity)
head(avgpolarity)

#Polarity Graphs:-

xyplot(ave_sentiment~element_id,avgpolarity)
histogram(ave_sentiment~element_id,avgpolarity)
z<-ggplot(data=avgpolarity,aes(y=ave_sentiment,x=element_id))
z+stat_smooth(colour=3, size=6) + geom_point()

#Graph of Negative Polarity:-
tmp<-polarity$sentiment <0
negative_polarity <- polarity[tmp, ]
head(negative_polarity)

avg_neg_polarity <- sentiment_by(negative_polarity)
xyplot(ave_sentiment~element_id,avg_neg_polarity)
histogram(ave_sentiment~element_id,avg_neg_polarity)
x1<-ggplot(data=avg_neg_polarity,aes(y=ave_sentiment,x=element_id))
x1+stat_smooth(colour=3, size=6) + geom_point()	