install.packages(c("devtools", "rjson", "bit64", "httr"))
install_github("twitteR", username="geoffjentry")


library(twitteR)

api_key <- "YOUR API KEY"
api_secret <- "YOUR API SECRET"
access_token <- "YOUR ACCESS TOKEN"
access_token_secret <- "YOUR ACCESS TOKEN SECRET"


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

searchTwitter("iphone")

#開始進行資料擷取
rdmTweets <- userTimeline("rdatamining", n=200)
# save(rdmTweets, file="rdmTweets.RData")
load("rdmTweets.RData")
length(rdmTweets)

#將tweets轉換為data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

#語料庫
library(tm)
myCorpus <- Corpus(VectorSource(df$text))

#資料處理：字母小寫化、移除標點符號、數字、網址

# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
#  remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("english"), "available", "via")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


#class(myCorpus[[1]]) character => PlainTextDocument,TextDocument
myCorpus <- Corpus(VectorSource(myCorpus))

# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[11:15])

stemCompletion_mod <- function(x,dict=dictCorpus) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
}

myCorpus <- tm_map(myCorpus, stemCompletion_mod, dict=myCorpusCopy)
inspect(myCorpus[11:15])


# count frequency of "mining"
miningCases <- tm_map(myCorpus, function(u)grep(" mining ",u$content))
which(sapply(miningCases, length)==1)
sum(unlist(miningCases))

# count frequency of "miners"
minerCases <- tm_map(myCorpus, function(u)grep(" miner ",u$content))
which(sapply(minerCases, length)==1)
sum(unlist(minerCases))

# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, function(u)gsub(" miner "," mining ", u$content))


# Building a Term-Document Matrix
myCorpus <- Corpus(VectorSource(myCorpus))
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))


# look at the frst six terms starting with “r” and tweets numbered 101 to 110.
idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])


# look at the popular words and the association between words
findFreqTerms(myTdm, lowfreq=10)


#amp是什麼？ 查看是哪一篇文章出現了amp：原來是....
ampCases <- tm_map(myCorpus, function(u)grep(" amp ",u$content))
which(sapply(ampCases, length)==1)
myCorpus[[11]]
rdmTweets[[11]]

#remove "amp"
myCorpus <- tm_map(myCorpus, removeWords, "amp")

#Re-Building a Term-Document Matrix and check popular words
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))
findFreqTerms(myTdm, lowfreq=10)


#visuallization
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
barplot(termFrequency, las=2)


#Association
findAssocs(myTdm, 'r' , 0.25)
findAssocs(myTdm, 'mining' , 0.25)


#文字雲
library(wordcloud)
m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)

#移除差距太大的詞，可讓視覺化的數據更有意義
m.withoutR <- m[-which(rownames(m)=="r"),]
wordFreq <- sort(rowSums(m.withoutR), decreasing=TRUE)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)


# Clustering Words
# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")

plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))



# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# k-means clustering of tweets
# http://www.dotblogs.com.tw/dragon229/archive/2013/02/04/89919.aspx
k <- 8
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=3)


names(which(m3[which(kmeansResult$cluster==1)[4],]!=0))


for (i in 1:k) {
cat(paste("cluster ", i, ": ", sep=""))
s <- sort(kmeansResult$centers[i,], decreasing=T)
cat(names(s)[1:5], "\n")
# print the tweets of every cluster
# print(rdmTweets[which(kmeansResult$cluster==i)])
}
