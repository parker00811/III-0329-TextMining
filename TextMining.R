# library(tm)

cat(gsub(" {2,}"," ",sentence))

# Tokenization-----------------------------------------
# readClipboard()
option1 <- "What is text mining?"
option2 <- "I'm not sure."
option3 <- "I like learning interesting skills."
option4 <- "Text mining is a trend in the future."
  
option1 <- gsub("[.]|[?]","",option1)
option2 <- gsub("[.]|[?]","",option2)
option3 <- gsub("[.]|[?]","",option3)
option4 <- gsub("[.]|[?]","",option4)


token1 <- unlist(strsplit(option1, " "))
token2 <- unlist(strsplit(option2, " "))
token3 <- unlist(strsplit(option3, " "))
token4 <- unlist(strsplit(option4, " "))

library(jiebaR)
mixseg = worker()
segment("你品嚐了夜的巴黎，你踏過下雪的北京", mixseg)
segment("江州市长江大桥参加了长江大桥的通车仪式", mixseg)


#Stemmming
library(SnowballC)
stem1 <- wordStem(token1, language = "english")
stem2 <- wordStem(token2, language = "english")
stem3 <- wordStem(token3, language = "english")
stem4 <- wordStem(token4, language = "english")


#字詞擴充
edit_dict()
mixseg = worker()
segment("全台大停電", mixseg)



# Practice (1)、(2)
Pr1 <- "Text mining, also referred to as text data mining, roughly equivalent to text analytics, refers to the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning."
Pr1 <- gsub("[.]|[?]","",Pr1)
unlist(strsplit(Pr1, " "))

example <- "文字探勘不同於資料探勘的地方，則在於它的原始輸入資料，都是沒有特定結構的純文字，這些文字的內容，都是用人類的自然語言所寫成的，所以，無法直接套用資料探勘的演算法，來計算出些什麼有意義的東西。"
segment(example, mixseg)

wordStem(unlist(strsplit(Pr1, " ")), language = "english")

edit_dict()
mixseg = worker()
segment(example, mixseg)
tagger = worker("tag")
tagger <= example


# Bad of words and word count--------------------------
corpus <- list(stem1,stem2,stem3,stem4)
tmp <- unique(c(stem1,stem2,stem3,stem4))

dtm_corpus <- matrix(0,nrow=length(corpus),ncol=length(tmp),dimnames=list(NULL,tmp))
for(i in 1:length(corpus)){
  count_tmp <- table(match(corpus[[i]],tmp))
  dtm_corpus[i,as.numeric(names(count_tmp))] <- count_tmp
}

sort(colSums(dtm_corpus))

#TF-IDF
tf <- dtm_corpus
idf <- log(nrow(dtm_corpus)/colSums(dtm_corpus))
tfidf <- dtm_corpus

for(word in names(idf)){
  tfidf[,word] <- tf[,word] * idf[word]
}

sort(colSums(tfidf))


library(wordcloud)
wordCount <- colSums(dtm_options)
words <- names(wordCount)
wordcloud(words,wordCount)



#Associated
cor(dtm_options)



# Practice(3)
doc_1 <- "資料處理與分析是當代IT顯學，在大數據當道的時代，讓我們來探討搜尋引擎與文字探勘的應用"
doc_2 <- "文字探勘，也被稱為文本挖掘、文字採礦、智慧型文字分析、文字資料探勘或文字知識發現，一般而言，指的是從非結構化的文字中，萃取出有用的重要資訊或知識"
doc_3 <- "資料探勘(Data Mining)與文字探勘(Text Mining)關係緊密，相較於前者顯著的結構化，後者長短不一、沒有規律，且尚有現今生活中隨口說出來的"
doc_4 <- "與 Data Mining 不同之處，在於 Text Mining 是針對文字進行分析，且文字多 ... TF-IDF 是一種用於資訊檢索與文字探勘的常用加權技術，為一種統計"
doc_5 <- "許多重要的資訊，其重要性不容小覷，使得文字探勘技術成為近年重要的研. 究領域之一。文字探勘（Text Mining）是從半結構化或非結構化的文件當中，. 發掘出文件中"


corpus <- list(doc_1,doc_2,doc_3,doc_4,doc_5)
mixseg = worker()
corpus <- lapply(corpus, function(u)segment(u, mixseg))
tmp <- unique(unlist(corpus))

dtm_corpus <- matrix(0,nrow=length(corpus),ncol=length(tmp),dimnames=list(NULL,tmp))
for(i in 1:length(corpus)){
  count_tmp <- table(match(corpus[[i]],tmp))
  dtm_corpus[i,as.numeric(names(count_tmp))] <- count_tmp
}


sort(colSums(dtm_corpus))

#TF-IDF
tf <- dtm_corpus
idf <- log(nrow(dtm_corpus)/colSums(dtm_corpus))
tfidf <- dtm_corpus

for(word in names(idf)){
  tfidf[,word] <- tf[,word] * idf[word]
}

sort(colSums(tfidf))


#Clustering words
# as.DocumentTermMatrix(dtm_corpus, weight = weightTf)

tdm_corpus <- t(dtm_corpus)
tdm_corpus <- tdm_corpus[nchar(rownames(tdm_corpus))!=1,]
tdm_corpus <- as.TermDocumentMatrix(tdm_corpus, weight = weightTf)
tdm_corpus <- removeSparseTerms(tdm_corpus, sparse=0.7)

dist_tdm_corpus <- dist(as.matrix(tdm_corpus))
fit <- hclust(dist_tdm_corpus, method="ward")
plot(fit)

###########

require(tm)
tmp <- "Text mining, also referred to as text data mining, roughly equivalent to text analytics, refers to the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means"
options <- c(option1,option2,option3,option4,tmp)
doc_Options <- Corpus(VectorSource(options))
doc_Options <- tm_map(doc_Options,stemDocument)
dtm_Options <- DocumentTermMatrix(doc_Options)
inspect(dtm_Options)
findAssocs(dtm_Options,"text",0.1)
tmp <- cor(as.matrix(dtm_Options))
tmp[rownames(tmp)=="text",]

doc <- "Text mining is a future."
doc <- Corpus(VectorSource(doc))
doc <- tm_map(doc,stemDocument)
inspect(DocumentTermMatrix(doc, list(stopwords=FALSE)))
