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
# remove "r" and "big" from stopwords
idx <- which(myStopwords %in% c("r", "big"))
myStopwords <- myStopwords[-idx]
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
inspect(myCorpus[1:5])


# count frequency of "mining"
miningCases <- tm_map(myCorpusCopy, function(u)grep("\\<mining",u$content))
sum(unlist(miningCases))

# count frequency of "miners"
minerCases <- tm_map(myCorpusCopy, function(u)grep("\\<miners",u$content))
sum(unlist(minerCases))

# replace "miners" with "mining"
myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")
