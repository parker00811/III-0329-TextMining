load("NewsContent.RData")
library(jiebaR)

StockKeyWords <- read.table("StockKeyWords.txt")

mixseg = worker()


Seg <- function(NEWs){
  tmp <- segment(NEWs, mixseg)
  tmp <- gsub("[0-9]","",tmp)
  tmp <- tmp[nchar(tmp)>1]
  return(tmp)
}

result <- unlist(sapply(NewsContent$Content,Seg))
sort(table(result))

library(wordcloud)
wordcloud(names(table(result)),table(result), max.words=40)


wordcloud(names(table(result)[table(result)<300]),table(result)[table(result)<300], 
          max.words=40,scale=c(2,0.002))
