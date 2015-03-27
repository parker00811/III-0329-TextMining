library(XML)
Sys.setlocale(category='LC_ALL', locale='C')
url <- "https://play.google.com/store/apps/details?id=com.facebook.katana&hl=zh-TW"
html <- htmlParse(paste(readLines(url,warn = F),collapse = ""),encoding="utf8")


temp <- xpathSApply(html,"//div[@class='review-body']")
doc <- lapply(temp,function(u)xmlValue(u,trim = T))
doc <- gsub("完整評論","",doc)
