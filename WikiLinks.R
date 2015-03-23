library(stringr)

Keyword <- "大數據"
Keyword <- iconv(Keyword,"big5","UTF-8")
Keyword <- URLencode(Keyword)
URL <- sprintf("http://zh.wikipedia.org/w/api.php?format=xmlfm&action=query&uselang=zh-tw&titles=%s&prop=revisions&rvprop=content&redirects",Keyword)

Content <- readLines(URL, warn = F)
Content <- iconv(Content,"UTF-8","UTF-8")
temp <- str_extract(Content,"\\[\\[.*\\]\\]")
temp <- temp[!is.na(temp)]
temp <- unlist(regmatches(temp, gregexpr("[[?<\\[].*?[?\\]]]", temp, perl=T)))
Dele <- grep(":|=",temp)

if(length(Dele)!=0){
  temp <- temp[-grep(":|=",temp)]
}

temp <- gsub("\\[|\\]","",temp)
temp <- gsub("\\|.*","",temp)
temp <- unique(temp)


