install.packages(c("devtools", "rjson", "bit64", "httr"))
install_github("twitteR", username="geoffjentry")
library(twitteR)

api_key <- "YOUR API KEY"
api_secret <- "YOUR API SECRET"
access_token <- "YOUR ACCESS TOKEN"
access_token_secret <- "YOUR ACCESS TOKEN SECRET"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

searchTwitter("iphone")
rdmTweets <- userTimeline("rdatamining", n=200)
# save(rdmTweets, file="rdmTweets.RData")
