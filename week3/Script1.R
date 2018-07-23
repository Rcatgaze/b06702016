#下載文本(使用老師寫好的pttTestFunction)

source('pttTestFunction.R')

id = c(2173:2174)
URL = paste0("https://www.ptt.cc/bbs/marvel/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)

#載入套件包
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)

#抓出文章內文對應的網址
from <- 2173
to   <- 2174
prefix = "https://www.ptt.cc/bbs/marvel/index"

data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)

head(data)

#完成基本文字清洗
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "他")
docs <- tm_map(docs, toSpace, "她")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "但")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "標題")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "老師")
docs <- tm_map(docs, toSpace, "吧")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "說")
docs <- tm_map(docs, toSpace, "一個")
docs <- tm_map(docs, toSpace, "想")
docs <- tm_map(docs, toSpace, "應該")
docs <- tm_map(docs, toSpace, "這樣")
docs <- tm_map(docs, toSpace, "到")
docs <- tm_map(docs, toSpace, "跟")
docs <- tm_map(docs, toSpace, "這個")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs

