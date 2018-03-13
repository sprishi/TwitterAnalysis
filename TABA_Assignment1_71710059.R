rm(list = ls())   # clears workspace

# Install or load packages

try(require(tm) || install.packages("tm"))
try(require(stringr) || install.packages("stringr", dependencies = TRUE))
try(require(dplyr) || install.packages("dplyr"))
try(require(tidytext) || install.packages("tidytext"))
try(require(tidyr) || install.packages("tidyr"))
try(require(gsubfn) || install.packages("gsubfn", dependencies = TRUE))
try(require(wordcloud) || install.packages("wordcloud"))

library(tm)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(gsubfn)
library(wordcloud)

#function to analyze tweets
twitter.analysis <- function(temp.text) {
  
  # Length of Tweets
  print("Length of tweets - ")
  print(length(temp.text$text))
  cat('\n')
  x <- temp.text$text
  twitter.df <- NULL
    
  twitter.hashtag <- unlist(strapplyc(x,"\\#\\w+"))   # extract twitter handles
  twitter.handle <- unlist(strapplyc(x,"\\@\\w+"))  # extract twitter hashtags
  
  # function to clean tweets
  clean_Twitter_Corpus <- function(x) {
    
    x  =  gsub("\\\\x[89a-f][0-9a-f]", "", x) # remove latin encoded characters
    x  =  gsub("(ftp|http|https):\\/\\/(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(\\/|\\/([\\w#!:.?+=&%@!\\-\\/]))?", " ", x) # remove http urls
    x  =  gsub("^(b\'RT|b\'|b\"RT|b\"|via)", "", x) # remove b character
    x  =  gsub('#\\w+|@\\w+',' ',x)           # remove hastags and @ 
    x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
    x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
    x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
    x  =  tolower(x)                          # convert to lower case characters
    x  =  removeNumbers(x)                    # removing numbers
    x  =  stripWhitespace(x)                  # removing white space
    x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
    return(x)
  }    
  
  # clean the twitter texts
  tweets <- clean_Twitter_Corpus(x)
  
  # top 5 hashtags
  tbl.tag <- table(twitter.hashtag)
  hashtags <- tbl.tag[order(tbl.tag, decreasing = T)] %>% head(5)
  print(hashtags)
  twitter.df$top_hashtags <- row.names(hashtags)
  cat('\n')
  
  # top 5 handles
  tbl.hdl <- table(twitter.handle)
  handles <- tbl.hdl[order(tbl.hdl, decreasing = T)] %>% head(5)
  print(handles)
  twitter.df$top_handles <- row.names(handles)
  cat('\n')
  
  # store the tweets into dataframe
  textdf = data_frame(text = tweets) 
  
  # top 5 unigrams
  unigrams <- textdf %>% unnest_tokens(word, text) %>% count(word, sort = TRUE) %>% rename(count = n) %>% anti_join(stop_words) %>% head(5)
  print(unigrams)
  twitter.df$top_words <- unigrams$word
  cat('\n')
  
  # top 5 bigrams
  bigrams <- textdf %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(bigram, sort = TRUE) %>% head(5)
  print(bigrams)
  twitter.df$top_bigrams <- bigrams$bigram
  cat('\n')
  
  # word cloud
  textdf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))
  
  # data frame with columns 'top_words', 'top_bigrams', 'top_hashtags' and 'top_handles'
  twitter.df <- as.data.frame(twitter.df, stringsAsFactors=FALSE)
  twitter.df
}


# Read @IBMResearch tweets
research.tweets <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE, encoding = "Latin-1")
twitter.analysis(research.tweets)


# Read @IBMResearch tweets
watson.tweets <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE, encoding = "Latin-1")
twitter.analysis(watson.tweets)

