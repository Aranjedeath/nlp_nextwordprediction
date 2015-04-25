memory.size(); gc()
#For reproducibility purposes
set.seed(1234)

##############################
## Loading libraries;  #######
## Downloading the dataset; ##
## Exploratory Analysis; #####
## Sampling the Data; ########
## Tokenization;      ########
## n-Grams; ##################
## Modeling  #################
##############################

#Installing and Loading necessary libraries
if(!"downloader" %in% rownames(installed.packages())){install.packages("downloader")}
library(downloader)
if(!"stringi" %in% rownames(installed.packages())){install.packages("stringi")}
library(stringi)
if(!"data.table" %in% rownames(installed.packages())){install.packages("data.table")}
library(data.table)
if(!"ggplot2" %in% rownames(installed.packages())){install.packages("ggplot2")}
library(ggplot2)


#Downloading the dataset
fileUrl = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download(fileUrl, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./data")

twitterfile = "./data/final/en_US/en_US.twitter.txt"
newsfile    = "./data/final/en_US/en_US.news.txt"
blogsfile   = "./data/final/en_US/en_US.blogs.txt"
  
connection_twitter <- file(twitterfile, "r") 
twitter <- readLines(connection_twitter, encoding="UTF-8") 
close(connection_twitter)
rm(connection_twitter); rm(twitterfile)

connection_news <- file(newsfile, "r") 
news <- readLines(connection_news, encoding="UTF-8") 
close(connection_news)

connection_blogs <- file(blogsfile, "r") 
blogs <- readLines(connection_blogs, encoding="UTF-8") 
close(connection_blogs)

#Size of the files:
file.info("./data/final/en_US/en_US.blogs.txt")$size/ 1024^2
file.info("./data/final/en_US/en_US.news.txt")$size/1024^2
file.info("./data/final/en_US/en_US.twitter.txt")$size/1024^2

####### Number of Lines
length(blogs); length(twitter); length(news)

### Sampling
sample_twitter <- twitter[as.logical(rbinom(length(twitter), 1, prob = 0.30))]
sample_blogs <- blogs[as.logical(rbinom(length(blogs), 1, prob = 0.30))]
sample_news <- news[as.logical(rbinom(length(news), 1, prob = 0.30))]
### Cleaning up
rm(twitter); rm(blogs); rm(news)

####### Number of characters per line
summary(nchar(sample_twitter))
character_stats_general <- stri_stats_general(sample_twitter)
wordcounts <- stri_count_words(sample_twitter)
qplot(wordcounts)

### Dealing with profane words
Url_badwords = "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download(Url_badwords, dest="badwords.txt", mode="wb") 
profane_words <- readLines("./badwords.txt")

### List of English Words
Url_en_words = "http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt"
download(Url_en_words, dest = "english_words.txt", mode = "wb")
english_words <- readLines("./englishwords.txt")

### Saving checkpoint
save(sample_twitter, profane_words, english_words, file = "sample.RData")

# Tokenizer function credits go to Maciej Szymkiewicz, a great CTA in
# Coursera Specialization on Data Science Capstone Project
######TOKENIZER -> N GRAMS
#' Ngrams tokenizer
#' 
#' @param n integer
#' @param skip_word_none boolean see: ?stri_split_boundaries
#' @param skip_word_number boolean see: ?stri_split_boundaries
#' @return n-gram tokenizer function
#' @examples
#' trigram_tokenizer <- ngram_tokenizer(3)
#' trigram_tokenizer(as.character(citation()))
#' 

ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  
  #' To avoid :: calls
  stri_split_boundaries <- stringi::stri_split_boundaries
  stri_join <- stringi::stri_join
  
  options <- stringi::stri_opts_brkiter(
    type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
  )
  
  #' Tokenizer
  #' 
  #' @param x character
  #' @return character vector with n-grams
  function(x) {
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}

quadrigram_tokenizer <- ngram_tokenizer(4)
trigram_tokenizer <- ngram_tokenizer(3)
bigram_tokenizer <- ngram_tokenizer(2)
unigram_tokenizer <- ngram_tokenizer(1)

twitter_1gram <- unigram_tokenizer(sample_twitter)
twitter_2gram <- bigram_tokenizer(sample_twitter)
twitter_3gram <- trigram_tokenizer(sample_twitter)
twitter_4gram <- quadrigram_tokenizer(sample_twitter)

#Spellchecking and jargon.
twitter_1gram <- twitter_1gram[twitter_1gram %in% english_words]
rm(english_words)
##Profanity Filtering
twitter_1gram[twitter_1gram %in% profane_words] <- "censured" 
rm(profane_words)

## Lower casing all n-grams; Shall I really lower case tokens?
twitter_1gram <- tolower(twitter_1gram)
twitter_2gram <- tolower(twitter_2gram)
twitter_3gram <- tolower(twitter_3gram)
twitter_4gram <- tolower(twitter_4gram)
## Actually, I decided not to lower case word.

###EDA
df1 <- as.data.frame(table(twitter_1gram))
top10uni <- head(df1[with(df1, order(-Freq)), ],10)
barplot(top10[,"Freq"], names.arg=top10$twitter_1gram, las=2)

df2 <- as.data.frame(table(twitter_2gram))
top10bi <- head(df2[with(df2, order(-Freq)), ],10)
barplot(top10bi[,"Freq"], names.arg=top10bi$twitter_2gram,las=2)

df3 <- as.data.frame(table(twitter_3gram))
top10tri <- head(df3[with(df3, order(-Freq)), ],10)
barplot(top10tri[,"Freq"], names.arg=top10tri$twitter_3gram, las=2)


### Saving checkpoint
save(ngram_tokenizer
    ,bigram_tokenizer
    ,trigram_tokenizer
    ,unigram_tokenizer
    ,twitter_1gram
    ,twitter_2gram
    ,twitter_3gram
    ,file = "ngrams.RData"
    )
load("ngrams.RData")

#### Unigram frequency
unigram_frequency <- as.data.table(table(twitter_1gram))
unigram_frequency <- unigram_frequency[unigram_frequency$N > 1]

### Bigram frequency
bigram_frequency <- as.data.table(table(twitter_2gram))
bigram_frequency <- bigram_frequency[bigram_frequency$N > 2]

### trigram frequency
trigram_frequency <- as.data.table(table(twitter_3gram))
trigram_frequency <- trigram_frequency[trigram_frequency$N > 2]

######## UNIGRAM TABLE ###########################################
###################################################################
splitted_unigrams <- lapply(unigram_frequency, function(x) {
  strsplit(as.character(x), split = " ")
})

unigram_table <- as.data.table(t(as.matrix(rbindlist(splitted_unigrams))))
unigram_table <- transform(unigram_table, V2 = as.integer(V2))

unigram_table <- data.frame(n1 = factor(unigram_table$V1, levels = twitter_1gram)
                           ,frequency = unigram_table$V2
)

unigram_table <- unigram_table[complete.cases(unigram_table), ]
unigram_table <- transform(unigram_table
                          ,n1 = factor(n1)
)
unigram_table <- data.table(unigram_table, key = "n1")

################################################################################
######## BI-GRAM TABLE #########################################################
################################################################################
splitted_bigrams <- lapply(bigram_frequency, function(x) {
  strsplit(as.character(x), split = " ")
})

bigram_table <- as.data.table(t(as.matrix(rbindlist(splitted_bigrams))))
bigram_table <- transform(bigram_table, V3 = as.integer(V3))

bigram_table <- data.frame(n2 = factor(bigram_table$V1, levels = twitter_1gram)
                           ,n1 = factor(bigram_table$V2 ,levels = twitter_1gram)
                           ,frequency = bigram_table$V3
)

bigram_table <- bigram_table[complete.cases(bigram_table), ]
bigram_table <- transform(bigram_table
                          ,n2 = factor(n2)
                          ,n1 = factor(n1)
)
bigram_table <- data.table(bigram_table, key = c("n2", "n1"))


################################################################################
######## TRI-GRAM TABLE ########################################################
################################################################################
splitted_trigrams <- lapply(trigram_frequency, function(x) {
  strsplit(as.character(x), split = " ")
})

trigram_table <- as.data.table(t(as.matrix(rbindlist(splitted_trigrams))))
trigram_table <- transform(trigram_table, V4 = as.integer(V4))

trigram_table <- data.frame(n3 = factor(trigram_table$V1, levels = twitter_1gram)
                            ,n2 = factor(trigram_table$V2 ,levels = twitter_1gram)
                            ,n1 = factor(trigram_table$V3 ,levels = twitter_1gram)
                            ,frequency = trigram_table$V4
)

trigram_table <- trigram_table[complete.cases(trigram_table), ]
trigram_table <- transform(trigram_table
                           ,n3 = factor(n3)
                           ,n2 = factor(n2)
                           ,n1 = factor(n1)
)
trigram_table <- data.table(trigram_table, key = c("n3","n2", "n1"))

################################################################################
################################################################################

sentence_handler <- function(input_sentence) {
  input_sentence <- as.character(input_sentence)
  input_sentence <- tolower(input_sentence) 
  split_input <- strsplit(input_sentence, split = " ")
  input_sentence <- factor(t(as.data.frame(split_input)))
  
  if (length(input_sentence) == 1) {
    input <- data.frame(n2 = input_sentence)
  } else {
    input <- data.frame(n3 = input_sentence[length(input_sentence)-1]
                       ,n2 = input_sentence[length(input_sentence)])
  }
  return(input)
}

backoff <- function(sentence_input) {
  
  require(data.table)
  
  input <- sentence_handler(sentence_input)
  
  filter_tri_table <- trigram_table[(as.character(trigram_table$n3) == as.character(input$n3) & as.character(trigram_table$n2) == as.character(input$n2)),]
  filter_bi_table  <- bigram_table[(as.character(bigram_table$n2) == as.character(input$n2)),]
  
  if (nrow(filter_tri_table) > 0) {
    prediction <- filter_tri_table[which.max(frequency), n1]    
  } 
  else if (nrow(filter_bi_table) > 0) {
    prediction <- filter_bi_table[which.max(frequency), n1]  
  }  
  else {
    prediction <- unigram_table[which.max(frequency),n1]
  }
  return(as.character(prediction))
}

save(unigram_table
     ,bigram_table
     ,trigram_table
     ,sentence_handler
     ,backoff
     ,file = "ngram_tables.RData"
)
load("ngram_tables.RData")
