This project is part of the Data Science Specialization, provided by Johns Hopkins University. 

The main goal is simply to predict the next word in a given sentence.

## Data Ingestion
Downloading the data is fairly simple. The code in the appendix is pretty self explanatory and simple to understand. The data is divided by three different sources: blogs, twitter and news sets of sentences. Initially, I took a small and random subset of each dataset, to explore each one in detail.

## Tokenization and profanity filtering
As referred in wikipedia, tokenization is the process of breaking a stream of text up into words, phrases, symbols, or other meaningful elements called tokens. All whitespaces are removed, as well as punctuation and numbers. Also, given the context of the problem - next word prediction - I think capitalization should be kept as it is. The algorithm I used to perform tokenization (attached as appendix) is based on the stringi package on R and provided by a R user on GitHub - see References for details. In terms of profanity filtering, I took a list of profane words on the web and used it to replace profanity on the datasets by a “censured” message. Furthermore, all word belong to the english dictionary.

## Modelling

N-grams are stored in efficient data tables and the predicton is based on a backoff model.
