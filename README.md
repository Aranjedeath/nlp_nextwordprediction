This project is part of the Data Science Specialization, provided by Johns Hopkins University. 

The main goal is simply to predict the next word in a given sentence.

Read codebook for instructions.

## Data Ingestion
Downloading the data is fairly simple. The code in the appendix is pretty self explanatory and simple to understand. The data is divided by three different sources: blogs, twitter and news sets of sentences. Initially, I took a small and random subset of each dataset, to explore each one in detail.

## Tokenization and profanity filtering
As referred in wikipedia, tokenization is the process of breaking a stream of text up into words, phrases, symbols, or other meaningful elements called tokens. My approach on this was straighforward. I decided to remove all whitespaces, as well as punctuation and numbers. Also, given the context of the problem - next word prediction - I think capitalization should be kept as it is. The algorithm I used to perform tokenization (attached as appendix) is based on the stringi package on R and provided by a R user on GitHub - see References for details. In terms of profanity filtering, I took a list of profane words on the web and used it to replace profanity on the datasets by a “censured” message.

## Discussion and forthcoming work
In this work I have experienced a bit of a cold start. I have a lack of expertise on the Natural Language Processing domain, so I had to acquire some background before getting some ideas on how to approach the problem at hand. I picked english language as it is the one we are going to be evaluated. Although I present a strategy for profanity filtering, I am still working on a better approach. So far I have mostly been cleaning and gaining feel on the dataset, as well as understanding the concepts on the type of problem. From this point, I intend to build a model with the n-grams based on Markov chains, and deploy it on shinyapps. In this report I focused on twitter data, which I think is the one that needs more attention regarding preprocessing.
