library(tm)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(readr)
library(SnowballC)
library(textstem)
library(syuzhet)
library(textstem)

df = read.csv("/home/jtobar/Tesis/Df_Final.csv",
              encoding="latin1") %>% tbl_df()

df <- select(df, Categoria,Comentario)

# Df a corpus
myCorpus <- VCorpus(VectorSource(df$Comentario))

#Minúscula
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# Remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

#Remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

#Remove Stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords('spanish'))

# Lemmatizing
myCorpusLemmatized <- tm_map(myCorpus, lemmatize_strings)
myCorpusLemmatized <- tm_map(myCorpusLemmatized, PlainTextDocument)

# tokenize the corpus
myCorpusTokenized <- lapply(myCorpus, scan_tokenizer)
myCorpusTokeLemma <- lapply(myCorpusLemmatized, scan_tokenizer)

# Concatenate tokens by document, create data frame
myDf <- data.frame(text = sapply(myCorpusTokenized, paste, collapse = " "), stringsAsFactors = FALSE)
myDf2 <-data.frame(text = sapply(myCorpusLemmatized, paste, collapse = " "), stringsAsFactors = FALSE)
myDf3 <- data.frame(text = sapply(myCorpusTokeLemma, paste, collapse = " "), stringsAsFactors = FALSE)

# Documento .csv con texto final.
Preprocessed_data_tokens = write.csv(myDf,'/home/jtobar/Tesis/Preprocessed_data_tokens.csv')
Preprocessed_data_Lemma = write.csv(myDf,'/home/jtobar/Tesis/Preprocessed_data_Lemma.csv')
Preprocessed_data_tokeLemma = write.csv(myDf3,'/home/jtobar/Tesis/Preprocessed_data_tokeLemma.csv')
