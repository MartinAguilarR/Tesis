install.packages("tm")
install.packages("ggplot2")
install.packages("textstem")
install.packages("syuzhet")
install.packages("SnowballC")
install.packages("stringi")

library(stringi)
library(tm)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(readr)
library(SnowballC)
library(textstem)
library(syuzhet)
library(dplyr)

df = read.csv("C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\Df_Final.csv",
              encoding="latin1") %>% tbl_df()

df <- select(df, Categoria,Comentario)

# sacan los tildes
df$Comentario <- chartr('αινσϊρ','aeioun',df$Comentario)

# Df a corpus
myCorpus <- VCorpus(VectorSource(df$Comentario))

#Minuscula
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

#Remove Stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords('spanish'))

# Remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

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
Preprocessed_data_tokens = write.csv(myDf,'C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\Preprocessed_data_tokens_limpia.csv')
Preprocessed_data_Lemma = write.csv(myDf,'C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\Preprocessed_data_Lemma_limpia.csv')
Preprocessed_data_tokeLemma = write.csv(myDf3,'C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\Preprocessed_data_tokeLemma_limpia.csv')
