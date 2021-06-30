library(dplyr)
library(tidytext)
library(janeaustenr)

# Data con tokenizado y categoria

data2 <- read_csv('/home/jtobar/Tesis/Preprocessed_data_tokens.csv', locale = readr::locale(encoding = "UTF-8"))
data2 <- select(data2,text)

df = read.csv("/home/jtobar/Tesis/Df_Final.csv",encoding="latin1") %>% tbl_df()
df <- select(df,Categoria)

data = merge(df,data2,all =T)
str(data)

comentario_tokens <- unnest_tokens(tbl= data,
                                   output = "word",
                                   input = "Comentario",
                                   token = "words") %>% count(Id, word, sort =TRUE)
                      

ggplot(comentario_tokens, aes(n/total, fill = id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# ENCODING 
comentarios <- factor(data2$Comentario)
factor <- factor(comentarios)
encoding <- as.numeric(factor)

#TF-IDF
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

total_words <- comentario_tokens %>% 
  group_by(id) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)


