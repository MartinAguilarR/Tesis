library("tidyverse")

# Datas a entrenar

data1 <- read_csv('/home/jtobar/Tesis/RS_Data_Final_sentiments.csv', locale = readr::locale(encoding = "UTF-8"))
data1 <- select(data1, palabra)
data1 <- rename(data1, Comentario = palabra)

# Data con tokenizado  
data2 <- read_csv('/home/jtobar/Tesis/Preprocessed_data_tokens.csv', locale = readr::locale(encoding = "UTF-8"))
data2 <- select(data2,text)
data2 <- rename(data2,Comentario = text)

# Data con Lemmatizado
data3 <- read_csv('/home/jtobar/Tesis/Preprocessed_data_Lemma.csv', locale = readr::locale(encoding = "UTF-8"))
data3 <- select(data3,text)
data3 <- rename(data3,Comentario = text)


# Entenamiento X e Y
set.seed(123)

# Entrenamiento X
indices_x <- sample(nrow(data1), 0.70 * nrow(data1))
train_x <- data1[indices_x, ]
test_x <- data1[3107:4438, ]

# Entrenamiento Y
indices_y <- sample(nrow(data2), 0.70* nrow(data2))
train_y <- data2[indices_y, ]
test_y <- data2[3107:4438, ]

