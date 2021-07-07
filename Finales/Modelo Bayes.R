library("tidyverse")
library(tm)
library(wordcloud)
library(e1071)
library(gmodels)

# Datas a entrenar
data <- read_csv('/home/jtobar/Tesis/Data_Final_sentiments.csv', locale = readr::locale(encoding = "UTF-8"))
data$sentimiento <- as.factor(data$sentimiento)

# Ver distribución de comentarios. 
# Hay más positivos que negativos
table(data$sentimiento)

# Separación data
set.seed(123)

yelp_train <- data[1:4720, ]
yelp_test  <- data[4721:6743, ]

# ver proporción de positivo y negativos
prop.table(table(yelp_train$sentimiento))

# Entrenamiento de X
train_corpus <- VCorpus(VectorSource(yelp_train$palabra))
test_corpus <- VCorpus(VectorSource(yelp_test$palabra))


# TF-IDF para train y test (Valor X)
train_dtm <- DocumentTermMatrix(train_corpus, control = list(
  weighting = weightTfIdf,
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

test_dtm <- DocumentTermMatrix(test_corpus, control = list(
  weighting = weightTfIdf,
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# Estadísticos de resultados de TF-IDF
train_dtm
test_dtm

# create function to convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  # x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}

# apply() convert_counts() to columns of train/test data
train_dtm_binary <- apply(train_dtm, MARGIN = 2, convert_counts)
test_dtm_binary  <- apply(test_dtm, MARGIN = 2, convert_counts)

train_binary <- as.data.frame(train_dtm_binary)
test_binary <- as.data.frame(test_dtm_binary)

#MODELO
# Estructura de X e Y
yelp_classifier <- naiveBayes(as.matrix(train_dtm_binary), yelp_train$sentimiento)

# Prediccion
yelp_test_pred <- predict(yelp_classifier, as.matrix(test_dtm_binary))
head(yelp_test_pred)


CrossTable(yelp_test_pred, yelp_test$sentimiento,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicción', 'actual'))

# Porcentaje de acierto
paste("El porcentaje de acierto es de",
      100 * ((880 + 188) / (821 + 134 + 880 + 188)), "%")

# Mejorar el modelo
# esto se hace a través de la adhesión de "laplace" 

# Laplace de 1
yelp_classifier2 <- naiveBayes(as.matrix(train_dtm_binary), yelp_train$sentimiento, laplace = 1)

yelp_test_pred2 <- predict(yelp_classifier2, as.matrix(test_dtm_binary))

CrossTable(yelp_test_pred2, yelp_test$sentimiento,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('prediccion', 'actual'))

# Porcentaje de acierto
paste("El porcentaje de acierto es de",
      100 * ((674 + 728) / (340 +281 + 674 + 728)), "%")

# Laplace de 0.5
yelp_classifier3 <- naiveBayes(as.matrix(train_dtm_binary), yelp_train$sentimiento, laplace = .5)

yelp_test_pred3 <- predict(yelp_classifier3, as.matrix(test_dtm_binary))

CrossTable(yelp_test_pred3, yelp_test$sentimiento,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# Porcentaje de acierto
paste("El porcentaje de acierto es de",
      100 * ((676 + 721) / (338 +288 + 676 + 721)), "%")
