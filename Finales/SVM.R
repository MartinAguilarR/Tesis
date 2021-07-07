library("tidyverse")
library("tm")
library("e1071")
library("gmodels")
library("MASS")

# Datas a entrenar
data <- read_csv('/home/jtobar/Tesis/Data_Final_sentiments.csv', locale = readr::locale(encoding = "UTF-8"))
data$sentimiento <- as.factor(data$sentimiento)

# Ver distribución de comentarios. 
# Hay más positivos que negativos
table(data$sentimiento)


data_corpus <- VCorpus(VectorSource(data$palabra))

data_dtm<- train_dtm <- DocumentTermMatrix(data_corpus, control = list(
  weighting = weightTfIdf,
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

data_dtm

# create function to convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  # x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}

dtm_binary <- apply(data_dtm, MARGIN = 2, convert_counts)
data_dtm_binary <- as.data.frame(dtm_binary)

# Separación data
set.seed(123)

yelp_train <- data_dtm_binary[1:4720, ]
yelp_test  <- data_dtm_binary[4721:6743, ]

yelp_train_y <- data[1:4720, ]$sentimiento
yelp_test_y  <- data[4721:6743, ]$sentimiento

#  MODELO

# Entrenando modelo
modelo_svm <- svm(x = yelp_train, y = as.factor(yelp_train_y),
                  kernel = "linear", cost = 1, scale = TRUE,
                  type = "C-classification")
modelo_svm

# Prediccion
predicciones <- predict(object = modelo_svm, newdata = yelp_test)
table(observado = yelp_test_y, predicho = predicciones)

# Comprension de resultados
clasificaciones_erroneas <- sum( yelp_test_y != predicciones)
error <- 100 * mean( yelp_test_y != predicciones)

# Porcentaje de acierto
paste("El porcentaje de acierto es de",
      100 * ((886 + 849) / (160 + 128 + 886 + 849)), "%")
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2), "%")

confMatrix1 <- confusionMatrix(fit1.pred, sms_test1$cat, positive="ham")
confMatrix1
