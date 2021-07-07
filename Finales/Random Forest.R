install.packages("tidyr")
install.packages("skimr")
install.packages("ISLR")
install.packages("ggpubr")
install.packages("tree")

library(tidyr)
library(skimr)
library(ISLR)
library("ggpubr")
library(tree)
library(ggplot2)
library(tidyverse)
library(tm)


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

#Aplicamos conversión a factor
dtm_binary <- apply(data_dtm, MARGIN = 2, convert_counts)

#Se transforma a dataframe
data_dtm_binary <- as.data.frame(dtm_binary)

# Separación data
set.seed(123)

yelp_train <- data_dtm_binary[1:4720, ]
yelp_test  <- data_dtm_binary[4721:6743, ]

yelp_train_y <- data[1:4720, ]$sentimiento
yelp_test_y  <- data[4721:6743, ]$sentimiento


# Entrenamiento del modelo
# ==============================================================================
arbol_clasificacion <- tree(
  formula = yelp_train_y ~ .,
  data    = yelp_train,
  minsize = 10
)
summary(arbol_clasificacion)


# Estructura del árbol creado
# ==============================================================================
par(mar = c(1,1,1,1))
plot(x = arbol_clasificacion, type = "proportional")
text(x = arbol_clasificacion, splits = TRUE, pretty = 0, cex = 0.7, col = "firebrick")

#EVALUAR EL MODELO
# Error de test del modelo
# ==============================================================================
predicciones <- predict(
  arbol_clasificacion,
  newdata = yelp_test,
  type    = "class"
)

table(predicciones, yelp_test_y)

## 1= positivo y 0=negativo
## el modelo predice que 56 comentarios son negativos correctamente
##predice que 9 son positivos pero realmente esos 9 son negativos
## predice que 34 son positivos correctamente
paste("El porcentaje de acierto es de",
      100 * ((917 + 455) / (97 +554+ 917 + 455)), "%")


#######---------poda del arbol (pruning)-------------###############

# El árbol se crece al máximo posible para luego aplicar el pruning
arbol_clasificacion <- tree(
  formula = yelp_train_y ~ .,
  data    = yelp_train,
  mincut  = 1,
  minsize = 2,
  mindev  = 0
)

# Búsqueda por validación cruzada
set.seed(123)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 5)

# Tamaño óptimo encontrado
# ==============================================================================
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
size_optimo

resultados_cv <- data.frame(n_nodos = cv_arbol$size, clas_error = cv_arbol$dev,
                            alpha = cv_arbol$k)

p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = clas_error)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = size_optimo, color = "red") +
  labs(title = " Error de clasificación vs \n tamaño del árbol") +
  theme_bw() 

p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = clas_error)) +
  geom_line() + 
  geom_point() +
  labs(title = " Error de clasificación vs \n penalización alpha") +
  theme_bw() 

ggarrange(p1, p2)


arbol_final <- prune.misclass(
  tree = arbol_clasificacion,
  best = size_optimo
)

# Error de test del modelo final
#-------------------------------------------------------------------------------
predicciones <- predict(arbol_clasificacion, newdata = yelp_test, type = "class")
table(predicciones, yelp_test_y)
paste("El porcentaje de acierto es de",
      100 * ((917 + 455) / (97 + 544 + 917 + 455)), "%")
