install.packages("caret", dependencies = T)

library(tidyverse)
library(readr)
library(dplyr)
library(caret)

data_limpia <- read_csv("C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\Preprocessed_data_tokeLemma_limpia.csv")

# semilla aleatoria
set.seed(100)

#el 10% de la muestra es para entrenar
indice_entrenamiento <- createDataPartition(y = data_limpia$text,
                                            p = 0.10,
                                            list = FALSE)
# se separan las muestras para entrenar y testear
test <- data_limpia[indice_entrenamiento,]
train <- data_limpia[-indice_entrenamiento,]



# calculamos el 50% del set de entrenamiento
num_entrenamiento<-as.integer(0.5*nrow(train))
num_entrenamiento

# Creamos una vector de 50% de los registros aleatorio
data_train <- sample(nrow(train), num_entrenamiento)

# Creamos el conjunto de registros de entrenamiento pasando ese vector a la tabla 
martin <- train[data_train,]
head(martin)

# Creamos los datos de comprobación o test (notese el -)
pancho <- train[-data_train,]
head(pancho)

# Guardan los csv
write.csv(martin, "C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\muestra_martin.csv")
write.csv(pancho, "C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\muestra_pancho.csv")
