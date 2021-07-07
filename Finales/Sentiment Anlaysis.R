install.packages("naivebayes")
install.packages("reshape2")
install.packages("wordcloud")
install.packages("caret")
install.packages("syuzhet")
install.packages("stopwords")

library("stopwords")
library("ggplot2")
library("naivebayes")
library("tidytext")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("wordcloud")
library("tm")
library("caret")
library("SnowballC")
library("syuzhet")

#----- ANÁLISIS DE EMOCIONES -----#


Data<-read_csv('/home/jtobar/Tesis/Preprocessed_data_tokeLemma.csv', locale = readr::locale(encoding = "UTF-8"))
Data <- select(Data,text)
Data<- rename(Data,Comentario = text)

Data %>% unnest_tokens(input = "Comentario", output = "palabra") %>%
  count(palabra) %>%
  spread(key = palabra, value = n)


crear_matriz <- function(tabla) {
  tabla %>%
    mutate(Comment = quitar_url(Comentario)) %>%
    unnest_tokens(input = "Comentario", output = "palabra") %>%
  count(Categoria, palabra) %>%
  spread(key = palabra, value = n)
}


data_tokens <- unnest_tokens(tbl= Data,
                             output = "word",
                             input = "Comentario",
                             token = "words")


# Transformamos la base de textos importados en un vector para
# poder utilizar la función get_nrc_sentiment
palabra_df <- as.vector(Data$Comentario)

# Aplicamos la función indicando el vector y el idioma y creamos
# un nuevo data frame llamado emocion_df
emocion_df <- get_nrc_sentiment(char_v = palabra_df, language = "spanish")

# Unimos emocion.df con el vector tweets.df para ver como
# trabajó la función get_nrc_sentiment cada uno de los tweets
emocion_df2 <- cbind(Data$Comentario, emocion_df)
head(emocion_df2)

# Creamos un data frame en el cual las filas serán las emociones
# y las columnas los puntajes totales

# Empezamos transponiendo emocion.df
emocion_df3 <- data.frame(t(emocion_df))

# Sumamos los puntajes de cada uno de los comentarios para cada emocion
emocion_df3 <- data.frame(rowSums(emocion_df3))

#Nombramos la columna de puntajes como cuenta
names(emocion_df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion_df3 <- cbind("sentimiento" = rownames(emocion_df3), emocion_df3)

#Quitamos el nombre de las filas
rownames(emocion_df3) <- NULL

#Verificamos el data frame
print(emocion_df3)

#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion_df3[1:8,],
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Ocho emociones",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
print(sentimientos1)

#Segundo gráfico: se detallan los puntajes para las valoraciones
#positiva y negativa
sentimientos2 <- ggplot(emocion_df3[9:10,], 
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Valoración positiva o negativa", 
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
print(sentimientos2)


# Se unen los df emocionn_df2 y datatokens para dejar los sentimientos de cada palabra por nombre
names (emocion_df2)[1] = "Palabra"
names(data_tokens)
names(emocion_df2)

data_emocion <-  data.frame(
  palabra = emocion_df2$Palabra,
  positivo = emocion_df2$positive,
  negativo = emocion_df2$negative
)

#Para comprobar que quedo bien ordenado, spoiler: si quedo ordenado
head(data_emocion)
head(emocion_df2)
head(data_tokens)

#####------ LIMPIEZA DE DATA -----#####                       
#Esto quiza sea como para un analisis descriptivo

stopwords <- get_stopwords("es")
head(stopwords)

#Renombramos "word" a "palabra"
stopwords <- stopwords %>% rename(palabra = word)

#Eliminamos del dataframe las stopwords
data_limpia <- data_emocion %>% anti_join(stopwords)

data_limpia_fil <- data_limpia %>% count(palabra, sort = T) %>%
                    filter(n >= 2) %>% mutate(palabra = reorder(palabra, n))

head(data_limpia_fil, n>=2)

frecuencia_absoluta <- data_limpia %>% count(palabra, sort=TRUE)
frecuencia_absoluta

frecuencia_relativa <- data_limpia %>% count(palabra, sort = TRUE) %>% mutate(relativa = n / sum(n))
frecuencia_relativa

# Se juntan las palabras y emociones(pos o neg), a través de la variable nombre y pais
names(data_emocion)
as_tibble(data_emocion)

# Primera unión, aun se repiten algunos nombres
data_emocion2 <- data_emocion %>% 
                  group_by(palabra) %>%     
                  summarize(pos = sum(positivo),
                            neg = sum(negativo),   
                            n = n()) %>% 
                  ungroup()
    
# Segunda unión
head(data_emocion2)
data_emocion3 <- data_emocion2 %>% 
                  group_by(palabra) %>%     
                  summarize(positivo = sum(pos),
                            negativo = sum(neg),   
                            n = n()) %>% 
                  ungroup()

# Se elimina la variable "n" porque no aporta 
head(data_emocion3)

borrar3 <- "n"
Dataset_final <- data_emocion3[ , !(names(data_emocion3) %in% borrar3)]


# Creación de la variable "sentimiento" donde,
#1 = positivo y 0 = negativo

Df_final_sentiments = mutate(Dataset_final, sentimiento = ifelse(positivo > negativo, 1,0))

write.csv(Df_final_sentiments,'/home/jtobar/Tesis/Data_Final_sentiments.csv', row.names = FALSE)

