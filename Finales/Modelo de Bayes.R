#####----- CONSTRUYENDO MODELO DE BAYES -----#####
###----- MODELO -----###

quitar_url = function(texto) {
  gsub("\\<http\\S*\\>|[0-9]", " ","\n", texto)
}

Data_final

Data_final %>% unnest_tokens(input = "Comentario", output = "palabra") %>%
  count(Pais, Nombre, palabra) %>%
  spread(key = palabra, value = n)


crear_matriz <- function(tabla) {
  tabla %>%
    mutate(text = quitar_url(text)) %>%
    unnest_tokens(input = "Comentario", output = "palabra") %>%
    count(Pais, Nombre, palabra) %>%
    spread(key = palabra, value = n) %>%
    select(-Nombre)
}

#AJUSTES POR SENTIMIENTO

ejemplo_matriz = Data_final %>%
  mutate(Pais = ifelse(Pais == "1" , Pais, "Otro"),
         Pais = as.factor(Pais)) %>%
  crear_matriz

elegir_usuario = function(nombres, usuario) {
  as.factor(ifelse(nombres %in% usuario, nombres, "Otro"))
}


set.seed(2001) #Base de prueba
ejemplo_entrenamiento = sample_frac(ejemplo_matriz, .7) #Set al 70%
ejemplo_prueba = setdiff(ejemplo_matriz, ejemplo_entrenamiento) #Set del % restante

crear_sets = function(tabla, prop = .7) {
  lista_sets <- list()
  lista_sets$train <- sample_frac(tabla, prop)
  lista_sets$test  <- setdiff(tabla, lista_sets[["train"]])
  
  lista_sets
}

#Correr modelo

ejemplo_modelo = naive_bayes(formula = Pais ~ .,  data = ejemplo_entrenamiento)

ejemplo_prediccion = predict(ejemplo_modelo, ejemplo_prueba)

head(ejemplo_prediccion, 25)

confusionMatrix(ejemplo_prediccion, ejemplo_prueba[["Pais"]])

#----- ANÁLISIS DE DATOS OBTENIDOS -----#

#Sensitivity: Porcentaje de predicción correcta del 100%. 
#Kappa: Indicador de predicción sobre probabilidad esperada. Mientras mas cercano a 1, más certera será la p(x) esperada.
#Balance accuaracy: Indica qué tan bien predice nuestro modelo tanto a la categoría positiva, como a la negativa.
#                   Esto es muy importante con datos como los nuestros, en los que tenemos clases no balanceadas, es decir, 
#                   que una es más abundante y tiene más probabilidades de aparecer que la otra.

#"Considerando todo lo anterior, podemos concluir que tenemos una buena precisión en nuestras predicciones,
# con más éxito para clasificar "Otro" que "MSFTMExico",
# y que nuestro modelo en efecto mejora la predicción con respecto a la probabilidad esperada".



#------------- AUTOMATIZACIÓN -------------#

obtener_bayes <- function(lista_sets, objetivo = "Pais") {
  bayes_formula<- as.formula(paste0(objetivo, "~ .") )
  bayes <- list()
  
  bayes$modelo <- naive_bayes(formula = bayes_formula, data = lista_sets[["train"]])
  bayes$prediccion   <- predict(object = bayes$modelo, newdata = lista_sets[["test"]])
  
  bayes
}

mat_conf <- function(resultado, set_test) {
  confusionMatrix(resultado[["prediccion"]], set_test[["test"]][["Pais"]])
}

ejemplo_conf <- confusionMatrix(ejemplo_prediccion, ejemplo_prueba[["Pais"]])

plot(ejemplo_conf[["table"]])

plot_conf <- function(resultados_bayes) {
  plot(resultados_bayes[["confusion"]][["table"]],
       col = c("#00BBFF", "#FF6A00"),
       main = resultados_bayes[["confusion"]][["positive"]])
}

#--------- IMPLEMENTACIÓN BAYES ----------#

hacer_bayes <- function(tabla, usuario) {
  ingenuo <- list()
  
  ingenuo[["matriz"]] <-
    tabla %>%
    mutate(screen_name = elegir_usuario(screen_name, usuario)) %>%
    crear_matriz()
  
  ingenuo[["sets"]] <- crear_sets(ingenuo[["matriz"]])
  
  ingenuo[["resultado"]] <- obtener_bayes(ingenuo[["sets"]])
  
  ingenuo[["confusion"]] <- list()
  
  ingenuo[["confusion"]] <- mat_conf(ingenuo[["resultado"]], ingenuo[["sets"]])
  
  ingenuo
}


#CASO 2

set.seed(1988)
bayes_cmll <- hacer_bayes(tuits_df, "CMLL_OFICIAL")

bayes_cmll[["mat"]]
bayes_cmll[["sets"]][["train"]]
bayes_cmll[["sets"]][["test"]]
bayes_cmll[["resultado"]][["modelo"]]
head(bayes_cmll[["resultado"]][["prediccion"]], 25)
bayes_cmll[["confusion"]]

lista_usuarios <- list(lopezobrador_ = "lopezobrador_",
                       MSFTMexico = "MSFTMexico",
                       UNAM_MX  = "UNAM_MX",
                       CMLL_OFICIAL = "CMLL_OFICIAL")

lista_bayes <- map(lista_usuarios, hacer_bayes, tabla = tuits_df)
hacer_bayes(map)

