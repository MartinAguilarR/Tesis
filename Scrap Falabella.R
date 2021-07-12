install.packages("RSelenium")
install.packages("rvest")
install.packages("tidyverse")
install.packages("tidytext")

library(RSelenium)
library(rvest)
library(tidyverse)
library(tidytext)

 ############################################################
## Comandos utiles al momentom de interactuar con RSelenium ##
 ############################################################

remote_driver$open() #Abre el navegador si es que se cierra
remote_driver$close() #Cierra el servidor
remote_driver$setWindowSize(width = 800, height = 300) #Tamaño de ventana del servidor
remote_driver$refresh() #Recargar la pagina


####################################
##              MODELO            ##
####################################

#Loading driver for Chrome
driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever =
                                system2(command = "wmic",
                                        args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                        stdout = TRUE,
                                        stderr = TRUE) %>%
                                stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                magrittr::extract(!is.na(.)) %>%
                                stringr::str_replace_all(pattern = "\\.",
                                                         replacement = "\\\\.") %>%
                                paste0("^",  .) %>%
                                stringr::str_subset(string =
                                                      binman::list_versions(appname = "chromedriver") %>%
                                                      dplyr::last()) %>%
                                as.numeric_version() %>%
                                max() %>%
                                as.character())

#Open the browser
remote_driver = driver[["client"]]


#Determinamos el número de ciclos a calcular
  #"x": numero de comentarios totales
  #"y": numero de comentario visibles
  #"z": numero de carga de comentarios

#El numero de replicaciones esta dado por "(n° comentarios total - n° comentarios visibles)/n° comentario de carga"
#Ej: 130 n° total, 4 visualizaciones inciales, 30 comentarios de carga:
# (130 - 4)/30 = 4.2 -> 4 ciclos.

calculo_ciclos = function(x,y,z){
  n_ciclos = (x-y)/z
  print(n_ciclos)
}

calculo_ciclos(1,1,1)

#Iteracion Boton comentarios

  #Una vez que calculamos el número de ciclos, estos determinaran el número de clicks del boton

click_boton = function(click){
          click <- replicate(click,
                  {
                  #Scroll de página
                  Body = remote_driver$findElement(using = 'class', 'custom_class')
                  Body$sendKeysToElement(list(key = "end"))
                  #Encontrar button
                  morereviews = remote_driver$findElement(using = 'class', 'bv-content-pagination-container')
                  #Click button
                  morereviews$clickElement()
                  #Segundos para cargar página
                  Sys.sleep(4)
                  })
}
click_boton()

#Iteracion Scrap y dataframe

#OBSERVACIONES
# SIEMPRE cambiar el "url", "click_boton()" y los nodos en caso de ser necesario

# El "Data" es el archivo con los links scrapeados
Data = read.csv2('C:\\Users\\marti\\OneDrive\\Documentos\\GitHub\\Tesis\\falabella_links.txt') #Aca se mete el archivo con el conjunto de links
n = 1               # numero de páginas a scrapear, está por defecto

for(i in Data) {
  url <- paste0(i)
  remote_driver$navigate(url) # navegar sitio

  click_boton(10) # Número de clicks al boton

  Comment = remote_driver$findElements(using = 'class', value = 'bv-content-summary-body-text')
  Comentarios =  unlist(lapply(Comment, function(x){x$getElementText()}))

  Comment_by = remote_driver$findElements(using = 'class', value = 'bv-content-author-name')
  Usuarios =  unlist(lapply(Comment_by, function(x){x$getElementText()}))

  Data = rbind(Data, data.frame(Usuarios, Comentarios, stringsAsFactors = TRUE))

  Data = Data[-c(1,2),]
}





