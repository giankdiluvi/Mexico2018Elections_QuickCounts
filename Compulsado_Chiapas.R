### ### ### ### ### ### ### ###
### Compulsado de Chiapas   ###
### ### ### ### ### ### ### ###

# Preamble ####
library(tidyverse)
library(purrr)
library(gridExtra)


# Establece directorios relevantes ####

# Establece el directorio donde está el buzón del equipo
path.buzon <- "//10.57.121.1/buzon_a/"
#path.buzon <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon_a/"


# Establece el directorio general de Manuel Mendoza
path.mendoza <- "//10.57.121.1/mendoza/"
#path.mendoza <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/"


# Establece el directorio de Chiapas de Manuel Mendoza
path.mendoza.chiapas <- "//10.57.121.1/mendoza/mendozachiapas/"
#path.mendoza.chiapas <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/mendozachiapas/"


# Establece el directorio donde está el análisis de Michelle de Chiapas
path.anzarut.chiapas <- "//10.57.121.1/mendoza/anzarutchiapas/"
#path.anzarut.chiapas <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/anzarutchiapas/"



# Establece el directorio donde se depositará el compulsado de Chiapas
path.compulsado.chiapas <- "//10.57.121.1/mendoza/compulsadochiapas/"
#path.compulsado.chiapas <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/compulsadochiapas/"



# Funciones relevantes ####

# Función para imprimir porcentajes
porcentaje <- function(x, digits=2) paste0(round(x, digits = digits), "%")

# Función para imprimir la hora
format_hora <- function(x){
  # x es un caracter de cuatro digitos en formato
  # hhmm. Regresa hh:mm como caracter
  if(nchar(x)!= 4){
    return(x) 
  }else{
    hh <- substr(x, start = 1, stop = 2)
    mm <- substr(x, start = 3, stop = 4)
    hora <- paste0(hh, ":", mm)
    return(hora)
  }
}



determina_chiapas <- function(path.anzarut.chiapas = "//10.57.121.1/mendoza/anzarutchiapas/",
                              path.mendoza.chiapas = "//10.57.121.1/mendoza/mendozachiapas/",
                              anteriores = "1300"){
  # ### ### ### ###
  # Determina si hay remesas con la misma hora
  # de Michelle y Manuel Mendoza de Chiapas
  #
  # Input.- path.anzarut.chiapas: directorio donde se revisará si están las remesas
  #                               de Michelle
  #         path.mendoza.chiapas: directorio donde están las remesas de Manuel Mendoza
  #
  # Output.- si los dos equipos tienen remesa con la misma hora y día,
  #          regresa un vector de longitud 2 con la hora y el día
  #          de las remesas. En cualquier otro caso regresa FALSE
  # ### ### ### ###
  
  # Obtén archivos
  files.anzarut <- dir(path.anzarut.chiapas)
  files.mendoza <- dir(path.mendoza.chiapas)
  
  # Checa si hay archivos
  if(purrr::is_empty(files.anzarut) ||
     purrr::is_empty(files.mendoza)){
    return(FALSE) 
  }
  
  # Importa última hora de remesa
  hora.anzarut <- files.anzarut[stringr::str_detect(files.anzarut,
                                                    "anzarut07")]
  hora.mendoza <- files.mendoza[stringr::str_detect(files.mendoza,
                                                    "mendoza07")]
  
  
  # Checa si todos han subido al menos una remesa
  if(purrr::is_empty(hora.anzarut) ||
     purrr::is_empty(hora.mendoza)) { 
    return(FALSE) 
  }
  
  
  # Ahora determina cuál es la última de Michelle
  hora.anzarut <- hora.anzarut[length(hora.anzarut)]
  
  # Obtén día y hora de las remesas
  dia.anzarut <- stringr::str_sub(hora.anzarut,
                                  start = 10,
                                  end = 11)
  hora.anzarut <- stringr::str_sub(hora.anzarut,
                                   start = 12,
                                   end = 15)
  hora.mendoza <- stringr::str_sub(hora.mendoza,
                                   start = 12,
                                   end = 15)
  
  
  # Verifica que MM y LEN hayan corrido ese análisis
  if(!(hora.anzarut %in% hora.mendoza)){
    return(FALSE)
  }
  
  
  # Verifica que la remesa es nueva
  if(hora.anzarut %in% anteriores){
    return(FALSE)
  }
  
  # Regresa dia y hora
  return(c(dia.anzarut, hora.anzarut))
  
}

une <- function(x1, x2, x3 = -1){
  # ### ### ### ###
  # Recibe tres intervalos con punto medio y regresa la unión de los
  # dos con menor longitud, o recibe dos y regresa la unión
  # Input.- x_i: vector de longitud dos, donde x_i[1]
  #              es el límite inferior del intervalo, 
  #              x_i[2] la estimación media y 
  #              x_i[3] el límite superior
  #
  # Output.- y: vector de longitud tres resultado de considerar
  #             la unión de los dos intervalos más pequeños o 
  #             de los dos intervalos proporcionados, y la media
  #             de las estimaciones medias
  # ### ### ### ###
  
  if(x3[1] == -1){
    
    # Si solo hay dos argumentos regresa
    # la unión sencilla y media de medias
    return(c(min(x1[1], x2[1]),
             round(0.5 * (x1[2] + x2[2]), digits = 2),
             max(x1[3], x2[3])))
    
  }else{
    
    # Obtén longitudes de intervalos y determina el mayor
    long1 <- x1[3] - x1[1]
    long2 <- x2[3] - x2[1]
    long3 <- x3[3] - x3[1]
    max.long <- max(long1, long2, long3)
    
    # Guarda y1 y y2 como los dos intervalos cortos
    if(max.long == long1){
      y1 <- x2
      y2 <- x3
    } else if(max.long == long2){
      y1 <- x1
      y2 <- x3
    }else{
      y1 <- x1
      y2 <- x2
    }
    
    # Obtén la unión (asumiendo que no son ajenos)
    # y la media de medias
    inf <- min(y1[1], y2[1])
    med <- round(0.5 * (y1[2] + y2[2]), digits = 2)
    sup <- max(y1[3], y2[3])
    
    # Seguro para que la media
    # esté en el intervalo
    # if(med < inf ||
    #    med > sup){
    #   med <- round(0.5 * (inf + sup), digits = 2)
    # }
    
    y <- c(inf, med, sup)
    
    
    return(y)
  }
  
}



compulsa_chiapas <- function(dia, hora,
                             path.anzarut = "//10.57.121.1/mendoza/anzarutchiapas/",
                             path.mendoza = "//10.57.121.1/mendoza/mendozachiapas/",
                             path.compulsa = "//10.57.121.1/mendoza/compulsadochiapas/"){
  # ### ### ### ###
  # Carga los análisis de los dos miembros del equipo A
  # encargados de la estimación para el estado de Chiapas
  # y compulsa obteniendo la unión de los dos intervalos
  # 
  # Input.-  dia: día de la jornada
  #          hora: hora de las remesas a compulsar
  #          path.anzarut: directorio donde están las remesas de Michelle
  #          path.mendoza: directorio donde están las remesas de Manuel Mendoza
  #          path.compulsa: directorio donde se guardará el resultado final
  #
  # Output.- compulsado07diahora.csv, archivo csv en formato 
  #          de cartografía compulsando las remesas
  # ### ### ### ###
  
  
  # Lee archivos de los miembros del equipo
  anzarut <- readr::read_csv(paste0(path.anzarut,
                                    "anzarut07",
                                    dia,
                                    hora,
                                    ".csv"))
  mendoza <- readr::read_csv(paste0(path.mendoza,
                                    "mendoza07",
                                    dia,
                                    hora,
                                    ".csv"))
  
  
  # Escribe análisis en buzón del equipo
  write.csv(anzarut,
            file = paste0(path.buzon,
                          "Chiapas/Analisis/anzarut07",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  write.csv(mendoza,
            file = paste0(path.buzon,
                          "Chiapas/Analisis/mendoza07",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  
  partidos <- c("JAAB",
                "RAAG",
                "LFCC",
                "RCEC",
                "JAOR",
                "PART")
  
  # Guarda estimaciones del intervalo del compulsado
  estimaciones <- NULL
  for(partido in partidos){
    
    # Obtén estimación del partido en cuestión
    x1 <- anzarut %>% 
      dplyr::select(partido) %>% 
      pull
    
    x2 <- mendoza %>% 
      dplyr::select(partido) %>% 
      pull
    
    
    
    # Obtén unión de los dos intervalos
    comp <- une(x1, x2)
    
    
    
    # Guarda las estimaciones del partido
    estimaciones <- bind_cols(estimaciones,
                              tibble(partido = comp))
    
    
  }
  
  colnames(estimaciones) <- partidos
  
  
  
  
  # Construye tibble para generar análisis
  analisis <- tibble(EQ = rep("compulsado", 3),
                     EN = rep("07", 3),
                     R = rep(paste0(dia, hora), 3))
  analisis <- bind_cols(analisis, 
                        estimaciones,
                        tibble(LMU = 0:2))
  
  write.csv(analisis,
            file = paste0(path.compulsa,
                          "compulsado07",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  
  write.csv(analisis,
            file = paste0(path.buzon,
                          "Chiapas/Compulsados/compulsado07",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  
}

# Proceso iterativo ####
anteriores.chiapas <- "1300"
repeat{
  
  ### ### ### ### ### Análisis Chiapas
  # Determina si hay análisis de Chiapas a compulsar
  fecha <- determina_chiapas(path.anzarut = path.anzarut.chiapas,
                             path.mendoza = path.mendoza.chiapas,
                             anteriores = anteriores.chiapas)
  
  # Si no los hay espera 5 segundos
  # Si los hay, compulsa, actualiza anteriores
  # y espera 5 segundos
  if(fecha[1] == FALSE){
    Sys.sleep(time = 5)
  }else{
    compulsa_chiapas(dia = fecha[1],
                     hora = fecha[2],
                     path.anzarut = path.anzarut.chiapas,
                     path.mendoza = path.mendoza.chiapas,
                     path.compulsa = path.compulsado.chiapas)
    anteriores.chiapas <- c(anteriores.chiapas, fecha[2])
    mensaje <- paste0("Compulsado de Chiapas de las ",
                      format_hora(fecha[2]),
                      " hrs realizado exitosamente")
    print(mensaje)
    
    Sys.sleep(time = 5)
  }
  ### ### ### ### ###
}


