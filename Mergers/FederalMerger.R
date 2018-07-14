### ### ### ### ### ### ###
### Compulsado federal  ###
###      COTECORA       ###
### ### ### ### ### ### ###



# Preámbulo ####
library(tidyverse)
library(purrr)
library(stringr)


# Establece directorios relevantes ####

# Establece el directorio general
path.gral <- "//10.57.121.1/"
#path.gral <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/"


# Establece el directorio del equipo A
path.equipoa <- "//10.57.121.1/mendoza/equipoa/"
#path.equipoa <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/equipoa/"


# Establece el directorio del equipo B
path.equipob <- "//10.57.121.1/rodriguez/equipob/"
#path.equipob <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/nunez/equipob/"


# Establece el directorio del equipo C
path.equipoc <- "//10.57.121.1/romero/equipoc/"
#path.equipoc <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/romero/equipoc/"


# Establece el directorio donde se depositará el compulsado federal
path.compulsado <- "//10.57.121.1/mendoza/compulsadofederal/"
#path.compulsado <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/compulsadofederal/"



# Establece el directorio del buzón del COTECORA para depositar una copia del compulsado
path.buzon <- "//10.57.121.1/buzon/"
#path.buzon <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon/"



# Funciones auxiliares ####
# Función para imprimir porcentajes
porcentaje <- function(x, digits=2) paste0(round(x, digits = digits), "%")

# Función para imprimir la hora
format_hora <- function(x){
  # x es un caracter de cuatro digitos en formato
  # hhmm. Regresa hh:mm como caracter
  if(nchar(x)!= 4){
    return(x) 
  }else{
    hh <- stringr::str_sub(x, start = 1, end = 2)
    mm <- stringr::str_sub(x, start = 3, end = 4)
    hora <- paste0(hh, ":", mm)
    return(hora)
  }
}


determina_federal <- function(pathA = "//10.57.121.1/mendoza/equipoa/",
                              pathB = "//10.57.121.1/nunes/equipob/",
                              pathC = "//10.57.121.1/romero/equipoc/",
                              anteriores = "1300"){
  # ### ### ### ###
  # Determina si hay remesas con la misma hora
  # de los tres equipos
  #
  # Input.- pathA: directorio donde se buscarán las remesas del equipo A
  #         pathB: directorio donde se buscarán las remesas del equipo B
  #         pathC: directorio donde se buscarán las remesas del equipo C
  #
  # Output.- si los tres equipos tienen alguna remesa con la misma hora y día,
  #          regresa un vector de longitud 2 con la hora y el día
  #          de las remesas más recientes
  #          En cualquier otro caso regresa FALSE
  # ### ### ### ###
  
  # Obtén archivos
  filesA <- dir(pathA)
  filesB <- dir(pathB)
  filesC <- dir(pathC)
  
  # Filtra solo compulsados federales de cada equipo
  filesA <- filesA[stringr::str_detect(filesA,
                                       "equipoa00")]
  
  filesB <- filesB[stringr::str_detect(filesB,
                                       "equipob00")]
  
  filesC <- filesC[stringr::str_detect(filesC,
                                       "equipoc00")]
  
  # Determina si todos los equipos han subido algún compulsado
  if(purrr::is_empty(filesA) ||
     purrr::is_empty(filesB) ||
     purrr::is_empty(filesC) ){
    return(FALSE)
  }
  
  
  # Si todos han subido compulsados, obtén los días y horas correspondientes
  dia_hora_A <- stringr::str_sub(filesA,
                                 start = 10,
                                 end = 15)
  
  dia_hora_B <- stringr::str_sub(filesB,
                                 start = 10,
                                 end = 15)
  
  dia_hora_C <- stringr::str_sub(filesC,
                                 start = 10,
                                 end = 15)
  
  
  
  # Determina las fechas comunes de los compulsados
  comunes <- intersect(dia_hora_A,
                       intersect(dia_hora_B,
                                 dia_hora_C))
  
  # Corrobora que haya fechas comunes
  if(purrr::is_empty(comunes)){ return(FALSE) }
  
  # Determina remesa de más reciente creación
  comunes <- comunes[length(comunes)]
  
  # Asegura que no se haya analizado previamente dicha remesa
  if(comunes %in% anteriores){ return(FALSE) }
  
  # En caso contrario, regresa el día y la hora a compulsar
  dia <- stringr::str_sub(comunes, start = 1, end = 2)
  hora <- stringr::str_sub(comunes, start = 3, end = 6)
  
  # Si la hora no termina en 00, 15, 30 o 45 regresa FALSE
  fin <- stringr::str_sub(hora, start = 3, end = 4)
  if(!(fin %in% c("00", "15", "30", "45"))){ return(FALSE) }
  
  return(c(dia, hora))
}


med_G <- function(x1, x2, x3){
  # ### ### ### ###
  # Recibe tres intervalos con punto medio y regresa la mediana grande 
  # de ellos, es decir, la mediana de los intervalos de mayor longitud
  # para los extremos y la media de los puntos medios como medida central
  # Input.- x_i: vector de longitud tres, donde x_i[1]
  #              es el límite inferior del intervalo, 
  #              x_i[2] la estimación media y 
  #              x_i[3] el límite superior
  #
  # Output.- y: vector de longitud tres resultado de considerar
  #             la mediana grande de los tres intervalos y la media
  #             de las estimaciones medias
  # ### ### ### ###
  
  
    # Obtén longitudes de intervalos y determina el menor
    long1 <- x1[3] - x1[1]
    long2 <- x2[3] - x2[1]
    long3 <- x3[3] - x3[1]
    min.long <- min(long1, long2, long3)
    
    # Guarda y1 y y2 como los dos intervalos largos
    if(min.long == long1){
      y1 <- x2
      y2 <- x3
    } else if(min.long == long2){
      y1 <- x1
      y2 <- x3
    }else{
      y1 <- x1
      y2 <- x2
    }
    
    # Obtén la mediana grande
    y <- c(round(mean(c(y1[1], y2[1])), digits = 2),
           round(0.5 * (y1[2] + y2[2]), digits = 2),
           round(mean(c(y1[3], y2[3])), digits = 2))

    
    
    return(y)
}


compulsa_federal <- function(dia, hora,
                             pathA = "//10.57.121.1/mendoza/equipoa/",
                             pathB = "//10.57.121.1/nunes/equipob/",
                             pathC = "//10.57.121.1/romero/equipoc/",
                             path.compulsado = "//10.57.121.1/mendoza/compulsadofederal/"){
  # ### ### ### ###
  # Carga los análisis federales de los tres equipos del COTECORA y compulsa
  # 
  # Input.-  dia: día de la jornada
  #          hora: hora de las remesas a compulsar
  #          pathA: directorio donde están las remesas del equipo A
  #          pathB: directorio donde están las remesas del equipo B
  #          pathC: directorio donde están las remesas del equipo C
  #          path.compulsado: directorio donde se depositará el compulsado
  #
  # Output.- compulsado00diahora.csv, archivo csv en formato 
  #          de cartografía compulsando los análisis de los tres equipos
  # ### ### ### ###
  
  
  # Lee archivos de los equipos
  equipoa <- readr::read_csv(paste0(pathA,
                                    "equipoa00",
                                    dia,
                                    hora,
                                    ".csv"))
  
  equipob <- readr::read_csv(paste0(pathB,
                                    "equipob00",
                                    dia,
                                    hora,
                                    ".csv"))
  
  equipoc <- readr::read_csv(paste0(pathC,
                                    "equipoc00",
                                    dia,
                                    hora,
                                    ".csv"))
  
  
  # Escribe los archivos en el buzón
  write.csv(equipoa,
            file = paste0(path.buzon,
                          "equipoa/equipoa00",
                          dia,
                          hora,
                          ".csv"),
            quote = FALSE,
            row.names = FALSE)
  
  write.csv(equipob,
            file = paste0(path.buzon,
                          "equipob/equipob00",
                          dia,
                          hora,
                          ".csv"),
            quote = FALSE,
            row.names = FALSE)
  
  write.csv(equipoc,
            file = paste0(path.buzon,
                          "equipoc/equipoc00",
                          dia,
                          hora,
                          ".csv"),
            quote = FALSE,
            row.names = FALSE)
  
  
  partidos <- c("RAC",
                "JAMK",
                "AMLO",
                "JHRC",
                "PART")
  
  
  # Guarda estimaciones del intervalo del compulsado
  estimaciones <- NULL

  for(partido in partidos){
    
    # Obtén estimación del partido en cuestión
    analisis_a <- equipoa %>% 
      dplyr::select(partido) %>% 
      pull
    
    analisis_b <- equipob %>% 
      dplyr::select(partido) %>% 
      pull
    
    analisis_c <- equipoc %>% 
      dplyr::select(partido) %>% 
      pull
    
    # Obtén mediana grande de los intervalos
    comp <- med_G(analisis_a,
                  analisis_b,
                  analisis_c)
    
    
    
    # Guarda las estimaciones del partido
    estimaciones <- bind_cols(estimaciones,
                              tibble(partido = comp))
    
    
  }
  
  colnames(estimaciones) <- partidos
  
  
  # Construye tibble para generar análisis
  analisis <- tibble(EQ = rep("compulsado", 3),
                     EN = rep("00", 3),
                     R = rep(paste0(dia, hora), 3))
  analisis <- bind_cols(analisis, 
                        estimaciones,
                        tibble(LMU = 0:2))
  
  # Guarda compulsado en carpeta designada
  write.csv(analisis,
            file = paste0(path.compulsado,
                          "compulsado00",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  
  # Guarda copia del compulsado en el buzón
  write.csv(analisis,
            file = paste0(path.buzon,
                          "compulsadofederal/compulsado00",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
}



# Proceso iterativo ####
anteriores.federal <- "171300"
repeat{
  
  ### ### ### ### ### Análisis federal
  # Determina si hay análisis federales a compulsar
  fecha <- determina_federal(pathA = path.equipoa,
                             pathB = path.equipob,
                             pathC = path.equipoc,
                             anteriores = anteriores.federal)
  
  # Si no los hay espera 5 segundos
  # Si los hay, compulsa, actualiza anteriores
  # y espera 5 segundos
  if(fecha[1] == FALSE){
    Sys.sleep(time = 5)
  }else{
    # Compulsa
    compulsa_federal(dia = fecha[1],
                     hora = fecha[2],
                     pathA = path.equipoa,
                     pathB = path.equipob,
                     pathC = path.equipoc,
                     path.compulsado = path.compulsado)
    
    # Actualiza anteriores
    anteriores.federal <- c(anteriores.federal, paste0(fecha[1], fecha[2]))
    
    # Imprime mensaje de éxito
    mensaje <- paste0("Compulsado federal de las ",
                      format_hora(fecha[2]),
                      " hrs realizado exitosamente")
    print(mensaje)
    
    # Descansa 5 segundos
    Sys.sleep(time = 5)
  }
  ### ### ### ### ###
}
