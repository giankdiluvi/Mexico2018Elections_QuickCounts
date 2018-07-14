### ### ### ### ### ### ###
### Compulsado nacional ###
### del equipo A        ###
### ### ### ### ### ### ###

# Preamble ####
library(tidyverse)
library(purrr)
library(gridExtra)
#library(easyGgplot2)
ggplot2::theme_set(theme_bw())

# Establece directorios relevantes ####

# Establece el directorio donde está el buzón del equipo
path.buzon <- "//10.57.121.1/buzon_a/"
#path.buzon <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon_a/"

# Establece el directorio donde están los análisis individuales nacionales
path.buzon.nacional <- "//10.57.121.1/buzon_a/Nacional/Analisis/"
#path.buzon.nacional <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon_a/Nacional/Analisis/"

# Establece el directorio donde depositar una copia del compulsado nacional
path.buzon.nacional.remesas <- "//10.57.121.1/buzon_a/Nacional/Compulsados/"
#path.buzon.nacional.remesas <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon_a/Nacional/Compulsados/"

# Establece el directorio general de Manuel Mendoza
path.mendoza <- "//10.57.121.1/mendoza/"
#path.mendoza <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/"


# Establece el directorio nacional de Manuel Mendoza
path.mendoza.nacional <- "//10.57.121.1/buzon_a/Nacional/Analisis/"
#path.mendoza.nacional <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/buzon_a/Nacional/Analisis/"


# Establece el directorio donde se depositará el compulsado nacional original
path.compulsado.nacional <- "//10.57.121.1/mendoza/equipoa/"
#path.compulsado.nacional <- "C:/Users/giank/Dropbox/COTECORA/10_57_121_1/mendoza/equipoa/"








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
    hh <- substr(x, start = 1, stop = 2)
    mm <- substr(x, start = 3, stop = 4)
    hora <- paste0(hh, ":", mm)
    return(hora)
  }
}

determina_nacional <- function(path.buzon.nacional = "//10.57.121.1/buzon_a/Nacional/Analisis",
                      anteriores = "1300"){
  # ### ### ### ###
  # Determina si hay remesas con la misma hora
  # de los tres miembros del equipo A
  #
  # Input.- path: directorio donde se revisará si están las remesas
  #
  # Output.- si los tres equipos tienen remesa con la misma hora y día,
  #          regresa un vector de longitud 2 con la hora y el día
  #          de las remesas. En cualquier otro caso regresa FALSE
  # ### ### ### ###
  
  # Obtén archivos
  files <- dir(path.buzon.nacional)
  #hora.mendoza <- dir(path.mendoza.nacional)
  
  # Checa si hay archivos
  if(purrr::is_empty(files)){ return(FALSE) }
  
  # Importa última hora de remesa
  hora.anzarut <- files[stringr::str_detect(files,
                                            "anzarut00")]
  hora.mendoza <- files[stringr::str_detect(files,
                                            "mendoza00")]
  hora.nieto <- files[stringr::str_detect(files,
                                          "nieto00")]
  

  
  # Checa si todos han subido al menos una remesa
  if(purrr::is_empty(hora.anzarut) ||
     purrr::is_empty(hora.mendoza) || 
     purrr::is_empty(hora.nieto) ) { 
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
  hora.nieto <- stringr::str_sub(hora.nieto,
                                 start = 10,
                                 end = 13)
  
  # Verifica que MM y LEN hayan corrido ese análisis
  if(!(hora.anzarut %in% hora.mendoza &&
     hora.anzarut %in% hora.nieto)){
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

compulsa_nacional <- function(dia, hora,
                     path.buzon.nacional = "//10.57.121.1/buzon_a/Nacional/Analisis",
                     path.compulsado.nacional = "//10.57.121.1/mendoza/equipoa/",
                     path.buzon = "//10.57.121.1/buzon_a"){
  # ### ### ### ###
  # Carga los análisis de los tres miembros del equipo A
  # y compulsa obteniendo la unión de los dos intervalos
  # de menor longitud para cada candidato
  # Además grafica comparativo LEN U MM contra compulsado
  # 
  # Input.-  dia: día de la jornada
  #          hora: hora de las remesas a compulsar
  #          path.buzon.nacional: directorio donde están las remesas
  #          path.compulsado.nacional: directorio donde se guardará el resultado final
  #          path.buzon: directorio del buzón del equipo A
  #
  # Output.- equipoa00diahora.csv, archivo csv en formato 
  #          de cartografía compulsando las remesas
  # ### ### ### ###
  
  
  # Lee archivos de los miembros del equipo
  anzarut <- readr::read_csv(paste0(path.buzon.nacional,
                                    "anzarut00",
                                    dia,
                                    hora,
                                    ".csv"))
  mendoza <- readr::read_csv(paste0(path.mendoza.nacional,
                                    "mendoza00",
                                    dia,
                                    hora,
                                    ".csv"))
  nieto <- readr::read_csv(paste0(path.buzon.nacional,
                                  "nieto00",
                                  dia,
                                  hora,
                                  ".csv"))
  
  partidos <- c("RAC",
                "JAMK",
                "AMLO",
                "JHRC",
                "PART")
  
  # Arregla compulsado de Michelle si tiene iniciales de partidos
  if("PAN_PRD_MC" %in% colnames(anzarut)){
    colnames(anzarut) <- c("EQ", "EN", "R", partidos, "LMU")
  }
  
  # Guarda estimaciones del intervalo del compulsado
  estimaciones <- NULL
  estimaciones.mm <- NULL
  for(partido in partidos){
    
    # Obtén estimación del partido en cuestión
    xa <- anzarut %>% 
      dplyr::arrange(LMU) %>% 
      dplyr::select(partido) %>% 
      pull
    
    xm <- mendoza %>% 
      dplyr::arrange(LMU) %>% 
      dplyr::select(partido) %>% 
      pull
    
    xn <- nieto %>% 
      dplyr::arrange(LMU) %>% 
      dplyr::select(partido) %>% 
      pull
    
    # Obtén unión de los dos más cortos
    comp <- une(xa, xm, xn)
    comp.mm <- une(xm, xn)
    
    
    
    # Guarda las estimaciones del partido
    estimaciones <- bind_cols(estimaciones,
                              tibble(partido = comp))
    
    estimaciones.mm <- bind_cols(estimaciones.mm,
                                 tibble(partido = comp.mm))
    
    
  }
  
  colnames(estimaciones) <- partidos
  colnames(estimaciones.mm) <- partidos
  
  
  
  
  # Construye tibble para generar análisis
  analisis <- tibble(EQ = rep("equipoa", 3),
                     EN = rep("00", 3),
                     R = rep(paste0(dia, hora), 3))
  analisis <- bind_cols(analisis, 
                        estimaciones,
                        tibble(LMU = 0:2))
  
  
  write.csv(analisis,
            file = paste0(path.compulsado.nacional,
                          "equipoa00",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  
  write.csv(analisis,
            file = paste0(path.buzon.nacional.remesas,
                          "equipoa00",
                          dia,
                          hora,
                          ".csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8",
            quote = FALSE)
  
  # Gráficas comparativas ####
  
  # Gráfica puntual para guardar:
  graph.puntual.titulo <- paste0("Comparativo de intervalos para la remesa de las ",
                        format_hora(hora),
                        " hrs")
                  
  
  graph.puntual <- estimaciones[c(1, 3), ] %>% 
    dplyr::mutate(Estimacion = "Compulsado") %>% 
    dplyr::bind_rows(estimaciones.mm[c(1, 3), ] %>% 
                       mutate(Estimacion = "LEN+MM")) %>%
    mutate(LMU = c("inf", "sup", "inf", "sup")) %>% 
    gather(Partido, Votos, RAC:PART) %>% 
    spread(LMU, Votos) %>% 
    mutate(Votos = 0.5 * (inf + sup)) %>% 
    ggplot(aes(x = Partido, y = Votos, color = Estimacion)) +
    geom_errorbar(aes(ymin = inf, ymax = sup), 
                  width = 0.2,
                  lwd = 1,
                  position = position_dodge(width = 0.3)) +
      scale_y_continuous(labels = porcentaje,
                         limits = c(0, 100)) +
      labs(title = graph.puntual.titulo,
           x = "Partido / coalición",
           y = "Proporción estimada de votos en favor",
           color = "Estimación") +
      scale_x_discrete(limits = c("PART",
                                  "RAC",
                                  "JAMK",
                                  "AMLO",
                                  "JHRC")) +
    theme(axis.text.x = element_text(size = 8)) +
    geom_text(aes(label = porcentaje(inf, digits=1),
                  color = Estimacion,
                  vjust = 3),
              check_overlap = TRUE,
              size = 2,
              position = position_dodge(width = 1)) +
    geom_text(aes(label = porcentaje(sup, digits=1),
                  color = Estimacion,
                  vjust = -3),
              check_overlap = TRUE,
              size = 2,
              position = position_dodge(width = 1))
      
    
    ggsave(filename = paste0("Comparativo Estimacion_", dia, hora, ".png"),
           path = paste0(path.buzon,
                         "Grafs_Compulsado_Nacional/Remesas/"))
    
    
    graph.puntual.aux <- estimaciones[c(1, 3), ] %>% 
      dplyr::mutate(Estimacion = "Compulsado") %>% 
      dplyr::bind_rows(estimaciones.mm[c(1, 3), ] %>% 
                         mutate(Estimacion = "LEN+MM")) %>%
      mutate(LMU = c("inf", "sup", "inf", "sup")) %>% 
      gather(Partido, Votos, RAC:PART) %>% 
      spread(LMU, Votos) %>% 
      mutate(Votos = 0.5 * (inf + sup)) %>% 
      ggplot(aes(x = Partido, y = Votos, color = Estimacion)) +
      geom_errorbar(aes(ymin = inf, ymax = sup), 
                    width = 0.2,
                    lwd = 1,
                    position = position_dodge(width = 0.3)) +
      scale_y_continuous(labels = porcentaje,
                         limits = c(0, 100)) +
      labs(subtitle = "Comparativo puntual",
           x = "Partido / coalición",
           y = "Proporción estimada de votos en favor",
           color = "Estimación") +
      scale_x_discrete(limits = c("PART",
                                  "RAC",
                                  "JAMK",
                                  "AMLO",
                                  "JHRC")) +
      theme(axis.text.x = element_text(size = 8)) +
      geom_text(aes(label = porcentaje(inf, digits=1),
                    color = Estimacion,
                    vjust = 3),
                size = 2,
                position = position_dodge(width = 1)) +
      geom_text(aes(label = porcentaje(sup, digits=1),
                    color = Estimacion,
                    vjust = -3),
                size = 2,
                position = position_dodge(width = 1)) +
      theme(plot.title = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            legend.position = "top")
    
    
    
  # Gráficas evolución
  
  # Genera archivo para pegar a historia
  hist.aux <- estimaciones %>% 
    mutate(LMU = c("inf", "med", "sup")) %>% 
    gather(partido, Votos, RAC:PART) %>% 
    spread(LMU, Votos) %>% 
    mutate(hora = hora,
           Estimacion = "Compulsado") %>% 
    select(hora, Estimacion, partido, inf, sup, med)
  
  hist.aux2 <- estimaciones.mm %>% 
    mutate(LMU = c("inf", "med", "sup")) %>% 
    gather(partido, Votos, RAC:PART) %>% 
    spread(LMU, Votos) %>% 
    mutate(med = 0.5 * (inf + sup),
           hora = hora,
           Estimacion = "LEN+MM") %>% 
    select(hora, Estimacion, partido, inf, sup, med)
  
  # Importa evolución anterior, agrega nueva info y exporta
  historia <- read_csv(file = paste0(path.buzon,
                                     "Auxiliar_Grafs/Evolucion_partidos.csv"),
                       col_types = "cccddd") %>% 
    bind_rows(hist.aux) %>% 
    bind_rows(hist.aux2)
  
  write.csv(historia,
            file = paste0(path.buzon,
                          "Auxiliar_Grafs/Evolucion_partidos.csv"),
            row.names = FALSE,
            fileEncoding = "UTF-8")
  
  # Genera gráficas
  for(partidoo in partidos){
    
    graph.titulo <- paste0("Evolución del comparativo para ",
                           partidoo,
                           " actualizado a las ",
                           format_hora(hora),
                           " hrs")
    
    graph.evolution <- historia %>% 
      filter(partido %in% partidoo) %>% 
      ggplot() + 
      geom_ribbon(aes(x = hora,
                      ymin = inf,
                      ymax = sup,
                      fill = Estimacion,
                      group = Estimacion),
                  alpha = 0.4) +
      geom_path(aes(x = hora,
                    y = med,
                    color = Estimacion,
                    group = Estimacion),
                linetype = 2) +
      labs(title = graph.titulo,
           x = "Hora de la remesa",
           y = "Proporción estimada de votos en favor") +
      scale_y_continuous(labels = porcentaje) +
      scale_x_discrete(labels = format_hora) +
      scale_fill_discrete(name = "Estimación") +
      scale_color_discrete(name = "Estimación")
    
    
    #,
    #limits = c(0, NA)
    
    graph.name <- paste0(partidoo, "_", dia, hora, ".png")
    
    ggsave(filename = graph.name,
           path = paste0(path.buzon,
                         "Grafs_Compulsado_Nacional/Evolucion_por_candidato/",
                         partidoo))
      
    
  }
  
  graph.titulo <- paste0("Evolución de los comparativos actualizado a las ",
                         format_hora(hora),
                         " hrs")
  
  graph.evolution.todos <- historia %>%  
    ggplot() + 
    geom_ribbon(aes(x = hora,
                    ymin = inf,
                    ymax = sup,
                    fill = Estimacion,
                    group = Estimacion),
                alpha = 0.4) +
    geom_path(aes(x = hora,
                  y = med,
                  color = Estimacion,
                  group = Estimacion),
              linetype = 2) +
    facet_wrap(~partido, 
               ncol = 1,
               scales = "free_y") +
    labs(title = graph.titulo,
         x = "Hora de la remesa",
         y = "Proporción estimada de votos en favor",
         fill = "Estimación",
         color = "Estimación") +
    scale_y_continuous(labels = porcentaje) +
    scale_x_discrete(labels = format_hora)
  
  
  graph.name <- paste0("EvolucionTodos_", dia, hora, ".png")
  
  ggsave(filename = graph.name,
         path = paste0(path.buzon,
                       "Grafs_Compulsado_Nacional/Evolucion_todos/"))
  
  
  graph.evolution.todos.aux <- historia %>%  
    ggplot() + 
    geom_ribbon(aes(x = hora,
                    ymin = inf,
                    ymax = sup,
                    fill = Estimacion,
                    group = Estimacion),
                alpha = 0.4) +
    geom_path(aes(x = hora,
                  y = med,
                  color = Estimacion,
                  group = Estimacion),
              linetype = 2) +
    facet_wrap(~partido, 
               ncol = 1,
               scales = "free_y") +
    labs(title = paste0("Actualizado a las ", format_hora(hora), " hrs"),
         subtitle = "Evolución de los comparativos",
         x = "Hora de la remesa",
         y = "Proporción estimada de votos en favor",
         fill = "Estimación",
         color = "Estimación") +
    scale_y_continuous(labels = porcentaje) +
    scale_x_discrete(labels = format_hora) +
    theme(plot.title = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "none")
  
  
  # conjunta <- ggplot2.multiplot(graph.evolution.todos.aux,
  #                               graph.puntual.aux,
  #                               cols = 2)
  
  conjunta <- grid.arrange(graph.evolution.todos.aux,
                           graph.puntual.aux,
                           nrow = 1)
  
  ggsave(filename = paste0(dia, hora, "_resumen_global.png"),
         plot = conjunta,
         path = paste0(path.buzon,
                       "Grafs_Compulsado_Nacional/"))
  
}





 


# Proceso iterativo ####
anteriores.nacional <- "1300"
repeat{
  
  ### ### ### ### ### Análisis Nacional
  # Determina si hay análisis nacionales a compulsar
  fecha <- determina_nacional(path.buzon.nacional = path.buzon.nacional,
                              anteriores = anteriores.nacional)
  
  # Si no los hay espera 5 segundos
  # Si los hay, compulsa, actualiza anteriores
  # y espera 5 segundos
  if(fecha[1] == FALSE){
    Sys.sleep(time = 5)
  }else{
    compulsa_nacional(dia = fecha[1],
                      hora = fecha[2],
                      path.buzon.nacional = path.buzon.nacional,
                      path.compulsado.nacional = path.compulsado.nacional,
                      path.buzon = path.buzon)
    anteriores.nacional <- c(anteriores.nacional, fecha[2])
    mensaje <- paste0("Compulsado nacional de las ",
                      format_hora(fecha[2]),
                      " hrs realizado exitosamente")
    print(mensaje)
    
    Sys.sleep(time = 5)
  }
  ### ### ### ### ###
}







