rm(list = ls())
library(dplyr)
library(readxl)
library(tidyr)
library(readr)


# RUTAS -----
setwd("~/2024-06-09")
ruta_proyecto <- getwd()
ruta_europeas <- paste0(ruta_proyecto,"/todo/")
ruta_claves_provincias <- paste0(ruta_europeas, "Claves_Provincias.csv")

rutas_archivos <- list.files(path= ruta_europeas, pattern="AU_202.CSV",full.names = T )
ruta_guardado <- paste0(ruta_proyecto,"/Resultados_Europeas.csv")

## eliminamos uno que sobra
rutas_archivos <- rutas_archivos[!grepl("007TOT99AU_202.CSV", rutas_archivos)]

# FUNCIONES ---

leer_y_combinar_csvs <- function(rutas_archivos) {
  # Leer todos los archivos CSV en una lista de dataframes
  lista_dataframes <- lapply(rutas_archivos, read.csv, header = F, sep = ";", dec = ".")
  
  # Combinar todos los dataframes en uno solo
  dataframe_combinado <- bind_rows(lista_dataframes)
  
  return(dataframe_combinado)
}

arreglar_datos_europeas <- function(datos){
  # Inicializar columnas para los resultados de los partidos
  datos <- datos %>%
    mutate(Votos_FO_N = NA, Votos_FO_perc = NA)
  
  # Iterar sobre las filas del DataFrame para encontrar las columnas de cada partido
  for (i in 1:nrow(datos)) {
    row <- datos[i, ]
    cols <- colnames(row)
    for (j in seq(3, length(cols), by = 4)) {
      
      
      # Votos FO  
      if (!is.na(row[[j]]) &  grepl(pattern = "FO", x = row[[j]]) ) {
        datos[i, 'Votos_FO_N'] <- row[[j + 1]]
        datos[i, 'Votos_FO_perc'] <- row[[j + 2]]
      }

      
    }
  }
  
  # Creamos los ID's
  datos <- datos %>%
    rename(
      ID_0 = V1, ID_1 = V2, codigoProvincia = V3, codigoMunicipio = V5, 
      Municipio = V7,
      Participacion_N = V14, Participacion_perc = V15,
      Abstencion_N = V16, Abstencion_perc = V17,
      Blancos_N = V18, Blancos_perc = V19,
      Nulos_N = V20, Nulos_perc = V21,
      ) %>%
    mutate(
      codigoProvincia = sprintf("%03d", codigoProvincia),
      codigoMunicipio = sprintf("%03d", codigoMunicipio),
      Identificador = paste0(ID_1, "_", codigoProvincia, "_", codigoMunicipio),
      Comunidad_autonoma = case_when(
        ID_1 == 1 ~ "Andalucía",
        ID_1 == 2 ~ "Aragón",
        ID_1 == 3 ~ "Principado de Asturias",
        ID_1 == 4 ~ "Illes Balears",
        ID_1 == 5 ~ "Canarias",
        ID_1 == 6 ~ "Cantabria",
        ID_1 == 7 ~ "Castilla-La Mancha",
        ID_1 == 8 ~ "Castilla y León",
        ID_1 == 9 ~ "Cataluña",
        ID_1 == 10 ~ "Extremadura",
        ID_1 == 11 ~ "Galicia",
        ID_1 == 12 ~ "Comunidad de Madrid",
        ID_1 == 13 ~ "Comunidad Foral de Navarra",
        ID_1 == 14 ~ "País Vasco",
        ID_1 == 15 ~ "Región de Murcia",
        ID_1 == 16 ~ "La Rioja",
        ID_1 == 17 ~ "Comunitat Valenciana",
        ID_1 == 18 ~ "Ciudad Autónoma de Ceuta",
        ID_1 == 19 ~ "Ciudad Autónoma de Melilla"
      ),
      
      # Creamos variable censo municipio
      Censo_N = Participacion_N + Abstencion_N + Nulos_N + Blancos_N, 
      # Arreglar porcentages
      Participacion_perc = Participacion_perc*0.01,
      Abstencion_perc = Abstencion_perc*0.01,
      Blancos_perc = Blancos_perc*0.01,
      Nulos_perc = Nulos_perc*0.01,
      Votos_FO_perc = Votos_FO_perc*0.01
      
    )
  
  # Filtramos lo que nos interesa
  datos <- datos %>% select(Identificador, ID_0, ID_1, codigoProvincia, codigoMunicipio,
                            Comunidad_autonoma, Municipio, 
                            Participacion_N, Participacion_perc, Abstencion_N, Abstencion_perc,
                            Nulos_N, Nulos_perc, Blancos_N, Blancos_perc, 
                            Censo_N, 
                            Votos_FO_N, Votos_FO_perc) 
  
  return(datos)
}

# EJECUCION -----
DATOS_SPAIN <- leer_y_combinar_csvs(rutas_archivos)

resultados <- arreglar_datos_europeas(DATOS_SPAIN)

# IDENTIFICADORES DE PROVINCIA -----
equivalencias_provincias <- read.csv(file = ruta_claves_provincias, header = T, sep = ";")
equivalencias_provincias <- equivalencias_provincias %>% mutate(ID_provincia = sprintf("%03d", ID_provincia)) # arreglo

resultados <- resultados %>% left_join(equivalencias_provincias, by = c("codigoProvincia"="ID_provincia"))

resultados <- resultados %>% select(Identificador, ID_0, ID_1, codigoProvincia, codigoMunicipio, Comunidad_autonoma, Provincia, everything())

# GUARDADO ------
# write.csv(x = resultados, file = ruta_guardado,sep = "|", dec = ".")
write.table(x = resultados, file = ruta_guardado, sep = "|", dec = ".")

# End session ---
rm(list=ls())
gc()
