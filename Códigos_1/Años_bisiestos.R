setwd("C:/Users/alekz/OneDrive/Escritorio/Tecnicas_Final_Agrupado/Oficial/Años")

getwd()

library(metR)
library(ncdf4)
library(ggplot2)
library(nortest)
library(ggstatsplot)
library(ggside)
library(plotly)

# Lista de nombres de los archivos CSV
archivos <- c("ho00000172.csv", "ho00000211.csv", "ho00000278.csv", "ho00000279.csv",
              "ho00000280.csv", "ho00000281.csv", "ho00000310.csv", "ho00000322.csv", "ho00000386.csv")

# Bucle para procesar cada archivo
for (archivo in archivos) {
  # Leer el archivo CSV en un dataframe
  df <- read.csv(archivo)
  
  # Convertir la columna de fechas a formato de fecha
  df$Fecha <- as.Date(paste0(df$year,"-",df$month,"-",df$day))
  
  # Identificar los años bisiestos
  es_bisiesto <- leap_year(year(df$Fecha))
  
  # Filtrar los registros que no correspondan al 29 de febrero en años bisiestos
  df_sin_29_febrero <- df[!(month(df$Fecha) == 2 & day(df$Fecha) == 29 & es_bisiesto), ]
  
  # Guardar el nuevo dataframe sin el 29 de febrero en un nuevo archivo CSV
  nuevo_archivo <- paste0("sin_29_febrero_", archivo)
  write.csv(df_sin_29_febrero, nuevo_archivo, row.names = FALSE)
}
