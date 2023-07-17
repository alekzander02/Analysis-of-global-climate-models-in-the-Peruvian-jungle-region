###Estableciendo directorio de trabajo
setwd("C:/Users/alekz/OneDrive/Escritorio/Tecnicas_Final_Agrupado/Nuevo Datos/Pisco2/Tmax")

getwd()

library(metR)
library(ncdf4)
library(ggplot2)
library(nortest)
library(ggstatsplot)
library(ggside)
library(plotly)


###Listar archivos de modelos para la variable de pp
archivos <- list.files(pattern = ".*tasmax.*historical.*\\.nc$")
archivos

###Extrayendo nombres de modelos de los modelos listados
Name_model <- substr(archivos,12,22)
Name_model

###Importando el archivo est.txt generado al homogeneizar nuestros datos en bruto
obs <- read.table("est.txt")
obs
colnames(obs) <- c("file","lon","lat","alt","code","name")
obs

obs$file <- sub('txt','csv',obs$file) #cambia la fila file de txt a csv
obs

obs$lon = obs$lon + 360
obs

GlanceNetCDF(archivos[3])


#####Lectura de modelos listados anteriormente
mod_sta <- list()
for (i in 1:length(obs$file)){
  ini <-NULL
  for (j in 1:length(archivos)){
    model <- ReadNetCDF(archivos[j], vars ="tasmax", subset=list(lat=obs$lat[i],lon=obs$lon[i], time =c("1981-01-02", "2014-01-01")))
    if (j ==1){
      ini <- cbind(ini,model)
    }else{
      ##########Cambiar pr (precipitacion) por tasmax o tasmin dependiendo de la variable a trabajar
    ini <- cbind(ini,model$tasmax)
    }
  }
  colnames(ini)[4:ncol(ini)]<- Name_model
  ini$time <- as.Date(ini$time)
  ini$`MRI-ESM2-0_`  = ini$`MRI-ESM2-0_`-273
  ini$`NorESM2-LM_` = ini$`NorESM2-LM_`-273
  ini$`UKESM1-0-LL` = ini$`UKESM1-0-LL`-273
  mod_sta[[i]] <- ini
  write.csv(mod_sta[[i]], file = paste0("model_hist_tmax_",obs$name[i],".csv"), row.names = FALSE)
  
}

mod_sta

#####Lectura de estaciones
sta <- list()
filt_station <-list()
for (i in 1: length(obs$file)){
  sta[[i]] <- read.csv(obs$file[i])
  sta[[i]]$Fecha <- as.Date(paste0(sta[[i]]$year,"-",sta[[i]]$month,"-",sta[[i]]$day))
  filt_station[[i]] <- sta[[i]][sta[[i]]$Fecha >= min(mod_sta[[i]]$time) & sta[[i]]$Fecha <= max(mod_sta[[i]]$time),]
  
  ###Cambiar pp por tmax o tmin
  write.csv(filt_station[[i]], file = paste0("obs_filt_hist_tmax_",obs$name[i],".csv"), row.names = FALSE)
}

colnames(sta[[1]])
filt_station


####Union de modelos y observado en dataframes
all_model <- list()
for (i in 1:length(obs$file)){
  all_model[[i]] <- data.frame(Fecha = filt_station[[i]]$Fecha,mod_sta[[i]][,4:length(colnames(mod_sta[[i]]))], Observado= filt_station[[i]]$Tmax) #### Pp, esto se cambia por la variable a trabajar sea Tmin o Tmax
}
#######################Lista con los dataframe con los datos del modelo y observado de cada estacion
all_model

###############################################################
#####Comparativa serie diaria

library(reshape2)

for (i in 1:length(obs$file)){
  graph <- ggplot(melt(all_model[[i]], id.vars = "Fecha",variable.name = "Fuente"), aes(x = Fecha, y = value)) +
    geom_line(aes(col=Fuente)) +
    facet_wrap(~ Fuente, ncol = 1, scales = "free_y")+
    
    
    ##########OJO aqui modifcar los colores con la cantidad de estaciones
    ##########En este caso son 7 , si teien mas estaciones agregar mas colores
    
    
    scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                  "cyan4", "indianred", "magenta"))+ggtitle(paste0("Estación ",obs$name[i]," T° maxima 1981 - 2013"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),
          strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
    scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
    theme(axis.title = element_text(face = "bold"))
  
  ggsave(plot=graph,file=paste0("serie_hist_mod_obs_",obs$name[i],".png"), width = 9, height = 9)
  inte <- ggplotly(graph)
  htmlwidgets::saveWidget(inte,paste0("serie_hist_mod_obs_",obs$name[i],"_i_.html"))
}



#install.packages("GGally")
library(GGally)

#Aplicamos Spearman de frente ya que funciona para datos normales o no 
#####Correlacion
for (i in 1:length(obs$file)){
  cor_plot <- ggcorr(all_model[[i]][2:length(colnames(all_model[[i]]))], label = TRUE,method= c("all.obs", "spearman"))+ggtitle(paste0("Coef. Correlación de Spearman - T° maxima Estación: ",obs$name[i]))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  ggsave(plot=cor_plot,file=paste0("cor_plot_tmax_",obs$name[i],".png"), width = 7, height = 6)
  
}


##################Calculo del RMSE para todas las estaciones
#install.packages("Metrics")
library(Metrics)

RMSE <- matrix(nrow = length(obs$file), ncol = length(Name_model))
library(Metrics)
for (i in 1:length(obs$file)) {
  for (j in 1:length(Name_model)) {
    res <- rmse(all_model[[i]]$Observado, all_model[[i]][, j+1])
    RMSE[i, j] <- res
  }
}
colnames(RMSE) <- Name_model
rownames(RMSE) <- obs$name
RMSE
write.csv(RMSE, file = paste0("RMSE_hist_tmax_.csv"), row.names = T)

##################Calculo del BIAS para todas las estaciones
BIAS <- matrix(nrow = length(obs$file), ncol = length(Name_model))
library(Metrics)
for (i in 1:length(obs$file)) {
  for (j in 1:length(Name_model)) {
    res2 <- bias(all_model[[i]]$Observado, all_model[[i]][, j+1])
    BIAS[i, j] <- res2
  }
}
colnames(BIAS) <- Name_model
rownames(BIAS) <- obs$name
BIAS
write.csv(BIAS, file = paste0("BIAS_hist_tmax_.csv"), row.names = T)


##################Calculo del  MAE para todas las estaciones
MAE <- matrix(nrow = length(obs$file), ncol = length(Name_model))
library(Metrics)
for (i in 1:length(obs$file)) {
  for (j in 1:length(Name_model)) {
    res2 <- mae(all_model[[i]]$Observado, all_model[[i]][, j+1])
    MAE[i, j] <- res2
  }
}
colnames(MAE) <- Name_model
rownames(MAE) <- obs$name
MAE
write.csv(MAE, file = paste0("MAE_hist_tmax_.csv"), row.names = T)




























