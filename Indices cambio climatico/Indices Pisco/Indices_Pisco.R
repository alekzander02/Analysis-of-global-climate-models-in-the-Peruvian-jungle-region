##Directorio de trabajo
setwd("C:/Users/alekz/OneDrive/Escritorio/Tecnicas_Final_Agrupado/Var_PISCO")

getwd()

######Creación de ficheros de entrada

##Lectura de series de climatol
pre <- read.table("pre_1981-2013_series.csv",sep=",",header=T)
tmax <- read.table("tmax_1981-2013_series.csv",sep=",",header=T)
tmin <- read.table("tmin_1981-2013_series.csv",sep=",",header=T)
View(tmax)

##Extraccion de año, mes y dia
year <- as.numeric(format(as.Date(tmax$Fecha),'%Y'))
month <- as.numeric(format(as.Date(tmax$Fecha),'%m'))
day <- as.numeric(format(as.Date(tmax$Fecha),'%d'))

##Viendo que series tenian datos y fuero homogeneizadas
#1 al 10 por el codigo de la estacion 
unique(substr(colnames(pre[,-1]),1,10))

unique(substr(colnames(tmax[,-1]),1,10))

unique(substr(colnames(tmin[,-1]),1,10))

##Seleccionando las series homogeneizadas que cumplan con la condicion +% de datos originales, menor SNHT, menor RMSE

##Reemplazar por estaciones del grupo
pre <- data.frame(pre[,-1])

tmax <- data.frame(tmax[,-1])

tmin <- data.frame(tmin[,-1])


##Generando fichero por estacion

archivo <- substr(list.files(pattern = "^ho000.*\\.txt$"),1,10)

estacion<- list()
for (i in 1:length(archivo)){
  estacion[[i]] <- data.frame(year,month,day,Pp=pre[,i],Tmax=tmax[,i],Tmin=tmin[,i])
#Exportando fichero
write.table(estacion[[i]],file =paste0(archivo[i],".csv"),sep = ",",row.names=F)
}



##################################
########################### Cálculo de índices climáticos


##Lectura de archivo
data <- estacion[[1]]   #####ho00000503, estacion[[2]] seria h00000541 y asi seguria

##Estblecienco nombred de columnas
colnames(data) <- c("Año","Mes","Dia","Pp","Tmax","Tmin")

##Columna de fechas
data$Fecha <- as.Date(paste0(data$Año,"-",data$Mes,"-",data$Dia))



head(data,10)


##############Indices climaticos

#####Calculo "manual"
##cdd: Mayor número de días secos consecutivos PP < 1 mm por año
cddf <- NULL

for (j in min(data$Año):max(data$Año)){
  dsec <- 0
  cdd2 <- 0
  #Recorre la longitud de la columna Pp por año
  for(i in 1:length(data$Pp[data$Año == j])) {
    #Condicional ¿El dia es seco?
    if(!is.na(data$Pp[data$Año == j][i]) && data$Pp[data$Año == j][i] < 1) {
      # Agrega dias secos
      dsec <- dsec+ 1
      # Comparacion de secuencias, queda el mas largo
      if(dsec > cdd2) {
        cdd2 <- dsec
      }
      #¿Dia no seco? entonces reinica el contador
      #Recordar que cdd2 va mantener la mayor cantidad de dias secos
    } else {
      dsec <- 0
    }
  }
  #Agrupar por año
  df <- data.frame(Año = j,CDD = cdd2)
  cddf <- rbind(cddf,df)
}

cddf

#Graficado
# Serie de tiempo
plot(cddf$Año,cddf$CDD,type="l",col="green",main="Estacion CDD .....")

library(ggplot2)
library(plotly)

# serie de tiempo: ggplot
g_cdd <- ggplot(cddf,aes(x=Año,y=CDD))+
  geom_line(col="green",lwd=0.7)+
  theme_light()+ggtitle("Indice CDD Estacion ..............")+
  scale_x_continuous(breaks = seq(1965,2019,2))+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g_cdd) 

# boxplot 
box_cdd <- ggplot(cddf,aes(y=CDD))+ geom_boxplot()
ggplotly(box_cdd)                                                                               

#........................


#############################################
##################Paquete RClimDex

###Instalación directa no funciona
###install.packages("RClimDex")

#install.packages(devtools)
library(devtools)

##Instalando RClimdex desde github
##devtools::install_github("ECCC-CDAS/RClimDex")

library(RClimDex)
library(PCICt)


##Inicializando RClimdex (Shiny app)
rclimdex.start()




####################Paquete climdex.pcic

#install.packages("climdex.pcic")
library(climdex.pcic)

#############Estableciendo fechas
Fecha <- as.character(data$Fecha)

Fecha <- as.PCICt(Fecha, cal="365_day")

#########Creando fichero interno de datos para climdex.pcic

ci <- climdexInput.raw(data$Tmax,
                       data$Tmin, data$Pp,
                       Fecha,Fecha,Fecha, base.range=c(1965, 2019),
                       northern.hemisphere = F)

#########################Cálculo de todos los índices

##cdd: Mayor número de días secos consecutivos PP < 1 mm por año
cdd <- climdex.cdd(ci)
cddf <- data.frame(cdd)
cddf$Year <- 1965:2019


ggplot(data=cddf,aes(x= Year,y= cdd))+
  coord_cartesian()+
  geom_line(aes(x= Year,y= cdd),col="green")+
  theme_light()+ggtitle("Indice ... Estacion ..............")+
  theme(plot.title = element_text(hjust = 0.5))


##prcptot: Precipitación total anual  
prcptot <- climdex.prcptot(ci)
prcptotf <- data.frame(prcptot)

##Mayor número de días húmedos consecutivos PP >=1 mm por año

cwd <- climdex.cwd(ci)
cwdf <- data.frame(cwd)

##Rango de temperatura mensual
dtr <- climdex.dtr(ci)
dtr <- data.frame(dtr)

##Numero de dias de heladas anual
fd <- climdex.fd(ci)
fd <- data.frame(fd)

##Duración de la temporada de crecimiento
gsl <- climdex.gsl(ci)
gsl <- data.frame(gsl)

##..........................
id <- climdex.id(ci)
id <- data.frame(id)

r10mm <- climdex.r10mm(ci) 
r10mm <- data.frame(r10mm)

r20mm  <- climdex.r20mm(ci) 
r20mm <- data.frame(r20mm)

r95ptot <- climdex.r95ptot(ci)
r95ptot <- data.frame(r95ptot)

r99ptot <- climdex.r99ptot(ci)
r99ptot <- data.frame(r99ptot)

rnnmm <- climdex.rnnmm(ci)
rnnmm <- data.frame(rnnmm)

rx1day <- climdex.rx1day(ci)
rx1day <- data.frame(rx1day)

rx5day <- climdex.rx5day(ci)
rx5day <- data.frame(rx5day)

sdii <- climdex.sdii(ci)
sdii <- data.frame(sdii)

su <- climdex.su(ci)
su <- data.frame(su)

tn10p <- climdex.tn10p(ci)
tn10p <- data.frame(tn10p)

tn90p <- climdex.tn90p(ci)
tn90p <- data.frame(tn90p)

tnn <- climdex.tnn(ci)
tnn <- data.frame(tnn)

tnx <- climdex.tnx(ci)
tnx <- data.frame(tnx)

tr <- climdex.tr(ci)
tr <- data.frame(tr)

tx10p <- climdex.tx10p(ci)
tx10p <- data.frame(tx10p)

tx90p <- climdex.tx90p(ci)
tx90p <- data.frame(tx90p)

txn <- climdex.txn(ci)
txn <- data.frame(txn)

txx <- climdex.txx(ci)
txx <- data.frame(txx)

wsdi <- climdex.wsdi(ci)
wsdi <- data.frame(wsdi)

csdi <- climdex.csdi(ci)
csdi <- data.frame(csdi)




#########################Metodo completo


####Si es que los hiciste manualmente o en otro lenguaje:
####Si es que solo sigues la secuencia obvia esto

files <- c("ho00000172.csv","ho00000211.csv","ho00000278.csv", "ho00000279.csv","ho00000280.csv","ho00000281.csv","ho00000310.csv", "ho00000322.csv", "ho00000386.csv")
estacion <- list()
for (k in 1:9){
  estacion[[k]] <-read.csv(files[[k]],sep = ",")
}
estacion[1]

library(climdex.pcic)
#install.packages("reshape2")
library(reshape2)
#install.packages("htmlwidgets")
library(htmlwidgets)
#install.packages("plotly")
library(plotly)
#############Hasta aqui





####Continua

#Aqui ponemos todas las funciones disponibles de climdex.pcic
funciones <- list("climdex.cdd", "climdex.prcptot", "climdex.cwd", "climdex.dtr", "climdex.fd", "climdex.gsl", "climdex.id", "climdex.r10mm",
                  "climdex.r20mm", "climdex.r95ptot", "climdex.r99ptot", "climdex.rnnmm", "climdex.rx1day", "climdex.rx5day", "climdex.sdii",
                  "climdex.su", "climdex.tn10p", "climdex.tn90p", "climdex.tnn", "climdex.tnx", "climdex.tr", "climdex.tx10p", "climdex.tx90p",
                  "climdex.txn", "climdex.txx", "climdex.wsdi", "climdex.csdi")

library(ggplot2)

for (j in 1:length(funciones)){
  #Objeto nulo como inicializador
  index_df <- NULL
  for (i in 1:9){ 
    
    #Nombres de columnas
    colnames(estacion[[i]]) <- c("Año","Mes","Dia","Pp","Tmax","Tmin")
    
    ##Vector de fechas
    Fecha <- as.Date(paste0(estacion[[i]]$Año,"-",estacion[[i]]$Mes,"-",estacion[[i]]$Dia))
    
    #install.packages("climdex.pcic")
    library(climdex.pcic)
    
    #############Convirtiendo a formato PCIC 
    Fecha_pcic <- as.character(Fecha)
    
    Fecha_pcic <- as.PCICt(Fecha_pcic, cal="365_day")
    #########Creando fichero interno de datos para climdex.pcic
    ci <- climdexInput.raw(as.numeric(estacion[[i]]$Tmax),
                           as.numeric(estacion[[i]]$Tmin),as.numeric(estacion[[i]]$Pp),
                           Fecha_pcic,Fecha_pcic,Fecha_pcic, base.range=c(1981, 2013),
                           northern.hemisphere = F)
    
    #Aplicando do.callc (llama a una funcion como string)
    index <- do.call(funciones[[j]], list(ci))
    
    #Agrupando cada archivo de cada estacion
    index_df <- cbind(index_df,index)
    
    index_df2 <- data.frame(index_df)
    
    #Si es la ultima estacion aplicar lo demas
    if (i==length(estacion)){
      colnames(index_df2) <-  c("Tamshiyacu","Saposoa","San_Ramon","Nauta","Requena","Genaro_Herrera","El_Porvenir", "Tabalosos", "Navarro")
      ##Si los resultados son anuales
      if (nrow(index_df2 > 55)){
        #Fechas para el nuevo dataframe
        Fecha <- as.Date(paste0(rownames(index_df2),"-07-16"))
        index_df2 <- data.frame(Fecha = Fecha,index_df2)
        
        #Creando titulo y eje Y como mayusculas
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        #melt agrupa todas las estaciones en una sola columna
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 1, scales = "free_y")+
          scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                        "cyan4", "indianred", "magenta"))+ ylab(toupper(substr(funciones[[1]],9,nchar(funciones[[1]]))))+ggtitle(paste0("Índice ",Name_index," 1981 - 2013"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))
        
        ggsave(plot=graph,file=paste0(funciones[[j]],".png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"_i_.html"))
        
        #En el caso que sean mensuales
      }else{
        Fecha <- as.Date(paste0(rownames(index_df2),"-16"))
        index_df2 <- data.frame(Fecha= Fecha,index_df2)
        
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 1, scales = "free_y")+
          scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                        "cyan4", "indianred", "magenta"))+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," 1981 - 2013"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))
        
        ggsave(plot=graph,file=paste0(funciones[[j]],".png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"_i_.html"))
      }
      
    }
    
    
    write.csv(index_df2 , file = paste0(funciones[[j]],".csv"), row.names = FALSE)
    
  }
} 





# Crear los subplots con facet_wrap()
graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
  geom_line(aes(col=Estacion)) +
  facet_wrap(~ Estacion, ncol = 1, scales = "free_y")+
  scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                "cyan4", "indianred", "magenta"))+ ylab(toupper(substr(funciones[[1]],9,nchar(funciones[[1]]))))+ggtitle(paste0("Índice ",Name_index," 1981 - 2013"))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title = element_text(face = "bold"))

#¿Que hace melt?
library(reshape2)
data <- data.frame(Fecha=1:20,A=sample(1:100,20),B=sample(1:100,20),C=sample(1:100,20))
melt(data, id.vars = "Fecha",variable.name = "Estacion")
