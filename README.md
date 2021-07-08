# Informe-de-tenis-

Este proyecto es un informe dinámico (indiferente a al torneo seleccionado) sobre campeonatos de tenis, que contiene lo siguiente:
Efectividad del saque
Lider bajo presion
¿Existe una relación entre la Efectividad del Saque y ganar el partido?
¿Existe una relación entre la cantidad de WNR y UFE cuando el ganador no se ceden
ningún set del partido (FNL=0)?
Por cada uno de los jugadores:
Promedio de efectividad al saque.
Promedio de winners por partido.
Promedio de errores no forzados por partido.
Porcentaje de Break Point convertidos.
Porcentaje de Break Point salvados.
Porcentajes de puntos ganados en la red.
Promedio de puntos por partido.
En base a las variables creadas por jugador, determina que característica caracteriza
al campeón respecto a los restantes semifinalistas.

rm(list =ls())

#Buscar carpeta por defecto
getwd()

#Cambiar directorio
setwd('C:/Users/asus/Documents/ORT/1er modulo/Introd prog analitica/Obligatorio/Bases de Datos-20200601')


#BD que se va a trabajar
ArchivoData <- c("AusOpen-men-2014.csv")

#Cargar Base de Datos
data <- read.csv(ArchivoData)

#Nombre del Torneo
Campeonato <- substr(ArchivoData,1,str_locate_all(ArchivoData,'-')[[1]][1,1]-1)
Genero<- substr(ArchivoData,str_locate_all(ArchivoData,'-')[[1]][1,1]+1,
                str_locate_all(ArchivoData,'-')[[1]][2,2]-1)
Ano<- substr(ArchivoData,str_locate_all(ArchivoData,'-')[[1]][2,2]+1,
             str_locate(ArchivoData,'.csv')[[1]]-1)
if(Campeonato == 'AusOpen'){
  Campeonato <- 'Australian Open'
} else if (Campeonato == 'FrenchOpen'){
  Campeonato <- 'Torneo de Roland Garros'
} else if (Campeonato == 'USOpen'){
  Campeonato <- 'US Open'
}


#Cargar Librerias
library(tidyverse)
library("ggplot2")
library(plotly)
install.packages(DT)
library(DT)
#install.packages("tables")
#library(tables)

#Verificar que todas las BD tengan el mismo nombre - error
#View(data)

#Agregar Id del Partido
data$id <-c(1:nrow(data)) 

# Borrar NA y verificar si está ok

data <- data %>% 
  mutate_all(funs(replace(.,is.na(.),0)))

apply(is.na(data), 2, sum)

#Agregar variables para Efectividad del saque
data <- data %>% 
  mutate(ES.1=FSP.1+FSW.1+SSW.1+ACE.1-DBF.1, 
         ES.2=FSP.2+FSW.2+SSW.2+ACE.2-DBF.2) 

#Agregar variables para Breack point salvados y convertidos
data <- data %>% 
  mutate(BPA.1= BPC.2, 
         BPA.2= BPC.1, 
         BPS.1= BPC.2 - BPW.2,
         BPS.2= BPC.1 - BPW.1) 


#Crear df Auxiliares de los jugador 1 y 2
Jugadores1 <- data %>%
                select (c(id,Player.1,ROUND,Result,FNL.1,ES.1,WNR.1,UFE.1,BPC.1,BPW.1,BPA.1,BPS.1,NPA.1,NPW.1,TPW.1))

Jugadores2 <- data %>%
                select (c(id,Player.2,ROUND,Result,FNL.2,ES.2,WNR.2,UFE.2,BPC.2,BPW.2,BPA.2,BPS.2,NPA.2,NPW.2,TPW.2))


#Modificar el resultado para los jugadores 2. Valor = 0 - Pierde / 1 - Gana
for (i in 1:nrow(Jugadores2)) {
  if (Jugadores2$Result[i] == 0 ) {
    Jugadores2$Result[i] = 1
  } else {
    Jugadores2$Result[i] = 0
  }}

###Combinar los df auxiliares de Jugadores 1 y 2

#Para combinar las tablas, deben llamarse igual, se modifican los nombres
colnames(Jugadores1)<- gsub('.1$','',colnames(Jugadores1))

colnames(Jugadores2)<- gsub('.2$','',colnames(Jugadores2))

#Comprobar que los nombres de las dos variables son iguales
colnames(Jugadores2) == colnames(Jugadores1)


#Crear un df para todos los jugadores
JugadoresTot <- rbind(Jugadores1,Jugadores2)

head (JugadoresTot)

################Semifinalistas################


Semifinalistas<- JugadoresTot %>%
  select(Player, ROUND, Result) %>%
  filter(ROUND== max(ROUND-1))

paste(Semifinalistas$Player, "semifinalista", sep=" es " )




################Finalistas################

#Finalistas

Finalistas<- JugadoresTot %>%
  select(Player, ROUND, Result) %>%
  filter(ROUND== max(ROUND))

Finalistas$Player<- as.character(Finalistas$Player)

paste(Finalistas$Player, "finalista",sep= " es ")


fig <- plot_ly(  
  type = "table",
  header = list(
    values = c("PLAYER", "ROUND"),
    line = list(color = '#506784'), #color linea de cuadro cabezal
    fill = list(color = '#467'), #Camio el color titulo, el cudro
    align = c('center','center'),
    font = list(color = 'white', size = 12)), #color y tamaño de letra titulo
  cells = list(
    values = rbind (Finalistas$Player, Finalistas$ROUND),
    line = list(color = '#506784'), #linea parte de abajo del cuadro
    fill = list(color = c('#CCC591', 'white')), #colores cuadros parte abajo por siquieren cambiar uno
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12))) #color y tamaño fuente de parte de abajo del cuadro

fig


# otra forma

Figurafinalistas<- Finalistas %>%
  select("Player")%>%
  datatable()

Figurafinalistas  


################Ganador################


Ganador<- JugadoresTot %>%
  select(Player, ROUND, Result) %>%
  filter(ROUND== max(ROUND), Result==1)
  
paste("El ganador del campeonato es", Ganador$Player )


###efectividad del saque por jugador por partdo#####


ESaux <- JugadoresTot %>% 
  select(Player,ES) %>%
  arrange(desc(ES))
 
   

ES_grafica <- ESaux[1:10,] %>%
  ggplot(aes(x=reorder(Player,ES), y=ES )) + 
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Jugadores con mayor ES")
  
 

ES_grafica


#Lider bajo presion LBP=%BPS+%BPC+WNR+UFE########


LBP <-
  JugadoresTot %>%
   mutate(PBPC = BPW/BPC*100,
         PBPS = BPS/BPC*100,
         LBP  = PBPS+PBPC+WNR+UFE) %>%
print

LBPaux<- LBP %>%
    select(Player, ROUND, LBP)%>%
    arrange(desc(LBP))%>%
    

#expresar informacion

paste("El jugador con mayor valor de indicador LBP  es", )


#####Existe relacion entre efetividad del saque y ganar el partido?####3

grafico_caja <- JugadoresTot %>%
                  ggplot(aes(x=factor(Result),y=ES, fill=Result))+
                  geom_boxplot()+
                  labs(title =" Relación ES y Ganar el partido ", x = "Resultado", y = "ES")+
                  theme(legend.position = 'none')
                  

grafico_caja



####### Existe una relacion entre WNR (Puntos ganados por el jugador) y UFE (Errores no forzados) cuando el ganador no se ceden ningun set del partido (FNL=0).


grafico_WNRVsUFE <- JugadoresTot %>%
                  filter (FNL == 0) %>%
                  ggplot(aes(x=WNR, y=UFE))+
                  geom_point()+
                  geom_smooth(method="lm", se=FALSE)+
                  ggtitle =(" Relación WNR y UFE con FNL=O")
                  
                  
grafico_WNRVsUFE
                  

######Promedio Efectividad del Saque por cada jugador ##### 

ES_Jugador <- JugadoresTot %>%
  group_by(Player) %>%
  summarise (Promedio=mean(ES)) %>%
  arrange(desc(Promedio))

#histograma

EFS_grafica <- ES_Jugador[1:10,] %>%
  ggplot(aes(x=Player, y=Promedio)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Prom efectividad del saque por jugador")
 

 
EFS_grafica


#######Promedio de winners por partido######3

WNR_partido <-  JugadoresTot %>%
  group_by(Player, ROUND) %>%
  select(Player, ROUND, WNR)%>%
  summarise(Promedio= mean(WNR))%>%
  arrange(desc(Promedio))


#Histograma
WNR_grafica <- WNR_partido[1:10,] %>%
  ggplot(aes(x=Player, y=Promedio)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Promedio winners por partido")

WNR_grafica


#####Promedio Errores no forzados por jugador por partido#####

UFE_partido<- JugadoresTot %>%
  group_by(Player,ROUND) %>%
  select(Player, ROUND, UFE)%>%
  summarise (Promedio= mean(UFE))%>%
  arrange(desc(Promedio))

UFE_grafica <- UFE_partido[1:10,] %>%
    ggplot(aes(x=Player, y=Promedio)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ggtitle("Promedio errores no forzados")
  
UFE_grafica


#Porcentaje de Break Point convertidos

PBPC <- LBP %>%
  group_by(Player) %>%
  summarise (Promedio= sum(PBPC)) %>%
  arrange(desc(Promedio))

PBPC_grafica <- PBPC[1:10, ] %>%
  ggplot(aes(x=Player,y=Promedio))+
  geom_bar(stat='identity')+
  coord_flip() +
  ggtitle("Porcentaje de Break Point convertidos")

PBPC_grafica


#Porcentaje de Break Point Salvados


PBPS <- LBP %>%
  group_by(Player) %>%
  summarise (Promedio = sum(PBPS)) %>%
  arrange(desc(Promedio))

PBPS_grafica <- PBPS[1:10, ] %>%
  ggplot(aes(x=Player,y=Promedio))+
  geom_bar(stat='identity')+
  coord_flip()+
  ggtitle('Porcentaje de Break Point Salvados')

PBPS_grafica

#Porcentajes de puntos ganados en la red


Ptosganred<- JugadoresTot %>%
  group_by(Player) %>%
  filter(NPA!=0)%>%
  summarise (sum(NPW)/length(NPA)) %>% #ver este
  print

names(Ptosganred)[2]="promedio"

MaxNPW<- Ptosganred %>%
  select(Player, promedio) %>%
  filter(promedio== max(promedio))


paste("El jugador con mayor porcentaje de puntos ganados en red es", MaxNPW$Player )

### grafica####



######Promedio de puntos por partido#####


PromPtosPartido<- JugadoresTot %>%
  group_by(Player) %>%
  summarise (sum(TPW)/length(ROUND)) %>%
  print

names(PromPtosPartido)[2]="promedio"

Maximoant<- PromPtosPartido %>%
  select(Player, promedio) %>%
  filter(promedio== max(promedio))
  

paste("El jugador con mayor promedio de puntos por partido es", Maximoant$Player )

