library(dygraphs)
library(dplyr)
library(readxl)
library(zoo)

load("Data/DatosAux.RData")
h=1

nivel = c('Nacional'=1,
          'Región Sierra'=2,
          'Región Costa'=3,
          'Guayaquil'=4,
          'Esmeraldas'=5,
          'Machala'=6,
          'Manta'=7,
          'Santo Domingo'=8,
          'Quito'=9,
          'Loja'=10,
          'Cuenca'=11,
          'Ambato'=12)

IPChist = read_excel("Data/ipc_ind_nac.xls",sheet = as.numeric(nivel[h]), skip = 4)
IPChist = IPChist[,-dim(IPChist)[2]]

Nombres[2] = "Producto"
names(IPChist) = Nombres
IPChist = IPChist[-c(1,dim(IPChist)[1] , dim(IPChist)[1]-1) , ]
IPCprod = inner_join(Productos,IPChist)
#Datos de IPC ---------------------------------------
# IPChist = read_csv("Data/IPChistorico.csv")
IPC = t(as.matrix(IPCprod[,-c(1:3)]))
Items = IPCprod$Producto
# Items = IPChist$ITEM
items = as.character(IPCprod$Abrebiatura)

ListItems = data.frame(Productos = Items , Abreviaturas = items)
colnames(IPC) = items

IPC = data.frame(Fecha=rownames(IPC),IPC)

#Aniadimos Identificador para le tiempo "t" (para la regresion)
Tmp = data.frame(Fecha = IPC$Fecha, Tiempo=1:length(IPC$Fecha))

#Consolidamos en una sola BASE (Por producto) ------------------
a=c()
b=c()
pval=c()
names(IPC)=c('Fecha',items)

#!!!!!!!!!!!!!!!!!!!!!!
IPC = IPC[118:130,]

for(k in 1:118){
  
  producto=k
  IPCaux = IPC[,c(1,producto+1)]
  BDD = Tmp %>% inner_join(IPCaux)
 
  #Regresion IPC(Producto) - GINI  -------------------------------
  
  #Modelo por producto
  modelo = lm( log(eval(as.name(items[producto])) )~log(Tiempo), data= BDD)
  print(names(IPCaux)[2])
  resumen = (summary(modelo))
  
  a[k] = modelo$coefficients[1]
  b[k] = modelo$coefficients[2]
  pval[k] = resumen$coefficients[2,4]
  
}


tabla = data.frame(Productos = Items,a,b,pval)
Prank = cut(tabla$pval,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*","."," ") )

tabla$Prank = Prank

tabla = ListItems %>% inner_join(tabla) %>% arrange(desc(b))


#Grafico de Series --------------------------------------------
library(dygraphs)
graf_series = function(BDDts){
  
  aux = BDDts
  
  dygraph(aux, main = "Grafico Conjunto",group = 'grupoIndices')%>%
    dyAxis('y',label='Series')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(show= "follow",width = 400)
  
}

# Por grupo

names(IPC) = c("Fecha",items)

IPCSaltos = IPC[,c("Fecha",as.character(tabla$Abreviaturas[1:15]))]
BDDSaltos = Tmp %>% #inner_join(Gini) %>% 
  inner_join(IPCSaltos) #%>% inner_join(PIB)

TBDDSaltos = ts(BDDSaltos[,-(1:3)], start = c(2014, 10),frequency = 12)
graf_series(TBDDSaltos)

sort(as.character(tabla$Productos[1:15]))




