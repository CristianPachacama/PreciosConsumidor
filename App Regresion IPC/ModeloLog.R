library(readr)
library(readxl)
library(dplyr)

#Datos de IPC ---------------------------------------
IPChist = read_csv("Data/IPChistorico.csv")
IPC = t(as.matrix(IPChist[,c(-1,-2)]))
Items = IPChist$ITEM
items = as.vector(abbreviate(IPChist$ITEM))
colnames(IPC) = items

IPC = data.frame(Fecha=rownames(IPC),IPC)

#Datos de GINI ---------------------------------------
gini = as.data.frame(read_csv("Data/GINI.csv"))
Gini = rep(NA, dim(IPChist)[2]-2)
for(i in 3:dim(gini)[1]){
  Gini[12*(i-3)+1] = gini[i,2] 
}
Gini = data.frame(Fecha = names(IPChist)[-c(1,2)], GINI = Gini)

#Interpolar datos NAs en Serie GINI
library(zoo)
Gini= Gini %>%  mutate(GiniAp = na.approx(GINI,na.rm = F))
ind = min(which(is.na(Gini$GiniAp)))
Gini[ind:(dim(Gini)[1]),"GiniAp"] = Gini[ind-1,"GiniAp"]
Gini = Gini[,-2]

#Consolidamos en una sola Serie
BDD = Gini %>% inner_join(IPC, by="Fecha")

#Aniadimos Identificador para le tiempo "t" (para la regresion)
Tmp = data.frame(Fecha = BDD$Fecha, Tiempo=1:dim(BDD)[1])
BDD = Tmp %>% inner_join(BDD,by="Fecha")
remove(Tmp,gini,IPChist)

#Regresion IPC(Producto) - GINI  -------------------------------

#General
modelo = lm(log(GENE)~log(Tiempo)+log(GiniAp), data= BDD)
summary(modelo)




#Grafico de Series --------------------------------------------
library(dygraphs)
graf_series = function(BDDts,ind){
  
  aux = BDDts[,ind]
  
  dygraph(aux, main = "Grafico Conjunto",group = 'grupoIndices')%>%
    dyAxis('y',label='Series')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(show= "follow",width = 400)
  
}

BDDts = ts(BDD[,-1], start = c(2005, 1),frequency = 12)

graf_series(BDDts,1:3)





