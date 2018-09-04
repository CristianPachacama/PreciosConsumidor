library(readr)
library(readxl)
library(dplyr)
library(zoo)

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
remove(i)
Gini = data.frame(Fecha = names(IPChist)[-c(1,2)], GINI = Gini)

#Interpolar datos NAs en Serie GINI

Gini= Gini %>%  mutate(GiniAp = na.approx(GINI,na.rm = F))
ind = min(which(is.na(Gini$GiniAp)))
Gini[ind:(dim(Gini)[1]),"GiniAp"] = Gini[ind-1,"GiniAp"] ; remove(ind);
Gini = Gini[,-2]

#Aniadimos Identificador para le tiempo "t" (para la regresion)
Tmp = data.frame(Fecha = Gini$Fecha, Tiempo=1:dim(Gini)[1])

#Datos del PIB  ------------------------------------------------
Pib_Trim = read_csv("Data/PIB_Trimestral.csv")
Pib = rep(Pib_Trim$PibTrim[Pib_Trim$Anio>=2005]/3,rep(3, sum(Pib_Trim$Anio>=2005) ))

#Extrapolamos los 2 ultimos valores del Pib (que no estan disponibles)
#Solo repetimos el ultimo
PIB = data.frame(Fecha = Gini$Fecha,Pib= c(Pib,rep(Pib[length(Pib)],2) ) )

remove(Tmp,gini,IPChist,Pib_Trim,Pib)


#Consolidamos en una sola BASE (Por producto) ------------------
a=c()
b=c()
pval=c()
names(IPC)=c('Fecha',items)
for(k in 1:116){
  
  producto=k
  IPCaux = IPC[,c(1,producto+1)]
  BDD = Tmp %>% inner_join(Gini) %>% 
    inner_join(PIB) %>% inner_join(IPCaux)
  
  #Dummy para el año 2015 (Comienzo de Deflación) ---------------
  # DummCrisis = rep(1,dim(IPCaux)[1])
  # DummCrisis[1:121]=0
  
  # BDD$Dummy = factor(DummCrisis, levels = c("No","Si"))
  
  #Regresion IPC(Producto) - GINI  -------------------------------
  
  #Modelo por producto
  modelo = lm( log(eval(as.name(items[producto])) )~log(Tiempo), data= BDD)
  print(names(IPCaux)[2])
  resumen = (summary(modelo))
  
  a[k] = modelo$coefficients[1]
  b[k] = modelo$coefficients[2]
  pval[k] = resumen$coefficients[2,4]
  
}


tabla = data.frame(Items,a,b,pval)
Prank = cut(tabla$pval,breaks = c(0,0.001,0.01,0.05,0.1,1),labels = c("***","**","*","."," ") )

tabla$Prank = Prank

tabla = ListItems %>% inner_join(tabla)

#Cuadro de rangos del coeficiente "b" de la regresion
# rangos = matrix(NA,nrow = 2,ncol = 2)
b = tabla$b

ind1 = b>=0 & abs(b)>=1
rangos1 = tabla[ind1,]

ind2 = b<0 & abs(b)>=1
rangos2 = tabla[ind2,]

ind3 = b>=0 & abs(b)<1
rangos3 = tabla[ind3,]

ind4 = b<0 & abs(b)<1
rangos4 = tabla[ind4,]

rangos4

write.csv(rangos3,file = "Analizado/Rango3.csv")
write.csv(rangos4,file = "Analizado/Rango4.csv")



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

IPCaux3 = IPC[,c("Fecha",as.character(rangos3$Abreviatura))]
BDD3 = Tmp %>% #inner_join(Gini) %>% 
  inner_join(IPCaux3) #%>% inner_join(PIB)


IPCaux4 = IPC[,c("Fecha",as.character(rangos4$Abreviatura))]
BDD4 = Tmp %>% #inner_join(Gini) %>% 
  inner_join(IPCaux4) #%>%  inner_join(PIB)
  


BDDts3 = ts(BDD3[,-(1:3)], start = c(2005, 1),frequency = 12)
BDDts4 = ts(BDD4[,-(1:3)], start = c(2005, 1),frequency = 12)


graf_series(BDDts3)
graf_series(BDDts4)




