# library(mFilter)
library(quantmod)
library(stlplus)
# .libPaths()
library(xts)
library(smooth)
library(Mcomp)
library(pracma)

source(file = "Code/FiltroHodrickPrescott.R")
source(file = "Code/ModeloLog.R")

#Filtro Hodrick Prescott   -------------------------------
hpGen = hpfilter(BDDts3[,1])
plot(hpGen)

hpProd =  ts(NA, start = c(2005, 1), end = c(2018,5),frequency = 12)
k = 1
for(i in 2:dim(BDDts3)[2]){
  hpProd[,k] =  BDDts3[,i]/hpGen$trend
  k = k+1
}

#Filtro STL-Loess (Plus)   --------------------------------
stlx = stlplus(BDDts3[,2], s.window = 12, t.window = 12)
plot(stlx)


# Medias Moviles 5 (Centradas)   --------------------------
mav = function(x,n=5){
  
  mvx = stats::filter(x,rep(1/n,n), sides=2)
  resx = x - mvx
  
  par(mfrow=c(2,1))
  
  plot(x, main="Serie de Tiempo")
  lines(mvx , col="red")
  plot(resx,ylab="Residuos",main="Residuos")
  
  return(data.frame(x,mvx))
}


mav12 = mav(BDDts3[,2] , n = 12 )
mav9 = mav(BDDts3[,2] , n = 9 )
mav5 = mav(BDDts3[,2] , n = 5 )



# Escribir en Archivos
write.csv(hpGen$trend ,file="Analizado/DescomposicionMedMov/hpGen.csv")

write.csv(mav12,file="Analizado/DescomposicionMedMov/mav12.csv")
write.csv(mav9,file="Analizado/DescomposicionMedMov/mav9.csv")
write.csv(mav5,file="Analizado/DescomposicionMedMov/mav5.csv")










