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
stlx = stlplus(BDDts3[,2],n.p= 12,s.window = 12, t.window = 1)
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


mav(BDDts3[,2], n =12 )
hist()














