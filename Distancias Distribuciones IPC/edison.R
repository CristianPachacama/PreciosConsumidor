resumen = data.frame(round(xtable(summary(modelo1)),digits = 5))
names(resumen) = c("Estimación","Error Estándar","t-valor","Pr(>|t|)")
Pval = as.numeric(summary(modelo1)$coefficients[,4])
rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
             labels = c("***","**","*","."," "))
resumen$Signif = rangos

bethas[x,y]=function(resumen){
  x=startsWith(rownames(resumen),"Tmp")
  
  resumentmp=resumen[x,]
  y=confint.lm(modelo1)[x,]
  (y[1,1]-y[1,2])/4
  (y[1,1]+resumentmp[1,1])/resumentmp[1,3]
}

tablaperiodo=function(N,periodos){
  etiquetas = c()
  etiquetas1 = c()
  for (i in 1:(length(periodos) - 1)) {
    etiquetas[i] = paste("Periodo: ", periodos[i], " - ", periodos[i + 1])
    etiquetas1=c(etiquetas1,replicate(N,etiquetas[i]))
  }
  return(etiquetas1)
}
periodos = c(2010,2011,2012,2013)
simulador=function(N,mu,sig){
  x=rnorm(N,mu,sig)
  return(x)
}

Betasdensidad=function(Betas,N){
  x=c()
  for (i in 1:length(Betas)) {
    x=c(x,simulador(n,betas$mu[i],Betas$sig[i]))
  }
  y=tablaperiodo(N,periodos)
  x= data.frame(round(xtable(cbind(x,y)),digits = 5))
  return(x)
}
