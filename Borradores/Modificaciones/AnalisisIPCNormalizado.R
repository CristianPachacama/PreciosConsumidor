library(readr)
# library(forecast)
library(TSdist)
library(TSclust)
library(smacof)
library(dygraphs)
getwd()
IPChist = read_csv("Data/IPChistorico.csv")

# Modificación de IPC (Normalización /IPC General)
# IPCaux= IPChist
# for(i in 2:dim(IPChist)[1]){
#   IPCaux[i,-(1:2)] = IPCaux[i,-(1:2)]/IPCaux[1,-(1:2)]
# }

#Transponer Datos
IPC = t(as.matrix(IPChist[,c(-1,-2)]))
# IPCx = t(as.matrix(IPCaux[,c(-1,-2)]))
IPCx = as.matrix(diff(ts(IPC, start = c(2005, 1),frequency = 12)))

#Función que realiza todo el Análisis -------------------------
EstudioIPC = function(IPC,IPCx,metrica=17,clusters=7,
                      p=5,window.size=5,g=2,sigma=2,epsilon=0.5,tau=2.5,w=3,h=4){
  
  Items = IPChist$ITEM
  items = as.vector(abbreviate(IPChist$ITEM))
  colnames(IPC) = items
  IPC = ts(IPC, start = c(2005, 1),frequency = 12)
  # dim(IPC)
  
  # Matriz de Distancia -----------------
  # metricasList = c("euclidean", "manhattan", "minkowski", 
  #                  "infnorm", "ccor", "sts", "dtw", "lb.keogh", 
  #                  "edr", "erp", "lcss", "fourier", "tquest", 
  #                  "dissim", "acf", "pacf", "ar.lpc.ceps", 
  #                  "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", 
  #                  "ar.pic", "cdm", "cid", "cor", "cort", "wav", 
  #                  "int.per", "per", "mindist.sax", "ncd", "pred", 
  #                  "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet")
  metricasList = c("1.Euclidean distance"="euclidean", 
                   "2.Manhattan distance"="manhattan", 
                   "3.Minkowski distance"="minkowski", 
                   "4.Infinite norm distance"="infnorm", 
                   "5.Distance based on the cross-correlation"="ccor", 
                   "6.Short time series distance"="sts", 
                   "7.Dynamic Time Warping distance"="dtw", 
                   "8.LB_Keogh lower bound for the Dynamic Time Warping distance"="lb.keogh", 
                   "9.Edit distance for real sequences"="edr", 
                   "10.Edit distance with real penalty"="erp",
                   "11.Longest Common Subsequence Matching"="lcss", 
                   "12.Distance based on the Fourier Discrete Transform"="fourier", 
                   "13.TQuest distance"="tquest", 
                   "14.Dissim distance"="dissim", 
                   "15.Autocorrelation-based dissimilarity"="acf", 
                   "16.Partial autocorrelation-based dissimilarity"="pacf", 
                   "17.Dissimilarity based on LPC cepstral coefficients"="ar.lpc.ceps", 
                   "18.Model-based dissimilarity proposed by Maharaj"="ar.mah", 
                   "ar.mah.statistic", 
                   "ar.mah.pvalue", 
                   "21.Model-based dissimilarity measure proposed by Piccolo"="ar.pic", 
                   "22.Compression-based dissimilarity measure"="cdm", 
                   "23.Complexity-invariant distance measure"="cid", 
                   "24.Dissimilarities based on Pearson's correlation"="cor", 
                   "25.Dissimilarity index which combines temporal correlation and raw value behaviors"="cort", 
                   "26.Dissimilarity based on wavelet feature extraction"="wav", 
                   "27.Integrated periodogram based dissimilarity"="int.per", 
                   "28.Periodogram based dissimilarity"="per", 
                   "29.Symbolic Aggregate Aproximation based dissimilarity"="mindist.sax", 
                   "30.Normalized compression based distance"="ncd", 
                   "31.Dissimilarity measure cased on nonparametric forecasts"="pred", 
                   "32.Dissimilarity based on the generalized likelihood ratio test"="spec.glk", 
                   "33.Dissimilarity based on the integrated squared difference between the log-spectra"="spec.isd", 
                   "34.General spectral dissimilarity measure using local-linear estimation of the log-spectra"="spec.llr", 
                   "35.Permutation Distribution Distance"="pdc", 
                   "36.Frechet distance"="frechet")
  matriz_dist = function(IPC,metrica=metricasList[1]){
    
    D = matrix(data=rep(NA,116^2),nrow=116,ncol = 116)
    for(i in 1:116){
      for(j in 1:116){
        if(i<j){
          if(i==3){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,p)
          }
          if(i==8){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,window.size)
          }
          if(i==10){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,g,sigma)
          }
          if(i==11){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,epsilon,sigma)
          }
          if(i==13){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,tau)
          }
          if(i==29){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,w)
          }
          if(i==31){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,h)
          }else{
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica)
          }
          
          D[j,i]=D[i,j]
        }
      }
      D[i,i]=0
      
    }
    return(D)
  }
  #Metrica de Atocorrelacion: 15,16
  #Metricas ARMA: 17 - 21
  m=metrica
  D=matriz_dist(IPCx,metricasList[m])
  
  #nube = smacofSym(D)
  #plot(nube)
  
  #Clusterizacion CLARA ----------------------
  Clustering = function(D,N=5){
    
    clus.fit = clara(D, k=N, samples = 50, stand=TRUE, pamLike = TRUE)
    #plot(clus.fit)
    cl =list()
    for(i in 1:N){
      cl[[i]] = which(clus.fit$clustering==i)
    }
    return(cl)
  }
  
  N = clusters
  cl = Clustering(D,N)
  
  # Gráfico del IPC por Producto(Item)  ---------------------
  graf_series = function(IPC,items,ind,metric,cluster){
    
    aux = IPC[,items[ind]]
    
    dygraph(aux, main = paste("IPC (Base 2004):","Cluster",cluster),group = 'itemsG')%>%
      dyAxis("x", label=names(metric)) %>%
      dyAxis('y',label='IPC (Base 2004)')%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(show= "follow",width = 400)
    
  }
  # Guardar Grafico de Series  ------------------------
  resultado = list()
  for(i in 1:N){
    resultado[[i]] = graf_series(IPC,items,cl[[i]],metricasList[m],cluster = i)
  }
  
  return(resultado)
}

#--------------------------------------------------------------
metricasList = c("1.Euclidean distance"="euclidean", 
                 "2.Manhattan distance"="manhattan", 
                 "3.Minkowski distance"="minkowski", 
                 "4.Infinite norm distance"="infnorm", 
                 "5.Distance based on the cross-correlation"="ccor", 
                 "6.Short time series distance"="sts", 
                 "7.Dynamic Time Warping distance"="dtw", 
                 "8.LB_Keogh lower bound for the Dynamic Time Warping distance"="lb.keogh", 
                 "9.Edit distance for real sequences"="edr", 
                 "10.Edit distance with real penalty"="erp",
                 "11.Longest Common Subsequence Matching"="lcss", 
                 "12.Distance based on the Fourier Discrete Transform"="fourier", 
                 "13.TQuest distance"="tquest", 
                 "14.Dissim distance"="dissim", 
                 "15.Autocorrelation-based dissimilarity"="acf", 
                 "16.Partial autocorrelation-based dissimilarity"="pacf", 
                 "17.Dissimilarity based on LPC cepstral coefficients"="ar.lpc.ceps", 
                 "18.Model-based dissimilarity proposed by Maharaj"="ar.mah", 
                 "ar.mah.statistic", 
                 "ar.mah.pvalue", 
                 "21.Model-based dissimilarity measure proposed by Piccolo"="ar.pic", 
                 "22.Compression-based dissimilarity measure"="cdm", 
                 "23.Complexity-invariant distance measure"="cid", 
                 "24.Dissimilarities based on Pearson's correlation"="cor", 
                 "25.Dissimilarity index which combines temporal correlation and raw value behaviors"="cort", 
                 "26.Dissimilarity based on wavelet feature extraction"="wav", 
                 "27.Integrated periodogram based dissimilarity"="int.per", 
                 "28.Periodogram based dissimilarity"="per", 
                 "29.Symbolic Aggregate Aproximation based dissimilarity"="mindist.sax", 
                 "30.Normalized compression based distance"="ncd", 
                 "31.Dissimilarity measure cased on nonparametric forecasts"="pred", 
                 "32.Dissimilarity based on the generalized likelihood ratio test"="spec.glk", 
                 "33.Dissimilarity based on the integrated squared difference between the log-spectra"="spec.isd", 
                 "34.General spectral dissimilarity measure using local-linear estimation of the log-spectra"="spec.llr", 
                 "35.Permutation Distribution Distance"="pdc", 
                 "36.Frechet distance"="frechet")
# Guardamos Analisis del IPC :---------------------------------
N=10
(m=which(metricasList=="cort"))
Analisis = EstudioIPC(IPC,IPC,metrica = m,clusters=N)
#Mostramos analisis
for(i in 1:N){
  print(Analisis[[i]])
}



