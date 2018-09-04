library(readr)
library(forecast)
library(TSdist)
library(TSclust)
library(smacof)
library(dygraphs)


IPChist <- read_csv("Data/IPChistorico.csv")

IPC = t(as.matrix(IPChist[,c(-1,-2)]))
Items = IPChist$ITEM
items = as.vector(abbreviate(IPChist$ITEM))
colnames(IPC) = items
IPC = ts(IPC, start = c(2005, 1),frequency = 12)
dim(IPC)

plot(IPC[,101:110])

# Gráfico del IPC por Producto(Item)  ---------------------
#ind=1:6

graf_series = function(IPC,items,ind){
  
  aux = IPC[,items[ind]]
  
  dygraph(aux, main = "IPC (Base 2004)",group = 'itemsG')%>%
    dyAxis('y',label='IPC (Base 2004)')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(show= "follow",width = 400)
  
}

# Analiis Cluster   ---------------------------------------
analisis_cluster = function(IPC,N,m){
  
  # Matriz de Distancia -----------------
  metricasList = c("euclidean", "manhattan", "minkowski", 
                   "infnorm", "ccor", "sts", "dtw", "keogh.lb", 
                   "edr", "erp", "lcss", "fourier", "tquest", 
                   "dissim", "acf", "pacf", "ar.lpc.ceps", 
                   "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", 
                   "ar.pic", "cdm", "cid", "cor", "cort", "wav", 
                   "int.per", "per", "mindist.sax", "ncd", "pred", 
                   "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet")
  matriz_dist = function(metrica=metricasList[1]){
    
    D = matrix(data=rep(NA,116^2),nrow=116,ncol = 116)
    for(i in 1:116){
      for(j in 1:116){
        if(i<j){
          D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica)
          D[j,i]=D[i,j]
        }
      }
      D[i,i]=0
      
    }
    return(D)
  }
  
  #m=1
  D=matriz_dist(metricasList[m])
  
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
  
  #N=7
  cl = Clustering(D,N)
  # Grafico de Series  ------------------------
  for(i in 1:N){
    print(graf_series(IPC,items,cl[[i]]))
  }
  
}

analisis_cluster(IPC,N=4,m=9)
