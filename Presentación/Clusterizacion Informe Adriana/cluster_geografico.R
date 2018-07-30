# Paquetes Necesarios
library(dygraphs)
library(plotly)
library(TSclust)
library(TSdist)
library(smacof)
library(cluster)
library(xts)
library(factoextra)
setwd("C:/Users/TOSHIBA/Desktop/Profit-Proyecto Semilla")
load('Vazoes_Profit.RData')

# save(D_acf,D_pacf,D_euc,D_manh,D_mink,D_ifnrm,
#      D_ccor,D_ccor2,D_cor,D_cort,D_per,D_fourier,
#      map1,map2,map3,map4,map5,map6,map15,map16,map13,
#      vazoes_profit_ts,vazoes_profit,vazoes_dat,vaz_code,
#      file = 'Vazoes_Profit.RData')

#=========================== K- MEDIAS (kmeans) =============================
D<-D_ccor
k_aux<-4

#Cluster Óptimo   --------------
map<-smacofSym(D)
op<-fviz_nbclust(map$conf, kmeans, method = "silhouette")
#op
ggplotly(op)

#Clustering con Kmeans
fit_kmeans <- kmeans(map$conf, centers = k_aux)

grupo <- fit_kmeans$cluster




#===========================   K-MEDIODS (PAM-CLARA)  =============================
D<-D_ccor
k_aux<-4

#Cluster Óptimo   --------------
map<-smacofSym(D)
op<-fviz_nbclust(map$conf, pam, method = "silhouette")
#op
ggplotly(op)

#Clustering con CLARA
fit_clara <- clara(map$conf, k=k_aux, samples = 50, stand=TRUE, pamLike = TRUE)

grupo <- fit_clara$cluster

#data_clust <- cbind(map$conf, cluster = fit_clara$cluster)

#Gráfico Nube de Puntos ----

g_pam<-fviz_cluster(list(data = map$conf, cluster = grupo),labels_cex=0.3,
             palette = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00","#283747"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

ggplotly(g_pam)

#===========================   CLUSTER GERARQUICO  ================================

#Dendograma -------
clus<-hclust(D)

dn<-fviz_dend(clus, k = k_aux, # Cut in 5 groups
              cex = 0.6, horiz=F,# label size
              k_colors = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00","#283747"),
              color_labels_by_k = TRUE, # color labels by groups
              rect = TRUE # Add rectangle around groups
)
dn

grupo <- cutree(clus, k = k_aux)

#Nube de puntos ---------
g_gerar<-fviz_cluster(list(data = map$conf, cluster = grupo),labels_cex=0.3,
                palette = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00","#283747"),
                ellipse.type = "convex", # Concentration ellipse
                repel = FALSE, # Avoid label overplotting (slow)
                show.clust.cent = FALSE, ggtheme = theme_minimal())

#p
ggplotly(g_gerar)


# =================  GRAFICO DE SERIES POR CLUSTER  ============================ 
#excl<-which(is.na(vazoes_code$Latitud))
D1<-as.numeric(map$conf[,'D1']);#D1<-D1[-excl]
D2<-as.numeric(map$conf[,'D2']);#D2<-D2[-excl]

BDD_profit<-data.frame(Nombre_ST = paste0('VAZOES(',vazoes_code$Estacion,')'),
                       Estacion=vazoes_code$Estacion,
                       Codigo_ONS=vazoes_code$Codigo_ONS,
                       Latitud=vazoes_code$Latitud,
                       Longitud=vazoes_code$Longitud,
                       D1, D2, Cluster=as.factor(as.character(grup))
)

#BDD_profit<-BDD_profit[-excl,]

# Grafico de Series por Cluster ---------
est_cluster1<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==1])
est_cluster2<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==2])
est_cluster3<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==3])
est_cluster4<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==4])
est_cluster5<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==5])
est_cluster6<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==6])

aux_vaz_dy<-vazoes_profit_ts

names(aux_vaz_dy)<-names(vazoes_profit)[-1]

#Series Cluster1

dygraph(aux_vaz_dy[,est_cluster1[1:5]], main = "Series del Cluster 1",group = 'grupo_clus')%>%
  dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')

#Series Cluster2
dygraph(aux_vaz_dy[,est_cluster2[1:5]], main = "Series del Cluster 2",group = 'grupo_clus')%>%
  dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')







#======================================================================================
#======================   Funcion Automatiza Clusterizacion   =========================
#======================================================================================

library(dygraphs)
library(plotly)
library(TSclust)
library(TSdist)
library(smacof)
library(cluster)
library(xts)
library(factoextra)
load('Vazoes_Cluster.RData')
#----------------------------------------------------------
# save(D_acf,D_pacf,D_euc,D_manh,D_mink,D_ifnrm,
#      D_ccor,D_ccor2,D_cor,D_cort,D_per,D_fourier,
#      vazoes_cluster_ts,vazoes_cluster,vazoes_dat,
#      vazoes_code,cluster_geografico,
#      file = 'Vazoes_Cluster.RData')
#----------------------------------------------------------

cluster_geografico <- function(D=D_ccor,k=4,tipo='kmedias',vazoes_code){
  k_aux<-k
  map <- smacofSym(D)
  fit <- switch(tipo,
                'kmedias' = {kmeans(map$conf, centers = k_aux)},
                'clara' = {clara(map$conf, k=k_aux, samples = 50, stand=TRUE, pamLike = TRUE)},
                'gerarquico' = {hclust(D)}
  )
  grupo <- switch (tipo,
                   'kmedias' = {fit$cluster},
                   'clara' = {fit$cluster},
                   'gerarquico' = {cutree(fit, k = k_aux)}
  )
  D1<-as.numeric(map$conf[,'D1']);
  D2<-as.numeric(map$conf[,'D2']);
  
  BDD_cluster<-data.frame(Nombre_ST = paste0('VAZOES(',vazoes_code$Estacion,')'),
                         Estacion=vazoes_code$Estacion,
                         Codigo_ONS=vazoes_code$Codigo_ONS,
                         Latitud=vazoes_code$Latitud,
                         Longitud=vazoes_code$Longitud,
                         D1, D2, Cluster=as.factor(as.character(grupo))
  )
  if(k_aux >= 1){
    est_cluster1<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==1])
  }
  if(k_aux >=2){
    est_cluster2<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==2])
  }
  if(k_aux >=3){
    est_cluster3<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==3])
  }
  if(k_aux >=4){
    est_cluster4<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==4])
  }
  if(k_aux >=5){
    est_cluster5<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==5])
  }
  if(k_aux >=6){
    est_cluster6<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==6])
  }
  if(k_aux >=7){
    est_cluster7<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==7])
  }
  if(k_aux>=8){
    est_cluster8<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==8])
  }
  if(k_aux > 8){
    print('Valor de k demasiado grande, elige un menor número de clusters')
  }
  
  resultado <- switch (k_aux,
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6,
                             'est_cluster7' = est_cluster7)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6,
                             'est_cluster7' = est_cluster7,'est_cluster8' = est_cluster8)}
  )
  
  return(resultado)
  
}

aux_cluster <- cluster_geografico(D=D_ccor,k=8,tipo='clara')

# aux_cluster <- cluster_geografico(D=D_ccor,k=7,tipo='kmedias')
# aux_cluster <- cluster_geografico(D=D_ccor,k=7,tipo='gerarquico')

# ------ Grafica de nube de puntos ---------
map<-aux_cluster$map
grupo<-aux_cluster$grupo
fviz_cluster(list(data = map$conf, cluster = grupo),labels_cex=0.3,
             palette = c("#00AFBB","#f39c12","#009900", "#9b59b6",
                         "#000000","#C0392B","#909497","#154360"),
             ellipse.type = "convex", # Concentration ellipse
             repel = FALSE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# ----- Grafica de Series por Cluster ------
dygraph(vazoes_cluster_ts[,aux_cluster$est_cluster1[1:5]], main = "Series del Cluster 1",group = 'grupo_clus')%>%
  dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')
dygraph(vazoes_cluster_ts[,aux_cluster$est_cluster2[1:5]], main = "Series del Cluster 1",group = 'grupo_clus')%>%
  dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')




# -------------------------------------------



#=======================   MAPA Clusterizado   =========================
library(leaflet)

BDD_cluster <- aux_cluster$BDD_cluster

#Modificar color y estilo de Marcadores para estaciones
getColor_clus <- function(BDD_cluster) {  
  #La funcion crea un vector con colores basado en una condicion
  sapply(BDD_cluster$Cluster, function(Cluster) {
    if(Cluster == 1) {
      "blue"
    } else if(Cluster == 2) {
      "orange"
    } else if(Cluster == 3) {
      "green"
    } else if(Cluster == 4) {
      'purple'
    } else if(Cluster == 5) {
      "gray"
    } else if(Cluster == 6) {
      "red"
    } else if(Cluster == 7) {
      "lightgray"
    } else if(Cluster == 8) {
      "darkblue"
    } else{
      NA
    }
    
    })
}

icons_clus <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor_clus(BDD_cluster) 
  #Vector que contiene los colores de cada marcador
)


#Generar Mapa  ------------------------------------------------
library(htmltools)

burbuja_clust <- paste(sep = "<br/>",
                       "<b><a href='-'>Tipo: </a></b>Vazoes",
                       paste0("<b><a href='-'>Estación: </a></b>", BDD_cluster$Estacion),
                       paste0("<b><a href='-'>Longitud: </a></b>", BDD_cluster$Longitud),
                       paste0("<b><a href='-'>Latitud: </a></b>" ,BDD_cluster$Latitud),
                       paste0("<b><a href='-'>Cluster: </a></b>" , BDD_cluster$Cluster)
                 )



mapa <- leaflet() %>% addTiles() %>%
  
  #Marcadores: 
  addAwesomeMarkers(lng=BDD_cluster$Longitud,lat=BDD_cluster$Latitud,#group ='Clima',
                    icon=icons_clus,popup=burbuja_clust)%>%
  #Añadir Minimapa
  addMiniMap(position='bottomright',toggleDisplay = TRUE)

mapa
