library(xts)
#devtools::install_github('hadley/ggplot2')

load("C:/Users/TOSHIBA/Desktop/SharePoint/ADRIANA UQUILLAS ANDRADE/Proyecto Semilla Energia Streamflow/Datos/BDD_descargadas/BDD_Diarias/1_BASES CONSOLIDADAS/Unificada/BDD_unificada(actualizada).RData")

series <- names(BDD_unificada)[startsWith(names(BDD_unificada),'VAZO')]
excp1<-which(series=='VAZOES(CACHOEIRA CALDEIRAO (204))')
excp2<-which(series=='VAZOES(FERREIRA GOMES (297))')
#excp3<-which(series=='VAZOES(XINGO (178))')

#series<-series[-c(excp1,excp2,excp3)]
series<-series[-c(excp1,excp2)]

aux <- BDD_unificada[,c('Fecha',series)]
fechas <- (as.character(aux$Fecha)>='2005-01-01')&(as.character(aux$Fecha)<='2015-12-31')


aux <- aux[fechas,]
vazoes_profit<-aux
aux_ts <- xts(aux[,-1],order.by = as.Date(aux[,1]))
vazoes_profit_ts<-aux_ts
sum(is.na(aux_ts))
#save(vazoes_profit_ts,vazoes_profit,file = 'Vazoes_Profit.RData')

#------------------ CODIFICACION ESTACIONES ---------------------
nomb<-sort(as.character(vazoes_dat$Estacion))
nomb<-nomb[-c(excp1,excp2)]
nomb<-data.frame(Estacion=nomb)
vaz_code<-merge(vazoes_dat,nomb,by = "Estacion")

# ----------------ANÁLISIS CON METRICAS-------------------------------------------
library(TSclust)
library(TSdist)
library(smacof)
library(xts)

setwd("C:/Users/TOSHIBA/Desktop/Profit-Proyecto Semilla")
load('Vazoes_Profit.RData')

names(vazoes_profit_ts)<-as.character(vazoes_code$Codigo_ONS)

D_acf<-TSDatabaseDistances(vazoes_profit_ts,distance="acf")
D_pacf<-TSDatabaseDistances(vazoes_profit_ts,distance="pacf")
D_euc<-TSDatabaseDistances(vazoes_profit_ts,distance="euclidean")
D_manh<-TSDatabaseDistances(vazoes_profit_ts,distance="manhattan")
D_mink<-TSDatabaseDistances(vazoes_profit_ts,distance="minkowski",p=3)
D_ifnrm<-TSDatabaseDistances(vazoes_profit_ts,distance="infnorm")
D_ccor<-TSDatabaseDistances(vazoes_profit_ts,distance="ccor")
D_ccor2<-TSDatabaseDistances(vazoes_profit_ts,distance="ccor",lag.max=365)


D_sts<-TSDatabaseDistances(vazoes_profit_ts,distance="sts")
D_dtw<-TSDatabaseDistances(vazoes_profit_ts,distance="dtw")

D_edr<-TSDatabaseDistances(vazoes_profit_ts,distance="edr")
D_keogh<-TSDatabaseDistances(vazoes_profit_ts,distance="lb.keogh",window.size=365)
D_erp<-TSDatabaseDistances(vazoes_profit_ts,distance="erp")
D_lcss<-TSDatabaseDistances(vazoes_profit_ts,distance="lcss")
D_fourier<-TSDatabaseDistances(vazoes_profit_ts,distance="fourier")
D_tquest<-TSDatabaseDistances(vazoes_profit_ts,distance="tquest")
D_dissim<-TSDatabaseDistances(vazoes_profit_ts,distance="dissim")
D_ar_ceps<-TSDatabaseDistances(vazoes_profit_ts,distance="ar.lpc.ceps")
D_ar_mah<-TSDatabaseDistances(vazoes_profit_ts,distance="ar.mah")
D_ar_mah_stat<-TSDatabaseDistances(vazoes_profit_ts,distance="ar.mah.statistic")

D_cor<-TSDatabaseDistances(vazoes_profit_ts,distance="cor")
D_cort<-TSDatabaseDistances(vazoes_profit_ts,distance="cort")
D_wav<-TSDatabaseDistances(vazoes_profit_ts,distance="wav")
D_frechet<-TSDatabaseDistances(vazoes_profit_ts,distance="frechet")
D_pred<-TSDatabaseDistances(vazoes_profit_ts,distance="pred")
D_int_per<-TSDatabaseDistances(vazoes_profit_ts,distance="int.per")
D_per<-TSDatabaseDistances(vazoes_profit_ts,distance="per")

D_spec_llr<-TSDatabaseDistances(vazoes_profit_ts,distance="spec.llr")
D_spec_isd<-TSDatabaseDistances(vazoes_profit_ts,distance="spec.isd")
D_spec_glk<-TSDatabaseDistances(vazoes_profit_ts,distance="spec.glk")
#attributes(D_acf)

save(D_acf,D_pacf,D_euc,D_manh,D_mink,D_ifnrm,
     D_ccor,D_ccor2,D_cor,D_cort,D_per,D_fourier,
     map1,map2,map3,map4,map5,map6,map15,map16,map13,
     vazoes_profit_ts,vazoes_profit,vazoes_dat,vaz_code,
     file = 'Vazoes_Profit.RData')


#--------------------------
map0<-smacofSym(D_euc)
map1<-smacofSym(D_acf)
map2<-smacofSym(D_pacf)
map3<-smacofSym(D_manh)
map4<-smacofSym(D_mink)
map5<-smacofSym(D_ifnrm)
map6<-smacofSym(D_ccor)

map15<-smacofSym(D_cor)
map16<-smacofSym(D_cort)

map13<-smacofSym(D_fourier)

map7<-smacofSym(D_sts)
map8<-smacofSym(D_dtw)
map9<-smacofSym(D_edr)
map10<-smacofSym(D_keogh)
map11<-smacofSym(D_erp)
map12<-smacofSym(D_lcss)

map14<-smacofSym(D_tquest)


#-------------------------
plot(map1)
plot(map2)
plot(map3)
plot(map4)
plot(map5)
plot(map6)

plot(map15)
plot(map16)

plot(map13)

plot(map7)
plot(map8)
plot(map9)
plot(map10)
plot(map11)
plot(map12)




#--------------------------
library(factoextra)

#clus0<-hclust(D_cor)
clus1<-hclust(D_acf)
fviz_dend(clus1, cex = 0.5)

# Dendograma -------------------------
fviz_dend(clus1, k = 2, # Cut in 5 groups
          cex = 0.4, horiz=F,# label size
          k_colors = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_dend(clus1, k = 5, # Cut in 5 groups
          cex = 0.5, type='circular',# label size
          k_colors = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
#grup0 <- cutree(clus0, k = 5)
grup1 <- cutree(clus1, k = 2)
# Estaciones por grupo
labels(D_acf)[grup1 == 1]
labels(D_acf)[grup1 == 2]
labels(D_acf)[grup1 == 3]
labels(D_acf)[grup1 == 4]
labels(D_acf)[grup1 == 5]

#Grafico de Nube de Grupos --------------
fviz_cluster(list(data = map1$conf, cluster = grup1),cex = 0.1,
             palette = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
             ellipse.type = "convex", # Concentration ellipse
             repel = F, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#Cluster Óptimo   --------------
fviz_nbclust(map1$conf, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)


fviz_nbclust(map1$conf, kmeans, method = "silhouette")



# --------------   PROFIT  ------------------------------

#Quitar NA's de Latitud y Longitud (tambien en Dimensiones MDS)
excl<-which(is.na(vazoes_code$Latitud))
D1<-as.numeric(map1$conf[,'D1']);#D1<-D1[-excl]
D2<-as.numeric(map1$conf[,'D2']);#D2<-D2[-excl]

BDD_profit<-data.frame(Nombre_ST = paste0('VAZOES(',vazoes_code$Estacion,')'),
                       Estacion=vazoes_code$Estacion,
                       Codigo_ONS=vazoes_code$Codigo_ONS,
                       Latitud=vazoes_code$Latitud,
                       Longitud=vazoes_code$Longitud, 
                       D1, D2, Cluster=as.factor(as.character(grup1))
)

BDD_profit<-BDD_profit[-excl,]

fit_lat<-lm(Latitud~D1+D2, data=BDD_profit)
fit_long<-lm(Longitud~D1+D2, data=BDD_profit)

# par(mfrow=c(2,3))
# plot(fit_lat,which = 1:6)
# plot(fit_long,which = 1:6)
# 
# summary(fit_lat)
# summary(fit_long)

b1_lat<-fit_lat$coefficients[2]
b2_lat<-fit_lat$coefficients[3]

b1_long<-fit_long$coefficients[2]
b2_long<-fit_long$coefficients[3]

#Vectores de Profit
beta<-matrix(0,2,2)
beta[1,c(1,2)]<-c(b1_lat, b2_lat)  #Coeficientes de Regresion Latitud
beta[2,c(1,2)]<- c(b1_long, b2_long)  #Coeficientes de Regresion Longitud

pk<-sqrt(beta[,1]^2+beta[,2]^2) #Escala

beta_stnd<-matrix(0,2,2) #Coeficientes Beta Estandarizados
beta_stnd[,1]<-beta[,1]/pk
beta_stnd[,2]<-beta[,2]/pk

#Grafico Profit
fviz_cluster(list(data = map1$conf, cluster = grup1),cex = 0.1,
             palette = c("#6C3483", "#00AFBB", "#E7B800", "#FC4E07", "#00FF00"),
             ellipse.type = "convex", # Concentration ellipse
             repel = F, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+ 
  annotate("segment", x=0, xend=6*beta_stnd[1,1], y=0, yend=6*beta_stnd[1,2], colour="red",
           size=1, arrow=arrow())+ 
  annotate("segment", x=0, xend=6*beta_stnd[2,1], y=0, yend=6*beta_stnd[2,2], colour="red",
           size=1, arrow=arrow())+ 
  annotate("text", x=6.2*beta_stnd[1,1], y=6.2*beta_stnd[1,2], label="Latitud")+
  annotate("text", x=6.2*beta_stnd[2,1], y=6.2*beta_stnd[2,2], label="Longitud")
  
for(i in 1:2){ #2 Variables dependientes
  text(1.4*beta_stnd[i,1],1.4*beta_stnd[i,2],labels = c('Latitud','Longitud')[i],col = 'red')
  arrows(0,0,1.4*beta_stnd[i,1],1.4*beta_stnd[i,2],col = 'red')
}


#vazoes_code<-vaz_code
# save(BDD_profit,D_acf,D_pacf,D_euc,D_manh,D_mink,D_ifnrm,
#      D_ccor,D_ccor2,D_cor,D_cort,D_per,D_fourier,
#      map1,map2,map3,map4,map5,map6,map15,map16,map13,
#      vazoes_profit_ts,vazoes_profit,vazoes_dat,vazoes_code,
#      file = 'Vazoes_Profit.RData')


#------------   Grafico conjunto de series   -------------------------
library(dygraphs)
est_cluster1<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==1])
est_cluster2<-as.character(BDD_profit$Nombre_ST[BDD_profit$Cluster==2])

aux_vaz_dy<-vazoes_profit_ts
names(aux_vaz_dy)<-names(vazoes_profit)[-1]

#Series Cluster1

dygraph(aux_vaz_dy[,est_cluster1[1:5]], main = "Grupo de Indices",group = 'indice')%>% 
  dyRangeSelector(dateWindow = c('2005-01-01','2015-12-31'))%>%
  #dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')

#Series Cluster2 
dygraph(aux_vaz_dy[,est_cluster2[1:5]], main = "Grupo de Indices",group = 'indice')%>% 
  dyRangeSelector(dateWindow = c('2005-01-01','2015-12-31'))%>%
  #dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  dyLegend(show='always')




