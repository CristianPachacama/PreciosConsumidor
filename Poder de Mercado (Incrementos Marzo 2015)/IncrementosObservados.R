library(readxl)
library(dplyr)
library(dygraphs)
library(TSstudio)

BDDSaltos = read_excel("Data/SeriesSaltos2.xlsx")

BDDSaltosTs = ts(BDDSaltos[,-1], start = c(2005, 1),frequency = 12)


graf_series = function(IPC,ind = NULL,fecha = fecha0){
  
  if(is.null(ind)){
    aux = IPC[,-1]
  }else{
    aux = IPC[,ind]
  }
  
  aux = ts(aux,start = fecha[c(2,1)] , frequency = 12)
  
  graf=dygraph(data = aux, main = "IPC Productos")%>%
    # dyAxis("x", label=names(metric)) %>%
    # dyAxis('y',label='')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)#%>%
  # dyLegend(show= "follow",width = 400)
  
  return(graf)
  
}

graf_series(BDDSaltosTs,fecha = c(1,2005))


#Graficos TSstudio -------------------------
ts_plot(BDDSaltosTs, 
        title = "Indice de Precios al Consumidor",
        type = "single",
        dash = "dash",
        width = 1,
        Xgrid = TRUE,
        Ygrid = TRUE)#,
        # slider = TRUE)




