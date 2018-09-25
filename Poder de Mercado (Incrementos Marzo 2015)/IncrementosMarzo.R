library(dygraphs)
library(dplyr)
library(readxl)
load("Data/DatosAux.RData")
h=1

nivel = c('Nacional'=1,
  'Región Sierra'=2,
  'Región Costa'=3,
  'Guayaquil'=4,
  'Esmeraldas'=5,
  'Machala'=6,
  'Manta'=7,
  'Santo Domingo'=8,
  'Quito'=9,
  'Loja'=10,
  'Cuenca'=11,
  'Ambato'=12)

IPChist = read_excel("Data/ipc_ind_nac.xls",sheet = as.numeric(nivel[h]), skip = 4)
IPChist = IPChist[,-dim(IPChist)[2]]

Nombres[2] = "Producto"
names(IPChist) = Nombres
IPChist = IPChist[-c(1,dim(IPChist)[1] , dim(IPChist)[1]-1) , ]
IPCprod = inner_join(Productos,IPChist)

#Clasificación -----------------------------
# IPC = IPCprod
VarPorcent = function(IPC){
  # Prod = IPCi[1]
  # ProdAbr = IPCi[2]
  IPCaux = IPC[-c(1:3)]
  
  VarIPCFeb = (IPCaux["01/02/15"]-IPCaux["01/11/14"])/IPCaux["01/11/14"]
  VarIPCMarzo = (IPCaux["01/03/15"]-IPCaux["01/12/14"])/IPCaux["01/12/14"]
  VarIPCAbril = (IPCaux["01/04/15"]-IPCaux["01/01/15"])/IPCaux["01/01/15"]
  
  VarIPC = data.frame(IPCprod[,1:3],VarIPCFeb,VarIPCMarzo,VarIPCAbril)
  names(VarIPC)[4:6] = c("01/02/15","01/03/15","01/04/15")
  VarIPC = VarIPC %>% arrange(desc(`01/02/15`),desc(`01/03/15`),desc(`01/03/15`))
  
  return(VarIPC)
}

IPCsaltos = VarPorcent(IPCprod)
IPCsaltos = IPCsaltos[1:10,]

#In
IPCprodx = IPCprod
IPCprodx$ID = 1:dim(IPCprodx)[1]
ind = IPCprodx %>% inner_join(IPCsaltos,by="Producto") %>% select(ID)

ind= ind$ID
#Grafico de Series   -----------------------
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

items = IPCprod$Producto
IPC = t(as.matrix(IPCprod[,-c(1:3)]))
IPC = ts(IPC, start = c(2005, 1),frequency = 12)
colnames(IPC) = items

graf_series(IPC,ind,fecha = c(1,2005))





