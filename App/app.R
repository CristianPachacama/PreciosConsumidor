#Descarga de Paquetes ================================
# (Solo compilar esta sección la primera vez)
# install.packages('shinythemes',dependencies = T)
# install.packages('shinydashboard',dependencies = T)
# install.packages('markdown',dependencies = T)
# install.packages('leaflet',dependencies = T)
# install.packages('htmltools',dependencies = T)
# install.packages('rgdal',dependencies = T)
# install.packages('DT',dependencies = T)
# install.packages('plotly',dependencies = T)
# install.packages('ggplot2',dependencies = T)
# install.packages('dygraphs',dependencies = T)
# install.packages('seasonal',dependencies = T)
# install.packages('stlplus',dependencies = T)

#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------        ANALISIS CLUSTER IPC - PUCE      ------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
#Mapas
# library(leaflet)
#library(htmltools)
#library(rgdal)
#Tablas
library(readr)
library(DT)
library(dplyr)
#Series de Tiempo
library(TSdist)
library(dygraphs)
# library(xts)
#Mutivariante
library(smacof)
library(cluster)
#>> Funciones Extra ----------------------------------------------
source("Code/AnalisisCluster.R",local = TRUE)
source("Code/GraficoCluster.R",local = TRUE)
#>> Carga de Datos -----------------------------------------------
load("Data/DatosAux.RData")
IPChist = read_csv("Data/IPChistorico.csv")
#Transponer Datos
IPC = t(as.matrix(IPChist[,c(-1,-2)]))
IPC = ts(IPC, start = c(2005, 1),frequency = 12)
#Series Diferenciadas
IPCx = as.matrix(diff(IPC))
#Modificación de IPC (Normalización /IPC General)
IPCx2= IPChist
for(i in 2:dim(IPChist)[1]){
  IPCx2[i,-(1:2)] = IPCx2[i,-(1:2)]/IPCx2[1,-(1:2)]
}
IPCx2 = t(as.matrix(IPCx2[,c(-1,-2)]))
#Extras
Items = IPChist$ITEM
items = as.vector(abbreviate(IPChist$ITEM))
colnames(IPC) = items

#Data Productos
names(IPChist)[2] = "Producto"
IPCprod = cbind(Productos,IPChist[,-c(2:158)])

#Lista de Métricas
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

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

ui <- navbarPage(title = "Clustering IPC",
                 header = tags$h3("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='puce.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",theme=shinytheme('yeti'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='puce_logo.png',width='90px',align='center'),
                                          tags$b('Proyecto: '),' "Análisis Clúster para Series Temporales del IPC".' ,
                                          '-',tags$a('Instituto de Investigaciones Económicas - PUCE (2018)',href='https://www.puce.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('Cristian Pachacama',href='http://www.linkedin.com/in/cristian-david-pachacama')
                 )
                 ),
                 
                 #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
                 tabPanel('Introducción',icon=icon('home'),
                          
                          fluidRow(
                            
                            sidebarPanel(img(src='puce_logo2.png',width='90%',align='center' ),
                                         fluidRow(' '),
                                         hr(),
                                         fluidRow(
                                           column(3,tags$b('Proyecto:')),column(1),
                                           column(8,'Análisis Clúster para Series Temporales del IPC.')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Linea de Investigación:')),column(1),
                                           column(8,'Machine Learning')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Unidad:')),column(1),
                                           column(8,'Instituto de Investigaciones Económicas')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Director:')),column(1),
                                           column(8,'PhD. Pedro Páez')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Researcher:')),column(1),
                                           column(8,'Cristian Pachacama')
                                         )
                                         
                            ),
                            
                            mainPanel(
                              h3('Análisis Clúster para Series Temporales del IPC'),
                              hr(),h4('Resume:'),
                              fluidRow(' '),
                              p('Para el anális se considera el Índice de Precios al 
                                Consumidor de todos los bienes de la canasta básica 
                                familiar (series mensuales de los últimos 13 años).
                                Podemos encontrar las series de los siguientes 116 productos.
                                Se procedió a realizar el análisis Clúster de las series 
                                asociadas a estos productos, para el análisis se consideraron 
                                varias métricas, entre las que se destacan las que consideran la correlación 
                                tanto temporal como entre distintas series.'),
                              dataTableOutput("tabla_prod")
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 #INFORMACIÓN DE LA BASE DE DATOS ------------------------------
                 tabPanel("Datos"
                            
                            ),
                 
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 tabPanel('Análisis',
                          
                          fluidRow(
                            # Panel Lateral -------------------------------
                            sidebarPanel(
                              h4('Cluster de Series de Tiempo'),
                              p('Primero selecciona que series deseas Análizar.'),
                              selectInput('serie', 
                                          label= 'Selecciona Serie de Tiempo',
                                          selected = 'IPC',
                                          choices=c('IPC'='IPC',
                                                    'IPC Normalizado (sobre IPC general)'='IPCx',
                                                    'IPC Diferenciado'='IPCx2')),
                              p('Selecciona una de las Métricas definidas para series de tiempo.'),
                              selectInput('metrica', 
                                          label= 'Selecciona Métrica',
                                          selected = 'cort',
                                          metricasList),
                              p('Elige el número de clusters que quieres que se formen.'),
                              sliderInput('clusters', label= 'Número de Clusters',min=2,max=20,value = 4),
                              actionButton('clus_boton',label='Clusterizar',icon = icon('braille')),hr(),
                              #Panel Control Graficos
                              h4('Gráfico de Series'),
                              # p('Elige que Series deseas Gráficar.'),
                              selectInput('serie_graf', 
                                          label= 'Selecciona Serie de Tiempo',
                                          selected = 'IPC',
                                          choices=c('IPC'='IPC',
                                                    'IPC Normalizado (sobre IPC general)'='IPCx',
                                                    'IPC Diferenciado'='IPCx2')),
                              p('A continuación elija un Clúster, para graficar todas las series del mismo.'),
                              numericInput(inputId = "grupo",label='Clúster',value = 1,min = 1,max=4)
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              h3('Análisis Clúster para Series Temporales del IPC'),hr(),
                              h4('Gráfico de las Series por Cluster'),
                              dygraphOutput('cluster_graf'),hr(),
                              h4('Tabla de Productos por Cluster'),
                              fluidRow(dataTableOutput("cluster_table",width = "50%"))
                              
                            )
                          ),hr()
                 )
                          
)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  #Actualizar Input (Grupo)
  observe({
    updateNumericInput(session,inputId = "grupo",
                       label='Clúster',value = 1,
                       min = 1,max=input$clusters)
  })
  #Tabla de Productos
  output$tabla_prod = renderDataTable({
    IPCprod
  })
  
  #Guardamos el Analsis Cluster en cl
  clust = eventReactive(input$clus_boton,{
    analisis = switch(input$serie,
      'IPC'={EstudioIPC(IPC, metricasList,
                        metrica = which(metricasList==input$metrica), 
                        clusters = input$clusters)},
      'IPCx'={EstudioIPC(IPCx, metricasList,
                         metrica = which(metricasList==input$metrica), 
                         clusters = input$clusters)},
      'IPCx2'={EstudioIPC(IPCx2, metricasList,
                          metrica = which(metricasList==input$metrica), 
                          clusters = input$clusters)}
    )
    return(analisis)
  })
  
  
  #Mostramos Graficos de Series
  output$cluster_graf = renderDygraph({
    cl=clust()
    grafico = switch(input$serie_graf,
                     'IPC'={graf_series(IPC,items,cl[[input$grupo]],input$metrica,cluster=input$grupo)},
                     'IPCx'={graf_series(IPCx,items,cl[[input$grupo]],input$metrica,cluster=input$grupo)},
                     'IPCx2'={graf_series(IPCx2,items,cl[[input$grupo]],input$metrica,cluster=input$grupo)}
                     )
    return(grafico)
  })

  
  # output$cluster_table
  output$cluster_table = renderDataTable({
    cl=clust()
    IPCprod[cl[[input$grupo]],]
  })
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

