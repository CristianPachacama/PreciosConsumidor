#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------      ANALISIS REGRESION IPC - PUCE      ------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
#Tablas
library(readxl)
library(readr)
library(dplyr)
library(zoo)
#Series de Tiempo
library(dygraphs)

#>> Funciones Extra ----------------------------------------------
source("Code/AnalisisRegresion.R",local = TRUE)
source("Code/GraficoRegresion.R",local = TRUE)
#>> Carga de Datos -----------------------------------------------
IPChist = read_csv("Data/IPChistorico.csv")

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
                              fluidRow(dataTableOutput("tabla_prod"))
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 #INFORMACIÓN DE LA BASE DE DATOS ------------------------------
                 # tabPanel("Datos"
                 #            
                 #            ),
                 
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 tabPanel('Análisis',
                          
                          fluidRow(
                            # Panel Lateral -------------------------------
                            sidebarPanel(
                              h4('Panel de Control'),
                              selectInput('nivel', 
                                          label= 'Selecciona Producto',
                                          selected = 1,
                                          choices=c('Nacional'=1,
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
                                                    'Ambato'=12)),
                              p('Ahora selecciona que series deseas Análizar (a partir de ella se realizará el Análisis Clúster).'),
                              selectInput('serie', 
                                          label= 'Selecciona Serie de Tiempo',
                                          selected = 'IPC',
                                          choices=c('IPC'='IPC',
                                                    'IPC Deflactado (sobre IPC general)'='IPCx2',
                                                    'Variación(%) IPC'='IPCx')),
                              p('Selecciona una de las Métricas definidas para series de tiempo.'),
                              selectInput('metrica', 
                                          label= 'Selecciona Métrica',
                                          selected = 'cort',
                                          c("a")),
                              p('Elige el número de clusters que quieres que se formen.'),
                              sliderInput('clusters', label= 'Número de Clusters',min=2,max=20,value = 20),
                              actionButton('clus_boton',label='Clusterizar',icon = icon('braille')),hr(),
                              #Panel Control Graficos
                              h4('Gráfico de Series'),
                              # p('Elige que Series deseas Gráficar.'),
                              selectInput('serie_graf', 
                                          label= 'Selecciona Serie de Tiempo',
                                          selected = 'IPC',
                                          choices=c('IPC'='IPC',
                                                    'IPC Deflactado (sobre IPC general)'='IPCx2',
                                                    'Variación(%) IPC'='IPCx')),
                              p('A continuación elija un Clúster, para graficar todas las series del mismo.'),
                              numericInput(inputId = "grupo",label='Clúster',value = 1,min = 1,max=20)
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              h3('Análisis Clúster para Series Temporales del IPC'),hr(),
                              h4('Gráfico de las Series por Cluster'),
                              dygraphOutput('cluster_graf'),hr(),
                              h4('Tabla de Productos por Cluster'),
                              fluidRow(dataTableOutput("cluster_table"))
                              # fluidRow()
                              
                            )
                          ),hr()
                 )
                          
)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  
  
  
  
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

