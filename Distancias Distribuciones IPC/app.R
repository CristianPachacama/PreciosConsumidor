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
library(readxl)
library(readr)
# library(DT)
# library(dplyr)
library(tidyverse)
#Series de Tiempo
library(TSdist)
library(ggplot2)
library(gridExtra)
# library(xts)
#Mutivariante
library(smacof)
library(cluster)
#>> Carga de Datos -----------------------------------------------
IPC = read_csv("Data/IPChistoricoTrn.csv")
productos = names(IPC)[-1]
ProductosLista = 1:length(productos)
names(ProductosLista) = productos
IPC = data.frame(IPC)
names(IPC) = c("Fecha", productos)
periodos = c(2005, 2007, 2010, 2014, 2019)
#-------------------------------------------
source(file ="Code/MedMovil.R" ,local = TRUE)
#------------------------------------------------

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

ui <- navbarPage(title = "Distancias K-S",
                 header = tags$h3("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='puce.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",theme=shinytheme('yeti'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='puce_logo.png',width='90px',align='center'),
                                          tags$b('Proyecto: '),' "Análisis del Poder de Mercado (Series IPC)".' ,
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
                                           column(8,'Análisis del Poder de Mercado (Series IPC)')
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
                              h3('Análisis del Poder de Mercado (Series IPC)'),
                              hr(),h4('Resume:'),
                              fluidRow(' '),
                              p('Para el anális se considera el Índice de Precios al 
                                Consumidor de todos los bienes de la canasta básica 
                                familiar (series mensuales de los últimos 13 años).
                                Podemos encontrar las series de los siguientes 116 productos.
                                Se procedió a realizar el análisis Clúster de las series 
                                asociadas a estos productos, para el análisis se consideraron 
                                varias métricas, entre las que se destacan las que consideran la correlación 
                                tanto temporal como entre distintas series.')#,
                              # fluidRow(dataTableOutput("tabla_prod"))
                              
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
                            sidebarPanel(width = 3,
                              h4('Panel Control Graficos'),
                              selectInput('producto', 
                                          label= 'Selecciona Producto',
                                          selected = 1,
                                          choices=ProductosLista)#,
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              h3('Distribución del IPC Deflactado'),
                              h4(textOutput('productNombre')),hr(),
                              plotOutput('graficoDist')
                              
                              
                            )
                          ),hr()
                 )
                          
)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  
  #Subtitulo Producto  ------------------------
  output$productNombre = renderText({
    
    names(ProductosLista)[as.numeric(input$producto)]
    
  })
  
  # Grafico Generado --------------------------
  output$graficoDist = renderPlot({
    
    k = as.numeric(input$producto)
    
    SerieStnd = as.numeric( IPC[,k+1] / mav12Gen$mvxRecup)
    
    
    # Serie = mav12Gen$resxRecup
    Fecha = as.Date(IPC$Fecha, format = "%d-%m-%y")
    Anio = as.numeric(format(Fecha, "%Y"))
    # Mes = as.numeric(format(Fecha,"%m"))
    
    etiquetas = c()
    for (i in 1:(length(periodos) - 1)) {
      etiquetas[i] = paste0("Periodo: ", periodos[i], " - ", periodos[i + 1])
    }
    
    
    PeriodoCorte = cut(Anio,
                       breaks = periodos ,
                       labels = etiquetas ,
                       right = F)
    
    
    BDDgraf = data.frame(Fecha, SerieStnd , SerieOrig =  IPC[,k+1], IPC_GeneralS = mav12Gen$mvxRecup , PeriodoCorte)
    MediaSeries = BDDgraf %>% group_by(PeriodoCorte) %>% summarise(Media = mean (SerieStnd))
    
    #Graficos Individuales -------------------------------------------
    BDDgraf1 = BDDgraf %>%
      select(Fecha,`Serie Original` = SerieOrig, `MM12 (IPC General)` = IPC_GeneralS) %>%
      gather(key = "Serie", value = "value", -Fecha)
    
    seriegraf1 = ggplot(BDDgraf1, aes(x = Fecha, y = value)) + 
      geom_line(aes(color = Serie), size = 0.7) +
      scale_color_manual(values = c("#0174DF","#2E2E2E")) +
      theme_minimal()+
      labs(title = paste("IPC:", productos[k]) , y = "IPC") +
      geom_vline(
        xintercept = as.Date(paste0(periodos[-c(1,length(periodos))],"-01-01")),
        linetype = "dashed",
        color = "red",
        size = 1
      ) +
      theme(
        legend.title = element_text(size = 12, color = "black", face = "bold"),
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank()
      )
    
    # seriegraf1 =  ggplot(data = BDDgraf1, aes(x = Fecha, y = SerieOrig)) +
    #   geom_line(size = 0.7) + theme_minimal() +
    #   labs(title = paste("IPC:", productos[k]) , y = "IPC")
    
    
    seriegraf2 =  ggplot(data = BDDgraf, aes(x = Fecha, y = SerieStnd)) +
      geom_line(size = 0.7) + theme_minimal() +
      labs(title = paste("IPC Deflactado:", productos[k]) , y = "IPC Deflactado (por IPC General Suavizado)") +
      geom_vline(
        xintercept = as.Date(paste0(periodos[-c(1,length(periodos))],"-01-01")),
        linetype = "dashed",
        color = "red",
        size = 1
      )
    
    
    densidades = ggplot(data = BDDgraf ,
                        aes(x = SerieStnd, fill = PeriodoCorte, colour =
                              PeriodoCorte)) +  geom_density(alpha = 0.2) +
      labs(title = paste("IPC Deflactado:", productos[k]), x = "IPC Deflactado por Periodo") +
      geom_vline(data = MediaSeries,
                 aes(xintercept = Media, color = PeriodoCorte),
                 linetype = "dashed") +
      theme(
        legend.title = element_text(size = 12, color = "black", face = "bold"),
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank()
      )
    
    
    #Grafico Multiple -----------------
    Grafico = grid.arrange(
      grobs = list(seriegraf1,seriegraf2,densidades),
      widths = c(3, 2),
      layout_matrix = rbind(c(1, 3),
                            c(2, 3))
    )
    
  })
  

}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

