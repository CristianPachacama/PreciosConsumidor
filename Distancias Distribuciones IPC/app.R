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
#Tablas
library(readxl)
library(readr)
library(xtable)
library(DT)
library(tidyverse)
#Series de Tiempo
library(TSdist)
library(ggplot2)
library(gridExtra)
# library(xts)
library(forecast)
library(hexbin)

#>> Carga de Datos -----------------------------------------------
IPC = read_csv("Data/IPChistoricoTrn.csv")

productos = names(IPC)[-1]
ProductosLista = 1:length(productos)
names(ProductosLista) = productos
IPC = data.frame(IPC)
names(IPC) = c("Fecha", productos)

# periodos0 = c(2005, 2007, 2010, 2014, 2019)
PeriodoLista = 2006:2018
names(PeriodoLista) = paste("Año:",PeriodoLista)

#Tipo de Estandarizacion (Deflactar)
TipoDeflactor = c("MM12(IPC General)" = 1, "MM12(Serie Original)" = 2)

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
                                           column(8,'Econometría')
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
                                          selected = 2,
                                          choices=ProductosLista),
                              radioButtons("deflactor", 
                                           label = "Elije Deflactor",
                                           choices = TipoDeflactor, 
                                           selected = 1),
                              checkboxGroupInput("periodos", 
                                                 label = "Eligir Periodos de Corte", 
                                                 choices = PeriodoLista,
                                                 selected = c(2007,2010,2014))
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              h3('Distribución del IPC Deflactado'),
                              h4(textOutput('productNombre')),hr(),
                              plotOutput('graficoDist',height = "530px",width = '110%'),
                              h4('Resumen del Modelo'),
                              DTOutput('resumenRegres')
                              
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
    
    #Datos Iniciales y Analisis -----------------------------
    source(file = "Code/RegresionPanelReact.R", local = TRUE)
    
    #Graficos Individuales ----------------------------------
    BDDgraf1 = BDDgraf 
    deflactAux = names(TipoDeflactor)[as.numeric(input$deflactor)]
    
    BDDgraf1 = BDDgraf1[,c(1,3,4)]
    names(BDDgraf1) = c("Fecha",
                        "Serie Original",
                        deflactAux)
    
    BDDgraf1 = BDDgraf1 %>%
      # select(Fecha,`Serie Original` = SerieOrig, IPC_GeneralS) %>%
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
    
    seriegraf2 =  ggplot(data = BDDgraf, aes(x = Fecha, y = SerieStnd)) +
      geom_line(size = 0.7) + theme_minimal() +
      labs(title = paste("IPC Deflactado+Regresión:", productos[k]) , y = "IPC Deflactado (por IPC General Suavizado)") +
      geom_vline(
        xintercept = as.Date(paste0(periodos[-c(1, length(periodos))], "-01-01")),
        linetype = "dashed",
        color = "red",
        size = 1
      ) +
      geom_line(
        data = predicted,  #Anadir Lineas de Regresion !!!!!!!!!
        aes(x = Fecha, y = IPCfit, colour = PeriodoCorte),
        size = 0.7
      ) +
      theme(
        legend.title = element_text(size = 12, color = "black", face = "bold"),
        legend.justification = c(0, 1),
        legend.position = c(0.75,0.5),
        legend.background = element_blank(),
        legend.key = element_blank()
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
    grid.arrange(
      grobs = list(seriegraf1,seriegraf2,densidades),
      widths = c(3, 2),
      layout_matrix = rbind(c(1, 3),
                            c(2, 3))
    )
    
  })
  
  
  # Tabla resumen de Regresion  -----------------------------
  output$resumenRegres = renderDT({
    #Datos Iniciales y Analisis -----------------------------
    source(file = "Code/RegresionPanelReact.R", local = TRUE)
    
    #Tabla Resumen de Regresion -----------------------------
    resumen = data.frame(round(xtable(summary(modelo1)),digits = 5))
    names(resumen) = c("Estimación","Error Estándar","t-valor","Pr(>|t|)")
    Pval = as.numeric(summary(modelo1)$coefficients[,4])
    rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
                 labels = c("***","**","*","."," "))
    resumen$Signif = rangos
    
    
    datatable(#filter = 'top',
              #Formato de la tabla -------------------
              extensions = c('Buttons'), #c('Responsive','Buttons'),
              options = list(pageLength=10,searchHighlight = TRUE,
                             dom = 'Bfrtip',
                             buttons = list('copy','print', list(
                               extend = 'collection',
                               buttons = c('csv', 'excel', 'pdf'),
                               text = 'Descargar'
                             ))
              ),
              {
                #Tabla a Mostrar  --------------------
                resumen
              })
    
  })
  

}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

