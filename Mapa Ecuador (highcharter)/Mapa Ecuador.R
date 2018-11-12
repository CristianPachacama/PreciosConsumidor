library(highcharter)
library(dplyr)
library(rjson)
library(stringr)

# Descarga de Datos --------------------
# ecuador <- fromJSON(file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json")
ecuador = fromJSON(file= "Mapa_Ecuador.json")
load(url("https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/poblacion_provincia_ecuador_2010.Rdata"))
periodo = "2018"

#-----------------------
glimpse(ecuador)
glimpse(poblacion_provincia_ecuador_2010)

BDDMap = poblacion_provincia_ecuador_2010

# Mapa en Highcharter  -----------------


highchart() %>%
  hc_title(text = "<b>Índice de Precios al Consumidor</b>",
           margin = 20, align = "center",
           style = list(color = "#08338F", useHTML = TRUE)) %>% 
  hc_subtitle(text = paste("Índice Provincial",periodo),
              align = "center",
              style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
  hc_tooltip(followPointer =  TRUE) %>%
  hc_add_series_map(map = ecuador, df = BDDMap, name = "Población",
                    value = "Poblacion", joinBy = c("name", "Provincia"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.woe-name}')) %>%
  # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
  # hc_colorAxis(minColor = "#B7D4EB", maxColor = "#08338F")  %>%
  # hc_colorAxis(minColor = "#FF9933", maxColor = "#006699")  %>%
  # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 30), 
  #                                          colors = c("#440154", "#21908C", "#FDE725"))) %>% 
  
  hc_mapNavigation(enabled = TRUE) %>%
  #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # hc_add_theme(hc_theme_ffx()) %>% 
  # hc_add_theme(hc_theme_538()) %>% 
  # hc_add_theme(hc_theme_ft()) %>%
  # hc_add_theme(hc_theme_db()) %>% 
  # hc_add_theme(hc_theme_flat()) %>%
  # hc_add_theme(hc_theme_flatdark()) %>% 
  # hc_add_theme(hc_theme_smpl()) %>%
  # hc_add_theme(hc_theme_elementary()) %>% 
  # hc_add_theme(hc_theme_google()) %>% 
  # hc_add_theme(hc_theme_monokai()) %>% 
# hc_add_theme(hc_theme_gridlight()) %>% 
# hc_add_theme(hc_theme_sandsignika()) %>% 
# hc_add_theme(hc_theme_darkunica()) %>%
# hc_add_theme(hc_theme_economist()) %>%
# hc_add_annotation(xValue = 0, yValue = 0, title = list(text = 'Fuente: INEC')) %>% 

# hc_chart(borderColor = "#08338F",
#          borderRadius = 10,
#          borderWidth = 2) %>% 

hc_legend(enabled = FALSE)#%>%
# hc_legend(align = "center", x = 0, y = -70) %>%
# hc_motion(enabled = TRUE)


# MAPA 2 ---------------------------------------------------
highchart() %>%
  hc_title(text = "<b>Índice de Precios al Consumidor</b>",
           margin = 20, align = "center") %>% 
  hc_subtitle(text = paste("Índice Provincial",periodo),
              align = "center") %>% 
  hc_tooltip(followPointer =  TRUE) %>%
  hc_add_series_map(map = ecuador, df = BDDMap, name = "Población",
                    value = "Poblacion", joinBy = c("name", "Provincia"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.woe-name}')) %>%
  # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
  # hc_colorAxis(minColor = "#B7D4EB", maxColor = "#08338F")  %>%
  hc_colorAxis(minColor = "#ffff99", maxColor = "#660066")  %>%
  # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 40),
  #                                          colors = c("#ff0000", "#ffcc00", "#33cc33"))) %>%
  
  hc_mapNavigation(enabled = TRUE) %>%
  #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # hc_add_theme(hc_theme_ffx()) %>%
  # hc_add_theme(hc_theme_538()) %>%
  # hc_add_theme(hc_theme_ft()) %>%
  # hc_add_theme(hc_theme_db()) %>%
  # hc_add_theme(hc_theme_flat()) %>%
  # hc_add_theme(hc_theme_flatdark()) %>%
  # hc_add_theme(hc_theme_smpl()) %>%
  # hc_add_theme(hc_theme_elementary()) %>%
  # hc_add_theme(hc_theme_google()) %>%
  # hc_add_theme(hc_theme_monokai()) %>%
  # hc_add_theme(hc_theme_gridlight()) %>%
  # hc_add_theme(hc_theme_sandsignika()) %>%
# hc_add_theme(hc_theme_darkunica()) %>%
hc_add_theme(hc_theme_economist()) %>%
# hc_add_annotation(xValue = 0, yValue = 0, title = list(text = 'Fuente: INEC')) %>% 

hc_chart(borderColor = "#404040",
         borderRadius = 10,
         borderWidth = 2) %>%
hc_legend(layout = "vertical", reversed = TRUE,
          floating = TRUE, align = "right")
# hc_legend(enabled = FALSE)#%>%
# hc_legend(align = "center", x = 0, y = -70) %>%
# hc_motion(enabled = TRUE)



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#------------------------- MAPA 3 ANIMADO --------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Data para Animacion Mapa
producto = "Alimentos"
BDDMap = read.csv(file="PoblacionEcMapa.csv",stringsAsFactors = FALSE)

DFMap = BDDMap %>% 
  group_by(Provincia) %>% 
  do(item = list(
    Provincia = first(.$Provincia),
    sequence = .$value,
    value = first(.$value))) %>% 
  .$item

#----------------------------------------------------------
highchart(type = "map") %>%
  hc_title(text = "<b>Índice de Precios al Consumidor</b>",
           margin = 20, align = "center") %>% 
  hc_subtitle(text = producto,
              align = "center") %>% 
  hc_tooltip(followPointer =  TRUE) %>%
  hc_add_series(data = DFMap,
                mapData = ecuador,
                name = "Población",
                value = "Poblacion", 
                joinBy = c("name", "Provincia"),
                dataLabels = list(enabled = TRUE,
                                  format = '{point.properties.woe-name}')) %>%
  
  # hc_motion(enabled = TRUE,series = 1,labels = 1:n) %>% 
  # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
  hc_colorAxis(minColor = "#FF9933", maxColor = "#006699")  %>%
  # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 40),
  #                                          colors = c("#ff0000", "#ffcc00", "#33cc33"))) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  hc_add_theme(hc_theme_ffx()) %>%
  # hc_add_theme(hc_theme_darkunica()) %>%
  # hc_add_theme(hc_theme_economist()) %>%
  hc_chart(borderColor = "#404040",
           borderRadius = 10,
           borderWidth = 2) %>%
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_motion(
    enabled = TRUE,
    axisLabel = "Año",
    labels = sort(unique(BDDMap$Anio)),
    series = 0,
    updateIterval = 10,
    magnet = list(
      round = "floor",
      step = 0.02
    )
  )






