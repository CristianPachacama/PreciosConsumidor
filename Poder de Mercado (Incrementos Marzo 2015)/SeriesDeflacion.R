#Series con tendencia al Decrecimiento
library(readxl)
library(TSstudio)

BDDDecre = read_excel("Data/SeriesDeflacion.xlsx")

BDDDecreTs = ts(BDDDecre[,-1], start = c(2005, 1),frequency = 12)


#Graficos TSstudio -------------------------
ts_plot(BDDDecreTs, 
        title = "Indice de Precios al Consumidor",
        type = "single",
        dash = "dash",
        width = 1,
        Xgrid = TRUE,
        Ygrid = TRUE)#,
# slider = TRUE)

