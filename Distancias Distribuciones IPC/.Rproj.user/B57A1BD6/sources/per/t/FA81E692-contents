library(forecast)

# Datos de Entrada  ----------------------------
k=2
periodos = c(2005, 2007, 2010, 2014, 2019)
source("DistanciasEstadisticas.R")

productos[k]

#Regresion: Modelo de Efectos Fijos ------------
BDDpanel = BDDgraf[,c("Fecha","SerieStnd","PeriodoCorte")]
BDDpanel$Tmp = 1:dim(BDDpanel)[1]
modelo1 = lm(data = BDDpanel, formula = SerieStnd ~ Tmp + PeriodoCorte*Tmp)
summary(modelo1)
plot(modelo1)


#Grafico de Modelo Lineal lm()  ----------------
library(car)
scatterplot(SerieStnd ~ Tmp + PeriodoCorte*Tmp,
            data=BDDpanel, 
            ylab="IPC(Deflactado) & Rectas de Regresión", 
            main="IPC(Deflactado)")

#Grafico Efectos Fijos ggplot2 -----------------
xmin = min(BDDpanel$Tmp)
xmax = max(BDDpanel$Tmp)
# TmpFit = seq(xmin, xmax, length.out=200)
TmpFit = BDDpanel$Tmp
PeriodoCorteFit = cut(Anio,
                      breaks = periodos ,
                      labels = etiquetas ,
                      right = F)

predicted <- data.frame(Tmp = TmpFit, PeriodoCorte = PeriodoCorteFit)
# predicted$IPCfit <- predict(modelo1, predicted)
IPCfit = forecast(modelo1,predicted)
predicted$IPCfit = as.numeric(IPCfit$mean)
predicted$Fecha = BDDgraf$Fecha

seriegraf2 + 
  geom_line(data=predicted, 
                       aes(x = Fecha, y = IPCfit,colour=PeriodoCorte ),
                       size=0.8)+
  theme(
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

# linPanel = ggplot(data = predicted, aes(x = Tmp, y = IPCfit,colour=PeriodoCorte )) +
#   geom_line(size = 0.7)

