#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!  GRAFICAS INDIVIDUALES 3  !!!!!!!!!!!!!!
#------------------------------------------------------------
# Graficos del IPC Deflactado, Regresion Panel

BDDgraf1 = BDDgraf 
deflactAux = "MM12(IPC General)"

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


#Grafico Densidades  ------------------------------------
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
