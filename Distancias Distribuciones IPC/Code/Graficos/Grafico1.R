#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!  GRAFICAS INDIVIDUALES 1  !!!!!!!!!!!!!!
#------------------------------------------------------------
# Graficos del IPC sin deflactar, Regresion Simple

graf=ggplot(data = BDDgraf, aes(x = Fecha, y = SerieOrig)) +
geom_line(size = 0.7,colour = "black") +  
  # annotate("text", label = "Within SLA", x = 1, y = 2)+
  # ggtitle("Diagrama de dispersión") +
  geom_smooth(method = "lm",color="red") +theme_minimal()+
  # theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("IPC:", productos[k]) , y = "IPC")

  plot(graf)

               