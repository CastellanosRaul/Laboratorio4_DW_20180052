library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)
distribuidora <- read.csv("tabla_completa.csv",encoding = 'latin1')
distribuidora <- distribuidora[,-1]

Ubicaciones <- split(distribuidora, distribuidora$UBICACION)
Viaje <- distribuidora %>% group_by(CLIENTE) %>% summarise(C=n())
Clientes <- Viaje[,1]

distribuidora$Motivo_Viaje <- ifelse(distribuidora$CLIENTE == "CHICHARRONERIA EL RICO COLESTEROL |||Faltante" |
                                       distribuidora$CLIENTE == "EL PINCHE OBELISCO |||Faltante" | 	
                                       distribuidora$CLIENTE == "POLLO PINULITO|||FALTANTE" | 
                                       distribuidora$CLIENTE == "TAQUERIA EL CHINITO |||Faltante" | 
                                       distribuidora$CLIENTE == "UBIQUO LABS |||FALTANTE", "Faltante",
                                     ifelse(distribuidora$CLIENTE == "EL GALLO NEGRO |||DEVOLUCION", "Devolucion",
                                            ifelse(distribuidora$CLIENTE =="SPORTA, S.A./Despacho a cliente |||Faltante", "Despacho y Faltante","Despacho")))

distribuidora$CLIENTES <- ifelse(distribuidora$CLIENTE == "EL GALLO NEGRO / Despacho a cliente" |
                                   distribuidora$CLIENTE == "EL GALLO NEGRO |||DEVOLUCION", "El Gallo negro",
                                 ifelse(distribuidora$CLIENTE == "EL PINCHE OBELISCO / Despacho a cliente" |
                                          distribuidora$CLIENTE == "EL PINCHE OBELISCO |||Faltante", "El Pinche",
                                        ifelse(distribuidora$CLIENTE == "POLLO PINULITO/Despacho a cliente" |
                                                 distribuidora$CLIENTE == "POLLO PINULITO|||FALTANTE", "Pollo Pinulito",
                                               ifelse(distribuidora$CLIENTE == "TAQUERIA EL CHINITO" |
                                                        distribuidora$CLIENTE == "TAQUERIA EL CHINITO |||Faltante", "Taqueria el Chinito",
                                                      ifelse(distribuidora$CLIENTE == "UBIQUO LABS" |
                                                               distribuidora$CLIENTE == "UBIQUO LABS |||FALTANTE", "UBIQUO Labs",
                                                             ifelse(distribuidora$CLIENTE == "TIENDA LA BENDICION / Despacho a cliente", "Tienda la Bendicion",
                                                                    ifelse(distribuidora$CLIENTE == "SPORTA, S.A./Despacho a cliente |||Faltante", "Sporta",
                                                                           ifelse(distribuidora$CLIENTE == "HOSPITAL ROOSEVELT / Despacho a cliente", "Hospital Roosevelt",
                                                                                  ifelse(distribuidora$CLIENTE == "HOSPITAL LAS AMERICAS", "Hospital las Americas",
                                                                                         ifelse(distribuidora$CLIENTE == "CHICHARRONERIA EL RICO COLESTEROL |||Faltante", "Chicharronera el rico colesterol",
                                                                                                ifelse(distribuidora$CLIENTE == "BAR LA OFICINA", "Bar la Oficina",
                                                                                                       ifelse(distribuidora$CLIENTE == "ABARROTERIA EBENEZER/Despacho a cliente", "Abarroteria Ebenezer",
                                                                                                              "Universidad Francisco Marroquin"))))))))))))

Clientes1 <- distribuidora %>% group_by(CLIENTES) %>% summarise(n())
write.xlsx(Clientes1, "clientes.xlsx")
CPilotos <- distribuidora %>% group_by(PILOTO) %>% summarise(n())
write.xlsx(CPilotos, "cpilotos.xlsx")
# Motivo de enntrega ------------------------------------------------------

motivo <- split(distribuidora, distribuidora$Motivo_Viaje)

# Camiones por mes --------------------------------------------------------
Camion <- distribuidora %>% group_by(MES, UNIDAD) %>% summarise(Cantidad = n())


# Unidades por Camion -----------------------------------------------------
Unidades <- distribuidora %>% group_by(UNIDAD) %>% summarise(Cantidad_Total = sum(CANTIDAD))
Unidades$Porcentaje <- round(Unidades[,2]/sum(Unidades$Cantidad_Total),2)


# Total de camiones utilizados --------------------------------------------
Camiones <- distribuidora %>% group_by(UNIDAD) %>% summarise(Cantidad_Camiones = n())
write.xlsx(Camiones, "Camiones.xlsx")

# Pilotos -----------------------------------------------------------------
Pilotos <- distribuidora %>% group_by(PILOTO,UNIDAD) %>% summarise(Cantidad_Viajes = n()) %>% arrange(desc(Cantidad_Viajes))
Pilotos1 <- Pilotos %>% spread(UNIDAD, Cantidad_Viajes)
Pilotos1$TotalViajes <- rowSums(Pilotos1[,2:4])
Pilotos1 <- Pilotos1 %>% arrange(desc(TotalViajes))

EF <- distribuidora[distribuidora$Motivo_Viaje == "Despacho" | distribuidora$Motivo_Viaje == "Faltante",]
Pilotos2 <- EF %>% group_by(PILOTO, Motivo_Viaje) %>% summarise(CantidadQ = sum(Q))
PilotosR <- Pilotos2 %>% spread(Motivo_Viaje, CantidadQ)
PilotosR$Porcentaje <- round(PilotosR$Faltante/PilotosR$Despacho,2)
PilotosR <- PilotosR %>% arrange(desc(Porcentaje))

PilotoRP <- PilotosR %>% select(PILOTO, Porcentaje)
Pilotos2$CantidadQ <- format(Pilotos2$CantidadQ, big.mark = ",")

PilotoFaltante <- motivo$Faltante
PilotoFaltante1 <- PilotoFaltante %>% group_by(PILOTO) %>% summarise(CantidadViajes = n()) %>% arrange(CantidadViajes)
PilotoFaltante2 <- PilotoFaltante %>% group_by(UNIDAD) %>% summarise(CantidadViajes = n()) %>% arrange(CantidadViajes)

PilotoFaltante1$Porcentaje <- PilotoFaltante1$CantidadViajes/sum(PilotoFaltante1$CantidadViajes)
PilotoFaltante2$Porcentaje <- PilotoFaltante2$CantidadViajes/sum(PilotoFaltante1$CantidadViajes)

write.xlsx(PilotoFaltante1, "Faltante1.xlsx")
write.xlsx(PilotoFaltante2, "Faltante2.xlsx")
write.xlsx(as.data.frame(Pilotos1), "Pilotos1.xlsx")


# Cantidad de camiones al mes ---------------------------------------------
Interv <- c(0,501,1001,1997)
distribuidora$Intervalo <- cut(distribuidora$CANTIDAD, breaks = Interv, right = FALSE)
Cantidad_Camiones <- distribuidora %>% group_by(MES, Intervalo, UNIDAD) %>% summarise(Cantidad_Enviada = sum(CANTIDAD))
Cantidad_Camiones <- Cantidad_Camiones[,-2]


# Precio por unidad enviada -----------------------------------------------

Precio <- distribuidora$Q/distribuidora$CANTIDAD
Precio <- unique(Precio) # Solo existe un precio único y es 0.25 centavos


# Precio Nuevo ------------------------------------------------------------
distribuidora$Precio <- ifelse(distribuidora$UNIDAD == "Camion Grande", 0.45,
                               ifelse(distribuidora$UNIDAD == "Camion Pequeño", 0.35, 0.25))

# Cantidad de unidades entregadas a los clientes --------------------------
Clientes2 <- distribuidora %>% group_by(CLIENTES, Motivo_Viaje) %>% summarise(CantidadOrdenada = sum(CANTIDAD)) %>% arrange(desc(CantidadOrdenada))
Clientes2 <- Clientes2 %>% spread(Motivo_Viaje, CantidadOrdenada)
Clientes2$Devolucion <- Clientes2$Devolucion*-1
Clientes2$Unidades <- rowSums(Clientes2[,2:5], na.rm = TRUE)
ClientesDespacho <- Clientes2 %>% select(CLIENTES, Unidades) %>% arrange(desc(Unidades))
ClientesDespacho$Ingresos <- ClientesDespacho$Unidades*Precio
ClientesDespacho$Unidades <- format(ClientesDespacho$Unidades, big.mark = ",")
ClientesDespacho$Ingresos <- format(ClientesDespacho$Ingresos, big.mark = ",")

# Clientes 80-20 ----------------------------------------------------------
Clientes8020 <- distribuidora %>% group_by(CLIENTES, Motivo_Viaje) %>% summarise(Ingreso = sum(Q)) %>% arrange(desc(Ingreso))
Clientes8020 <- Clientes8020 %>% spread(Motivo_Viaje, Ingreso)
Clientes8020$Devolucion <- Clientes8020$Devolucion*-1
Clientes8020 <- Clientes8020[,-c(3,5)]
Clientes8020$Venta <- rowSums(Clientes8020[,2:3], na.rm = TRUE)
Clientes8020 <- Clientes8020 %>% arrange(desc(Venta))
Clientes8020 <- Clientes8020[-c(12,13),]
Clientes8020Ex <- Clientes8020[,-c(3)]
write.xlsx(as.data.frame(Clientes8020Ex), "Cliente8020.xlsx")

# Cantidad de unidades entregadas, nuevo precio ---------------------------
Clientes3 <- distribuidora %>% group_by(CLIENTES, Motivo_Viaje, Precio) %>% summarise(CantidadOrdenada = sum(CANTIDAD)) %>% arrange(desc(CantidadOrdenada))
Clientes3$CantidadOrdenada <- ifelse(Clientes3$Motivo_Viaje == "Devolucion", Clientes3$CantidadOrdenada*-1, Clientes3$CantidadOrdenada)
Clientes3 <- Clientes3 %>% spread(Motivo_Viaje,CantidadOrdenada)
Clientes3$Unidades <- rowSums(Clientes3[,3:6], na.rm = TRUE)
Clientes3$Ingresos <- Clientes3$Precio*Clientes3$Unidades
Clientes3 <- Clientes3 %>% select(CLIENTES, Precio, Unidades, Ingresos)
Clientes3Lista <- split(Clientes3, Clientes3$Precio)
Cl <- function(x){
  g <- x[,-2]
  g <- g %>% arrange(desc(Unidades))
  g$Unidades <- format(g$Unidades, big.mark = ",")
  g$Ingresos <- format(g$Ingresos, big.mark = ",")
  return(g)
}
Clientes3Lista1 <- lapply(Clientes3Lista, Cl)

P25 <- Clientes3Lista1$`0.25`
P35 <- Clientes3Lista1$`0.35`
P45 <- Clientes3Lista1$`0.45`

# Grafica Camiones utilizados por mes -------------------------------------
ggplot(Camion, aes(MES, Cantidad, fill = UNIDAD)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~UNIDAD) +
  scale_x_continuous(breaks = Camion$MES, labels = Camion$MES) +
  labs(title = "Gráficas del tipo de camion utilizados por mes") +
  geom_text(aes(label = Cantidad), position = position_dodge(width = 0.5), vjust = -0.25)

# Grafica unidades despachadas al final del año ---------------------------
ggplot(ClientesDespacho, aes(Unidades, Ingresos, fill = CLIENTES)) +
  geom_bar(stat = "identity") +
  labs(title = "Unidades enviadas a los clientes", 
       subtitle = "Ingresos expresados en quetzales",
       caption = "Todas los tipos de trnasporte incluido, precio de 0.25 centavos")

# Grafica unidades despachadas, precios nuevos ----------------------------
ggplot(P25, aes(Unidades, Ingresos, fill = CLIENTES)) +
  geom_bar(stat = "identity") +
  labs(title = "Unidades enviadas a los clientes", 
       subtitle = "Ingresos expresados en quetzales",
       caption = "Unidad: Panel, precio de 0.25 centavos")

ggplot(P35, aes(Unidades, Ingresos, fill = CLIENTES)) +
  geom_bar(stat = "identity") +
    labs(title = "Unidades enviadas a los clientes", 
       subtitle = "Ingresos expresados en quetzales",
       caption = "Unidad: Camion pequeño, precio de 0.35 centavos")

ggplot(P45, aes(Unidades, Ingresos, fill = CLIENTES)) +
  geom_bar(stat = "identity") +
  labs(title = "Unidades enviadas a los clientes", 
       subtitle = "Ingresos expresados en quetzales",
       caption = "Unidad: Camion grande, precio de 0.45 centavos")


# Grafica Robo pilotos ------------------------------------------------------------
ggplot(Pilotos2, aes(PILOTO, CantidadQ, fill = Motivo_Viaje)) +
  geom_col(position = "dodge") + 
  labs(title = "Despachos vs Faltantes por Piloto", caption = "Cifras de cantidad expresadas en quetzales")

ggplot(PilotoRP, aes(PILOTO, Porcentaje, fill=PILOTO)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Porcentaje), position = position_dodge(width = 0.5), vjust = -0.25) + 
  labs(title = "Porcentaje de las unitdades de faltante sobre las unidades de despacho")



