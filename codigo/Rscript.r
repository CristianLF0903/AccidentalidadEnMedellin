require(pacman)
pacman::p_load(ggplot2, sf, readr)
rm(list = ls())


#dataframe
acc_med <- read_delim("GitHub/AccidentalidadEnMedellin/incidentes_viales.csv", locale = locale(encoding = "UTF-8"),
                      ";", escape_double = FALSE, col_types = cols(`AÑO` = col_character(),
                                                                   FECHA_ACCIDENTES = col_character()),
                      trim_ws = TRUE)
#limpieza
acc_med$AÑO[acc_med$AÑO == "2019\\r"] <- "2019"
acc_med$CLASE_ACCIDENTE[acc_med$CLASE_ACCIDENTE %in% c("Caída de Ocupante","Caida Ocupante", "Caída Ocupante")] <- "Caida de Ocupante"
acc_med$COMUNA <- gsub("\\xC1", "Á", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xF3", "ó", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xED", "í", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xF1", "ñ", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xFA", "ú", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xE1", "á", acc_med$COMUNA, fixed = TRUE)
acc_med$COMUNA <- gsub("\\xE9", "é", acc_med$COMUNA, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xC1", "Á", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xF3", "ó", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xED", "í", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xF1", "ñ", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xFA", "ú", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xE1", "á", acc_med$BARRIO, fixed = TRUE)
acc_med$BARRIO <- gsub("\\xE9", "é", acc_med$BARRIO, fixed = TRUE)
acc_med$GRAVEDAD_ACCIDENTE <- gsub("\\xF1", "ñ", acc_med$GRAVEDAD_ACCIDENTE, fixed = TRUE)

##creaccion de fecha
acc_med <- cbind(acc_med, 
                 strcapture('(\\d*[/]\\d*[/]\\d*)',
                            acc_med$FECHA_ACCIDENTE, data.frame(FECHA = character())))

#dbExport <- data.frame(FECHA = acc_med$FECHA, CLASE = acc_med$CLASE_ACCIDENTE, DANOS = acc_med$DAÑOS, LOCACION = acc_med$LOCATION)
#write.csv2(dbExport, 'db-acc-med.csv')

#bd map
barrios <- st_read("GitHub/AccidentalidadEnMedellin/shp/Barrio_Vereda_2014.shp")
barrios <- barrios[ , c(3,5,6,7,8)]

#criterios Clase_acc, gravedad, tipo via, frecuencia

##frecuencia de acc por barrio
freq <- data.frame(table(acc_med$BARRIO))
map1 <- merge(x=freq, y=barrios, by.y="NOMBRE", by.x="Var1")

##freccuencia de acc por clase de accidente
freq_class = data.frame(table(acc_med$CLASE_ACCIDENTE, acc_med$BARRIO))
map2.1 <- merge(x=freq_class[freq_class$Var1=="Atropello", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.2 <- merge(x=freq_class[freq_class$Var1=="Caida de Ocupante", ], y=barrios, by.y="NOMBRE", by.x="Var3")
map2.3 <- merge(x=freq_class[freq_class$Var1=="Choque", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.4 <- merge(x=freq_class[freq_class$Var1=="Incendio", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.5 <- merge(x=freq_class[freq_class$Var1=="Volcamiento", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.6 <- merge(x=freq_class[freq_class$Var1=="Otro", ], y=barrios, by.y="NOMBRE", by.x="Var2")

##frecuencia de acc en tipos de via
freq_vias = data.frame(table(acc_med$DISEÑO,acc_med$BARRIO))

##frecuencia de graveda de acc
freq_grav = data.frame(table(acc_med$GRAVEDAD_ACCIDENTE, acc_med$BARRIO))

##Combinanciones


#mapas
##general
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map1, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Medellin")
##por clases de acc
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.1, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Atropellos")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.2, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Caidas de ocupante")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.3, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Choques")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.4, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Incendios")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.5, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de VOlcamientos")
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.6, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Otros Accidentes")

##tipos de vias
ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("#1E90FF", "blue4"))+
  geom_sf(data = ACCbarrios[ACCbarrios$CLASE_ACC=="Caida de Ocupante", ], aes(geometry = geometry, fill=FRECUENCIA))

##por graveda
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map2.1, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("#FF3030", "red4"))+
  ggtitle("Frecuencia de Atropellos")
