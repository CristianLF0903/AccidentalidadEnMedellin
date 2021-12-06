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
acc_med$DISEÑO <- gsub("\\xF3", "ó", acc_med$DISEÑO, fixed = TRUE)

##creaccion de fecha
acc_med <- cbind(acc_med, 
                 strcapture('(\\d*[/]\\d*[/]\\d*)',
                            acc_med$FECHA_ACCIDENTE, data.frame(FECHA = character())))

dbExport <- data.frame(FECHA = acc_med$FECHA, CLASE = acc_med$CLASE_ACCIDENTE, CLASE_ACC = acc_med$GRAVEDAD_ACCIDENTE, BARRIO = acc_med$BARRIO)
write.csv2(dbExport, 'db-acc-med.csv')

#bd map
barrios <- st_read("GitHub/AccidentalidadEnMedellin/shp/Barrio_Vereda_2014.shp")
barrios <- barrios[ , c(3,5,6,7,8)]

#criterios Clase_acc, gravedad, tipo via, frecuencia

##frecuencia de acc por barrio
freq <- data.frame(table(acc_med$BARRIO))
freq <- freq[order(freq$Freq, decreasing = TRUE), ]
map1 <- merge(x=freq, y=barrios, by.y="NOMBRE", by.x="Var1")
write.csv(freq, 'freq.csv')

##freccuencia de acc por clase de accidente
freq_class = data.frame(table(acc_med$CLASE_ACCIDENTE, acc_med$BARRIO))
freq_class <- freq_class[order(freq_class$Freq, decreasing = TRUE), ]
write.csv2(freq_class, 'freq-class.csv')
map2.1 <- merge(x=freq_class[freq_class$Var1=="Atropello", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.2 <- merge(x=freq_class[freq_class$Var1=="Caida de Ocupante", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.3 <- merge(x=freq_class[freq_class$Var1=="Choque", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.4 <- merge(x=freq_class[freq_class$Var1=="Incendio", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.5 <- merge(x=freq_class[freq_class$Var1=="Volcamiento", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map2.6 <- merge(x=freq_class[freq_class$Var1=="Otro", ], y=barrios, by.y="NOMBRE", by.x="Var2")

##frecuencia de acc en tipos de via
freq_vias = data.frame(table(acc_med$DISEÑO,acc_med$BARRIO))
freq_vias <- freq_vias[order(freq_vias$Freq, decreasing = TRUE), ]
write.csv2(freq_vias, 'freq-vias.csv')
map3.1 <- merge(x=freq_vias[freq_vias$Var1=="Cliclo Ruta", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.2 <- merge(x=freq_vias[freq_vias$Var1=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.3 <- merge(x=freq_vias[freq_vias$Var1=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.4 <- merge(x=freq_vias[freq_vias$Var1=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.5 <- merge(x=freq_vias[freq_vias$Var1=="Paso a nivel", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.6 <- merge(x=freq_vias[freq_vias$Var1=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.7 <- merge(x=freq_vias[freq_vias$Var1=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.8 <- merge(x=freq_vias[freq_vias$Var1=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.9 <- merge(x=freq_vias[freq_class$Var1=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.10 <- merge(x=freq_vias[freq_vias$Var1=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.11 <- merge(x=freq_vias[freq_vias$Var1=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map3.12 <- merge(x=freq_vias[freq_vias$Var1=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var2")

##frecuencia de gravedad de acc
freq_grav = data.frame(table(acc_med$GRAVEDAD_ACCIDENTE, acc_med$BARRIO))
freq_grav <- freq_grav[order(freq_grav$Freq, decreasing = TRUE), ]
write.csv2(data.frame(Var1 = freq_grav$Var1, Var2 = freq_grav$Var2, Freq = freq_grav$Freq), 'freq-gavedad.csv')
map4.1 <- merge(x=freq_grav[freq_grav$Var1=="Con heridos", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map4.2 <- merge(x=freq_grav[freq_grav$Var1=="Con muertos", ], y=barrios, by.y="NOMBRE", by.x="Var2")
map4.3 <- merge(x=freq_grav[freq_grav$Var1=="Solo daños", ], y=barrios, by.y="NOMBRE", by.x="Var2")

##Combinanciones
###freq de clase de acc en tipo de via
freq_comb1 = data.frame(table(acc_med$CLASE_ACCIDENTE, acc_med$DISEÑO, acc_med$BARRIO))
freq_comb1 <- freq_comb1[order(freq_comb1$Freq, decreasing = TRUE), ]
write.csv2(freq_comb1, "freq1.csv")

freq_comb1.1 = freq_comb1[freq_comb1$Var1 == "Atropello", 2:4] ##atroprellos
mapcomb1.1.2 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.3 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.4 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.6 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.7 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.8 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.9 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.10 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.11 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.1.12 <- merge(x=freq_comb1.1[freq_comb1.1$Var2=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb1.2 = freq_comb1[freq_comb1$Var1 == "Caida de Ocupante", 2:4] ##caidas
mapcomb1.2.2 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.3 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.4 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.6 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.7 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.8 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.9 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.10 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.11 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.2.12 <- merge(x=freq_comb1.2[freq_comb1.2$Var2=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb1.3 = freq_comb1[freq_comb1$Var1 == "Choque", 2:4] ##choques
mapcomb1.3.2 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.3 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.4 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.6 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.7 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.8 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.9 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.10 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.11 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.3.12 <- merge(x=freq_comb1.3[freq_comb1.3$Var2=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb1.4 = freq_comb1[freq_comb1$Var1 == "Incendio", 2:4] ##incendios
mapcomb1.4.2 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.3 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.4 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.6 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.7 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.8 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.9 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.10 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.11 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.4.12 <- merge(x=freq_comb1.4[freq_comb1.4$Var2=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb1.6 = freq_comb1[freq_comb1$Var1 == "Otro", 2:4] ##otros
mapcomb1.6.2 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Glorieta", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.3 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Interseccion", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.4 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Lote o Predio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.6 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Paso Elevado", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.7 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Paso Inferior", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.8 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Pontón", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.9 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Puente", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.10 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Tramo de via", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.11 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Tunel", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb1.6.12 <- merge(x=freq_comb1.6[freq_comb1.6$Var2=="Via peatonal", ], y=barrios, by.y="NOMBRE", by.x="Var3")

###freq de gravedad en clase de acc
freq_comb2 = data.frame(table(acc_med$GRAVEDAD_ACCIDENTE, acc_med$CLASE_ACCIDENTE, acc_med$BARRIO))
freq_comb2 <- freq_comb2[order(freq_comb2$Freq, decreasing = TRUE), ]
write.csv2(freq_comb2, "freq2.csv")

freq_comb2.1 = freq_comb2[freq_comb2$Var1 == "Con heridos", 2:4] ##con heridos
mapcomb2.1.1 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Atropello", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.1.2 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Caida de Ocupante", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.1.3 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Choque", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.1.4 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Incendio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.1.5 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Volcamiento", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.1.6 <- merge(x=freq_comb2.1[freq_comb2.1$Var2=="Otro", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb2.2 = freq_comb2[freq_comb2$Var1 == "Con muertos", 2:4] ##con muertos
mapcomb2.2.1 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Atropello", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.2.2 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Caida de Ocupante", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.2.3 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Choque", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.2.4 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Incendio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.2.5 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Volcamiento", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.2.6 <- merge(x=freq_comb2.2[freq_comb2.2$Var2=="Otro", ], y=barrios, by.y="NOMBRE", by.x="Var3")

freq_comb2.3 = freq_comb2[freq_comb2$Var1 == "Solo daños", 2:4] ##solo daños
mapcomb2.3.1 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Atropello", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.3.2 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Caida de Ocupante", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.3.3 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Choque", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.3.4 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Incendio", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.3.5 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Volcamiento", ], y=barrios, by.y="NOMBRE", by.x="Var3")
mapcomb2.3.6 <- merge(x=freq_comb2.3[freq_comb2.3$Var2=="Otro", ], y=barrios, by.y="NOMBRE", by.x="Var3")

#mapas
##general
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map1, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Medellin")

##comb2

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("white","green","green4"))+
  geom_sf(data = mapcomb2.3.6, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Otros con solo daños")

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
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.1, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Cliclo Ruta")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.2, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Glorieta")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.3, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Interseccion")


ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.4, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Lote o Predio")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.5, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Paso a Nivel")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.6, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Paso Elevado")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.7, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Paso Inferior")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.8, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Pontón")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.9, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Puente")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.10, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Tramo de via")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.11, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Tunel")

ggplot() +
  geom_sf(data =medellin ,fill="white", color = "black")+
  scale_fill_gradientn(colours = c("orange", "brown"))+
  geom_sf(data = map3.12, aes(geometry = geometry, fill=Freq))+
  ggtitle("Frecuencia de Accidentes en Via Peatonal")


##por gravedad
ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map3.1, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("green", "green4"))+
  ggtitle("Frecuencia de Accidentes con heridos")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map3.2, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("green", "green4"))+
  ggtitle("Frecuencia de Accidentes con muertos")

ggplot() +
  geom_sf(data =barrios ,fill="white", color = "black")+
  geom_sf(data = map3.3, aes(geometry = geometry, fill=Freq))+
  scale_fill_gradientn(colours = c("green", "green4"))+
  ggtitle("Frecuencia de Accidentes con solo daños")