#######################################################################
# ------------  COMPARATIVA ESTACIONES METEOROLOGICAS    -------------
# ------------                  vs                       -------------
# ------------                  ERA                       -------------
#Media horaria

# Grilla de concentraciones para hacer buffer
grilla <- st_read("D:/Josefina/Proyectos/salud/movilidad_7/grillas/grilla_00.shp")
# Puntos de las estaciones meteo de Mendoza
estaciones<- st_read("D:/Josefina/Proyectos/ERA/estaciones/estaciones.shp")
# Datos de las estaciones
meteo<- read.csv("D:/Josefina/Proyectos/ERA/estaciones/mendoza_meteo_subst.csv")
# Transformamos a UTC la hora
meteo$day <- paste(meteo$FECHA_LOCAL, " ",meteo$HORA_LOCAL,":00",sep = "")
meteo$day <- as.POSIXct(strptime(meteo$day, format = "%d/%m/%Y %H:%M",tz="America/Argentina/Buenos_Aires"))
meteo <- meteo %>% 
  mutate(fecha_with_tz = with_tz(day, tzone = "UTC")) 
# Datos de ERA
variable_ERA <- "t2m"
path <- paste("D:/Josefina/Proyectos/ERA/dataset/proceed/",variable_ERA,sep = "")
setwd(path)
lista_archivos <- dir(path,pattern = ".csv")

archivo <- read.csv(lista_archivos[1])

df_mean_mes <- data.frame()
for (i in 1:length(lista_archivos)){
  print(paste("archivo num", i))
  archivo <- read.csv(lista_archivos[i])
  df_mean_dia <- data.frame()
  archivo%>%
    group_by(y,x) %>%  
    group_split() -> dat_agrupado
  for(j in 1:length(dat_agrupado)){
    if(j%%100 == 0){
      print(paste("archivo j", j))
    }
    
    df_grilla <- data.frame(
      x = dat_agrupado[[j]][["x"]][1],
      y = dat_agrupado[[j]][["y"]][1],
      mean = mean(dat_agrupado[[j]][["valor"]],na.rm=T),
      min = min(dat_agrupado[[j]][["valor"]],na.rm=T),
      max = max(dat_agrupado[[j]][["valor"]],na.rm=T),
      unidad = dat_agrupado[[j]][["unidad"]][1],
      nombre_var = dat_agrupado[[j]][["nombre_var"]][1],
      date = dat_agrupado[[j]][["date"]][1])
    df_mean_dia <- rbind(  df_mean_dia,df_grilla)
  }

### Media estaciones
meteo%>%
    group_by(FECHA_LOCAL) %>%  
    group_split() -> dat_agrupado_meteo 
meteo_mean_dia <- data.frame() 
for(p in 1:length(dat_agrupado_meteo)){
  if(p%%100 == 0){
    print(paste("archivo j", p))
  }
 dat_meteo <-  data.frame(date = dat_agrupado_meteo[[p]][["fecha_with_tz"]][1],
  temp =  mean(dat_agrupado_meteo[[p]][["TEMPERATURA...C."]],na.rm=T),
  hr =  mean(dat_agrupado_meteo[[p]][["HUM..REL...."]],na.rm=T),
  tmp_rocio =  mean(dat_agrupado_meteo[[p]][["TEMP..ROCIO"]],na.rm=T),
  p_estacion =  mean(dat_agrupado_meteo[[p]][["PRESION.ESTACION..hPa."]],na.rm=T),
  p_mar =  mean(dat_agrupado_meteo[[p]][["PRESION.NIVEL.DEL.MAR..hPa."]],na.rm=T))
 meteo_mean_dia <- rbind( meteo_mean_dia,dat_meteo)
  
}

archivo<-df_mean_dia
#Pongo en formato la fecha de ERA
#archivo$day <- as.POSIXct(strptime(archivo$fecha, format = "%Y-%m-%d %H:%M:%S",tz="UTC"))
archivo$day <- as.POSIXct(strptime(archivo$date, format = "%Y-%m-%d",tz="UTC"))

#Hago la cuadricula de ERA
coordinates(archivo) <- ~x+y
proj4string(archivo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeOGR(archivo,".","temp2", driver="ESRI Shapefile")
point_era <- st_read("temp2.shp",quiet = TRUE) 
point_era_UTM = st_transform(point_era, 32519)
# st_crs(point_era)
buffer.points <-st_buffer(point_era_UTM , 175)
buffer.points_WGS = st_transform(buffer.points, 4326)
#07:03 -07-14
#08:09-08:10
interseccion_buffer <- st_intersection(estaciones,buffer.points_WGS)
file.remove(file.path(".", dir(path="." ,pattern="temp.*")))


df_tot <- data.frame()

for (i in 1: nrow(interseccion_buffer)){ #ARCHIVO MAS CORTO
  if (i %% 50 == 0) {
    print (i)
  }
  
  tabla_aeronet <- meteo_mean_dia  #ARCHIVO MAS LARGO
  eq_year <- which(year(tabla_aeronet$date) == year(interseccion_buffer [i,]$day))
  
  tabla_aeronet <- tabla_aeronet[eq_year,] 
  
  eq_month <- which(month(tabla_aeronet$date) == month(interseccion_buffer [i,]$day))
  tabla_aeronet <- tabla_aeronet[eq_month,] 
  
  eq_day <- which(day(tabla_aeronet$date) == day(interseccion_buffer [i,]$day))
  tabla_aeronet <- tabla_aeronet[eq_day,]
  
  #eq_hour <- which(hour(tabla_aeronet$fecha_with_tz) == hour(interseccion_buffer [i,]$day))
  #tabla_aeronet <- tabla_aeronet[eq_hour,]
  
  df <- data.frame(date= tabla_aeronet$date,
                  
                   temp= tabla_aeronet$temp,
                   hr= tabla_aeronet$hr,
                   p.estacion = tabla_aeronet$p_estacion,
                   p.mar= tabla_aeronet$p_mar,
                   estacion = interseccion_buffer$estaciones[i],

                   x = interseccion_buffer $x[i],
                   y = interseccion_buffer $y[i],
                   valor = interseccion_buffer $mean[i],
                   date = interseccion_buffer$date[i],
                   unidad = interseccion_buffer$unidad[i],
                   nombre_var = interseccion_buffer$nombre_var[i])
                   # valor = interseccion_buffer $valor[i],
                   # date = interseccion_buffer $day[i],
                   # unidad = interseccion_buffer $unidad[i],
                   # nombre_var = interseccion_buffer $nombre_var[i])
  df_tot <- rbind(df_tot,df)
}
  
if(variable_ERA=="t2m" || variable_ERA=="d2m"){
  df_tot$valor_2 <- df_tot$valor -  273.15
}

if(variable_ERA=="sp"){
  df_tot$valor_2 <- df_tot$valor /100
}
  
  
  


