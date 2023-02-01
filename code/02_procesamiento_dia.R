################################################################################
# ------------ ESTIMACION DE LAS VARIABLES METEOROLOGICAS DE ERA   -------------     
# Es necesario ingresar un path donde se encuentren todos los datos de ERA 
#Formato .nc.
# Con esta funcion vamos a obtner los datos de diferentes variables meteo en un 
# punto dado y en una fecha determinada
# En el caso de no ingresar coordenadas:latitud y longitud devuelve toda la grilla


process_era5 <- function (path='D:/Josefina/Proyectos/ERA/dataset/',lat=NA,long=NA,fecha_ingresada, tipo){
  df_mean_dia_salida<- data.frame()
  rbind_dia_coords_2 <- data.frame()
  crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  era.df<- data.frame()
  shape <- readOGR("D:/Josefina/Proyectos/ERA/grilla/grilla.shp")
  
  ext = extent(-68.93357, -68.67841, -33.06053, -32.78231)
  raster_template <- raster(nrows = 88, ncols = 68, 
                            crs = crs_project, 
                            ext =ext)
  
  dire <- path
  setwd(dire)
  id_lista <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)
  fecha_era <- substr(id_lista,1,10) 
  fecha_buscada <- which(fecha_era == fecha_ingresada)
  id <- id_lista[fecha_buscada]
  rbind_dia_coords <- data.frame()
  for (i in 1:length(id)){
    

    
    print(paste("Esto es i = ", i, sep= ""))
    file.name = id[i]
    # Get the data sets
    sds <- get_subdatasets(file.name)

    for (num_sds in 1:2){#length(sds)){
      print(sds)
    # Get orbit information
      name_sds<- substring(sds[num_sds],31)
      MIRRAraster <- raster(file.name,varname=name_sds)
      num_bands <- nbands(MIRRAraster)
      # --- For each orbit --- #
      
      era.df <- data.frame()
      #print(Sys.time())
      #24 hs
      
      for (nband in 1:num_bands) {
        if(nband %%20==0){
          print(print(paste("Esto es nband = ", nband, sep= "")))
        }
  
        MIRRAraster <- raster(file.name,varname=name_sds,b=nband)
        #print(Sys.time())# print(sds[1])    
        unidad <-  MIRRAraster@data@unit
        t <- MIRRAraster@z[[1]]
        tiempo <- substr(t,1,10)
        
        
        # 2) Reproyectar
        MIRRAraster2 <- projectRaster(MIRRAraster,
                                      crs = crs_project,
                                      method = "bilinear")
        rst_resampling <- raster::resample(MIRRAraster2, raster_template)
      
        
        # 4) Recortar
        data_recorte <- crop(rst_resampling, shape)  #recorto imagen para CABA + LP
        
        
        df<- raster::as.data.frame(data_recorte, xy = T)
        df$tiempo <- t
        df$unidad <- unidad
        df$variable <-name_sds
        df$date <- substr(df$tiempo,1,10)
        df$hora <- substr(df$tiempo,12,19)
        names(df) <- c("x","y","valor","fecha","unidad","nombre_var","date","hora") 
        era.df <- rbind(era.df,df)
        names(era.df) <-c("x","y","valor","fecha","unidad","nombre_var","date","hora")  
        rm(data_recorte, MIRRAraster, rst_resampling)  
      }
      # Media Diaria
      df_mean_dia <- data.frame()
      era.df%>%
        group_by(y,x) %>%  
        group_split() -> dat_agrupado
      for(j in 1:length(dat_agrupado)){
        if(j%%1000 == 0){
          print(paste("archivo j", j))
        }
        
        df_grilla <- data.frame(
          date = dat_agrupado[[j]][["date"]][1],
          x = dat_agrupado[[j]][["x"]][1],
          y = dat_agrupado[[j]][["y"]][1],
          mean = mean(dat_agrupado[[j]][["valor"]],na.rm=T),
          min = min(dat_agrupado[[j]][["valor"]],na.rm=T),
          max = max(dat_agrupado[[j]][["valor"]],na.rm=T),
          unidad = dat_agrupado[[j]][["unidad"]][1],
          nombre_var = dat_agrupado[[j]][["nombre_var"]][1])
        df_mean_dia <- rbind(  df_mean_dia,df_grilla)
      }
      #df_mean_dia_salida <- rbind(df_mean_dia_salida,df_mean_dia)
      #Hago buffer para tomar las coordenadas ingresadas
    #}
    
    # ############################################################################
    # #                          Hasta acaProcesamiento Obligatorio
    # ############################################################################
    # if (is.na(lat) & tipo = df){
    #   return(df_mean_dia_salida )
    # }
    # if (is.na(lat) & tipo = plot){
    #   #   --- Filtramos dato
    #   mean_dia_subst <- df_mean_dia_salida [df_mean_dia_salida$nombre_var == variable,]
    #   prueba <- grilla_cuadrada(mean_dia_subst)
    #   
    # }
    # else{
      coordinates(df_mean_dia) <- ~x+y
      
      proj4string(df_mean_dia) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      writeOGR(df_mean_dia,".","temp_dia", driver="ESRI Shapefile")
      point_era <- st_read("temp_dia.shp",quiet = TRUE) 
      point_era_UTM = st_transform(point_era, 32519)
      
      buffer.points <-st_buffer(point_era_UTM , 250)
      buffer.points_WGS = st_transform(buffer.points, 4326)
      
      # Convert data frame to sf object
      estacion <- data.frame(lat=lat,long=long)
      my.sf.point <- st_as_sf(x = estacion, 
                              coords = c("long", "lat"),
                              crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      interseccion_buffer <- st_intersection(my.sf.point,buffer.points_WGS)
      df_interseccion_buffer <- data.frame(interseccion_buffer)
      rbind_dia_coords <- rbind(rbind_dia_coords,df_interseccion_buffer)
      file.remove(file.path(".", dir(path="." ,pattern="temp.*")))
      file.remove(file.path(".", dir(path="." ,pattern="temp_dia.*")))
      rbind_dia_coords_salida <- data.frame(fecha = rbind_dia_coords$date[1],
                                            (rbind_dia_coords$mean[1] - 273.15),(rbind_dia_coords$mean[2]- 273.15),
                                            (rbind_dia_coords$mean[3]/100),rbind_dia_coords$mean[4],
                                            rbind_dia_coords$mean[5],rbind_dia_coords$mean[6],
                                            rbind_dia_coords$mean[7])
      
      names(rbind_dia_coords_salida) <- c("fecha",rbind_dia_coords$nombre_var[1],rbind_dia_coords$nombre_var[2],
                                          rbind_dia_coords$nombre_var[3] ,rbind_dia_coords$nombre_var[4],
                                          rbind_dia_coords$nombre_var[5],rbind_dia_coords$nombre_var[6],
                                          rbind_dia_coords$nombre_var[7])
      
      
      
    }
    return(rbind_dia_coords_salida)
  } 
  #return(rbind_dia_coords_salida)

}


# -- Pruebas
prueba <- process_era5 (path='D:/Josefina/Proyectos/ERA/dataset/',lat=-32.91051, 
                        long = -68.864, fecha_ingresada = "2016-07-03" )
prueba_na <- process_era5 (path='D:/Josefina/Proyectos/ERA/dataset/',lat=NA, 
                        long = NA, fecha_ingresada = "2016-07-03" )
05:43
06:00





