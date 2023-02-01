################################################################################
# ------------ ESTIMACION DE LAS VARIABLES METEOROLOGICAS DE ERA   -------------     
# Es necesario ingresar un path donde se encuentren todos los datos de ERA 
#Formato .nc.
# Con esta funcion vamos a obtner los datos de diferentes variables meteo en un 
# punto dado y en una fecha determinada
# En el caso de no ingresar coordenadas:latitud y longitud devuelve toda la grilla


process_era5_grilla <- function (path='D:/Josefina/Proyectos/ERA/dataset/',lat=NA,long=NA,fecha_ingresada, tipo=NA,var_interes=NA){
  df_mean_dia_salida<- data.frame()

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
      df_mean_dia_salida <- rbind(df_mean_dia_salida,df_mean_dia)
      #Hago buffer para tomar las coordenadas ingresadas
      }
      
      # ############################################################################
      # #                          Hasta acaProcesamiento Obligatorio
      # ############################################################################
    if (tipo == "df"){
      return(df_mean_dia_salida )
    }
    #   ---
    if (tipo == "plot"){
      #   --- .shp con toda la grilla segun variable de interes
      mean_dia_subst <- df_mean_dia_salida [df_mean_dia_salida$nombre_var == var_interes,]
      
      buffer_mean_dia <- grilla_cuadrada(mean_dia_subst)
      return(buffer_mean_dia )
    }
  }
  
   
   
  #return(rbind_dia_coords_salida)
  
}
prueba_plot <- process_era5_grilla (path='D:/Josefina/Proyectos/ERA/dataset/',lat=NA, 
                        long = NA, fecha_ingresada = "2016-07-03",tipo = "plot",var_interes <- "t2m")
prueba_df <- process_era5 (path='D:/Josefina/Proyectos/ERA/dataset/',lat=NA, 
                        long = NA, fecha_ingresada = "2016-07-03",tipo = "df",var_interes <- "t2m")
