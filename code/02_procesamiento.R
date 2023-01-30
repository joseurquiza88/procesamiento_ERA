process_era5 <- function (path){
  #hora_llegada <-  as.POSIXct(strptime(hora_interes  ,format = "%Y-%m-%d %H:%M:%S"))
  #hora <- paste(hour(hora_llegada),":00:00",sep="")

  crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  era.df<- data.frame()
  shape <- readOGR("D:/Josefina/Proyectos/ERA/grilla/grilla.shp")
  
  ext = extent(-68.93357, -68.67841, -33.06053, -32.78231)
  raster_template <- raster(nrows = 88, ncols = 68, 
                            crs = crs_project, 
                            ext =ext)
  
  dire <- path
  setwd(dire)
  id <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)

  for (i in 1:length(id)){
    
    print(paste("Esto es i = ", i, sep= ""))
    file.name = id[i]
    # Get the data sets
    sds <- get_subdatasets(file.name)
    
    for (num_sds in 1:length(sds)){
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
        #timestamp <- lubridate::as_datetime(c(t*60*60), origin = "1900-01-01")
        
        # 2) Reproyectar
        MIRRAraster2 <- projectRaster(MIRRAraster,
                                      crs = crs_project,
                                      method = "bilinear")
        rst_resampling <- raster::resample(MIRRAraster2, raster_template)
        #rst_resampling <- raster::resample(data_recorte, raster_template)
        
        # 4) Recortar
        data_recorte <- crop(rst_resampling, shape)  #recorto imagen para CABA + LP
        #data_recorte <- crop(MIRRAraster2, shape)  #recorto imagen para CABA + LP
        
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
      #era.df_subset <- era.df [era.df$hora == hora,]
      #horario <- substr(hora,1,2)
      #nombre <-paste("./proceed/",id[i],"_",name_sds,"_",horario,".csv",sep = "")
      nombre <-paste("./proceed/",id[i],"_",name_sds,".csv",sep = "")
      
      write.csv(era.df ,nombre)
        
      
    }


  }   

  
}
prueba <- process_era5 (path='D:/Josefina/Proyectos/ERA/dataset/')
hora_interes <- "2023-01-21 14:30:00 -03"



