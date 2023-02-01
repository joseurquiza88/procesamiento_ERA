
#######################################################################
# ------------             DESCARGA Y PROCESAMIENTO DATOS ERA     -------------
#https://confluence.ecmwf.int/display/CUSF/Download+CDS+ERA5+data+using+R
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview
download_era5 <- function(fecha,uid,key, path){
  fecha<- fecha
  hora_format <-  as.POSIXct(strptime(fecha, format = "%Y-%m-%d"))
  year <- year(hora_format)
  month <- month(hora_format)
  day <- day(hora_format)
  
  name <- paste(fecha,"_download.nc",sep="")
  cds.key <- key #"Insert_your_key_here",
  wf_set_key(user = uid , key = cds.key, service = "cds") #"Insert_your_CDS_UID_here",
  
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type   = "reanalysis",
    format = "netcdf",
    variable = c("2m_temperature",'2m_dewpoint_temperature','surface_pressure','10m_u_component_of_wind', '10m_v_component_of_wind', #'2m_dewpoint_temperature',
                'boundary_layer_height', 'total_precipitation'),#, This parameter is the temperature to which the air, at 2 metres above the surface of the Earth, would have to be cooled for saturation to occur. It is a measure of the humidity of the air. Combined with temperature and pressure, it can be used to calculate the relative humidity.
    year = year,
    month = month,
    day = day,
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", 
             "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
             "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", 
             "21:00", "22:00", "23:00"),
    # area is specified as N, W, S, E
    area = c(-32.78, -68.93, -33.05, -68.68),
    target = name
  )
  
  file <- wf_request(user = uid ,#"Insert_your_CDS_UID_here",
                     request = request,
                     transfer = TRUE,
                     path = path,
                     verbose = TRUE)
}
# hasta 2 dias para atras
download_era5 (fecha = "2023-01-27",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-02",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-03",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-04",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-05",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-06",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
download_era5 (fecha = "2018-01-07",uid ="134318",path = "D:/Josefina/Proyectos/ERA/dataset",key="2045f203-20ba-43fc-b338-e242d5431485")
