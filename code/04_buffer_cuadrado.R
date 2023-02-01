################################################################################
# ------------        FUNCION PARA HACER UNA GRILLA CUADRADA       ------------- 

grilla_cuadrada <- function(df){
  coordinates(df) <- ~x+y
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  writeOGR(df,".","temporal", driver="ESRI Shapefile")
  punto <- st_read("./temporal.shp",quiet = TRUE)
  st_crs(punto) = 4326  
  
  
  df_trans<-spTransform(df, CRS("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  buffer_square <- function(point, length, degree = 360){
    return(buffer_rectangle(point, length, length, degree))  
  }
  
  buffer_rectangle <- function(point, x_length, y_length, degree =360){
    
    #Error handlers for input type
    if(!("sf" %in% class(point))) stop("Input one point of class sf")
    if(nrow(point) != 1) stop("Input one point of class sf")
    if(!sf::st_is(point, "POINT")) stop("Input one point of class sf")
    
    if(!is.numeric(degree) || degree < 0 || degree > 360){
      stop("Input degree as numeric between 0 and 360")
    }
    
    #function starts
    point_coordinates <- sf::st_coordinates(point)
    
    radians <- degree #* 0.0174532925
    
    #create an empty matrix
    pts_df = as.data.frame(matrix(nrow = 4, ncol = 2))
    colnames(pts_df) <- c("x", "y")
    
    #set the points
    pts_df[1,1] <- (point_coordinates[1] - x_length / 2) 
    pts_df[1,2] <- (point_coordinates[2] + y_length / 2)
    
    pts_df[2,1] <- (point_coordinates[1] + x_length / 2) 
    pts_df[2,2] <- (point_coordinates[2] + y_length / 2)
    
    pts_df[3,1] <- (point_coordinates[1] - x_length / 2) 
    pts_df[3,2] <- (point_coordinates[2] - y_length / 2) 
    
    pts_df[4,1] <- (point_coordinates[1] + x_length / 2) 
    pts_df[4,2] <- (point_coordinates[2] - y_length / 2) 
    
    #convert to sf
    pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"), crs = sf::st_crs(point))
    
    ##create the convex hull
    rectangular_sf <- sf::st_convex_hull(sf::st_union(pts_sf))
    rotation_f = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    (rectangular_sf - point_coordinates) * rotation_f(radians) + point_coordinates -> rectangular_sf
    
    #return
    return(rectangular_sf)  
  }
  rbind_buffer<- data.frame()
  for (i in 1:nrow(df_trans)){
    if(i%%100 == 0){
      print(paste("archivo i", i))
    }
    example_point = sf::st_point(a)
    example_point = sf::st_sfc(a)
    example_point = sf::st_sf(a)
    
    c <- example_point3[i,1]
    buffer_square(c, 350, 0) -> square_shaped_buffer
    sf_object = square_shaped_buffer %>%
      st_sf %>%
      st_cast
    rbind_buffer <- rbind(rbind_buffer,sf_object)
    
  }
  
  st_crs(rbind_buffer) =32519
  salida<-st_transform(rbind_buffer,crs = 4326)
  interseccion_punto_ERA <- st_intersection(punto,salida)
  m
  return(salida)
  #st_write(salida,"rbind_buffer_final5.shp")

}

prueba <- grilla_cuadrada(point_era_UTM)

st_write(prueba ,"rbind_buffe.shp")
