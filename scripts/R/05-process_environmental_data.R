library(terra)

for(i in c("Veluwe", "Isen", "Tubingen")){
  
  study_area <- vect(list.files("data/created/vectors/",
                                pattern = paste0(i,".shp"),
                                full.names = T))
  
  list_vars <- list.files(paste0("data/created/rasters/",i),
                          full.names = T)
  
  rst <- rast(list_vars[c(4,9,6,1,10,7,2)])

  rst_topo <- terrain(rst$Elevation, v = c("slope",
                                            "TRI"))
  names(rst_topo) <- c("Slope", "TRI")
  rst <- c(rst, rst_topo)

  rst_landcover <- rast(list_vars[c(5)])
  class_df <- data.frame(
    value = 1:8,
    label = c("Artificial_land", "Cropland", "Woodland", "Shrubland",
              "Grassland", "Bare_land", "Water_permanent", "Wetland"))
  rst_landcover_by_cat <- rast()
  
  for(k in 1:nrow(class_df)){
    
    rst_tmp <- rst_landcover
    rst_tmp[] <- 0
    rst_tmp[rst_landcover == class_df[k,"value"]] <- 100
    rst_tmp <- project(rst_tmp, rst[[1]])
    names(rst_tmp) <- class_df[k,"label"]
    rst_landcover_by_cat <- c(rst_landcover_by_cat, rst_tmp)
  }
  rst <- c(rst, rst_landcover_by_cat)
  rst <- mask(rst, study_area)
  plot(rst)
  writeRaster(rst, paste0("data/created/rasters/",i,"/All_environmental_variables.tif"))
}

rst_stack <- raster::stack(rst)
mapview::mapview(rst_stack)
