library(terra)

for(i in c("Veluwe", "Isen", "Tubingen")){
  
  study_area <- vect(paste0("data/created/vectors/",i,"/study_area.shp"))
  
  list_vars <- list.files(paste0("data/created/rasters/",i),
                          full.names = T)
  
  rst <- rast(list_vars)

  rst_topo <- terrain(rst$Elevation, v = c("slope"))
  names(rst_topo) <- c("Slope")

  rst_landcover <- rst$Landcover
  class_df <- data.frame(
    value = 1:8,
    label = c("Artificial", "Cropland", "Woodland", "Shrubland",
              "Grassland", "Bareland", "Water_permanent", "Wetland"))
  
  rst_landcover_by_cat <- rast()
  
  for(k in 1:nrow(class_df)){
    
    rst_tmp <- rst_landcover
    rst_tmp[] <- 0
    rst_tmp[rst_landcover == class_df[k,"value"]] <- 100
    rst_tmp <- project(rst_tmp, rst[[1]])
    names(rst_tmp) <- class_df[k,"label"]
    rst_landcover_by_cat <- c(rst_landcover_by_cat, rst_tmp)
  }
  rst <- c(rst_topo, rst_landcover_by_cat)
  rst <- mask(rst, study_area)
  
  rst_mean <- focal(rst, w = 3, fun = mean, na.rm = TRUE) # 25 x 10 = 250 meters
  rst_sd   <- focal(rst, w = 3, fun = sd,   na.rm = TRUE)
  names(rst_sd) <- paste0(names(rst_sd), "_sd") 
  
  rst <- c(rst_mean, rst_sd)
  writeRaster(rst, paste0("data/created/rasters/",i,"/All_environmental_variables.tif"),
              overwrite = T)
}

# # Explore
# rst_stack <- raster::stack(rst)
# mapview::mapview(rst_stack$Woodland,
#                  maxpixels =  592130)
