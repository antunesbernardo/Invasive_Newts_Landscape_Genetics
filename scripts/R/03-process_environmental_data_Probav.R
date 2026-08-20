library(terra)

for(i in c("Veluwe", "Isen", "Tubingen")){
  
  study_area <- vect(paste0("data/created/vectors/",i,"/study_area.shp"))
  
  list_vars <- list.files(paste0("data/created/rasters/",i),
                          full.names = T)
  
  rst <- rast(list_vars)
  rst[is.na(rst)] <- 0
  
  rst_topo <- terrain(rst$Elevation, v = c("slope"))
  names(rst_topo) <- c("Slope")
  rst <- c(rst, rst_topo)
  rst <- mask(rst, study_area)
  
  rst_mean <- focal(rst, w = 3, fun = mean, na.rm = TRUE) # 3 x 100 = 300 meters
  rst_sd   <- focal(rst, w = 3, fun = sd,   na.rm = TRUE)

  names(rst_sd) <- paste0(names(rst_sd), "_sd") 

  rst <- c(rst_mean, rst_sd)

  writeRaster(rst, paste0("data/created/rasters/",i,"/All_environmental_variables.tif"))
}

# # Explore
# rst_stack <- raster::stack(rst)
# mapview::mapview(rst_stack$Woodland,
#                  maxpixels =  592130)
