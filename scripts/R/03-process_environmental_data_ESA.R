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
    value = c(10,20,30,40,50,60,70,80,90,95,100),
    label = c("Tree_cover", "Shrubland", "Grassland", "Cropland",
              "Built_up", "Bare", "Snow", "Permanent_water",
              "Herbaceous_wetland", "Mangroves", "Moss_and_lichen"))
  rst_landcover_by_cat <- rast()
  
  for(k in 1:nrow(class_df)){
    
    rst_tmp <- rst_landcover
    rst_tmp[] <- 0
    rst_tmp[rst_landcover == class_df[k,"value"]] <- 100
    rst_tmp <- project(rst_tmp, rst[[1]])
    names(rst_tmp) <- class_df[k,"label"]
    rst_landcover_by_cat <- c(rst_landcover_by_cat, rst_tmp)
  }
  rst_landcover_by_cat <- rst_landcover_by_cat[[c("Tree_cover", "Grassland",
                                                  "Cropland", "Built_up","Bare")]]
  rst <- c(rst_topo, rst_landcover_by_cat)
  rst <- mask(rst, study_area)
  
  rst_mean <- focal(rst, w = 9, fun = mean, na.rm = TRUE) # 9 x 30 = 270 meters
  rst_sd   <- focal(rst, w = 9, fun = sd,   na.rm = TRUE)
  names(rst_sd) <- paste0(names(rst_sd), "_sd") 
  
  rst <- c(rst_mean, rst_sd)
  writeRaster(rst, paste0("data/created/rasters/",i,"/All_environmental_variables.tif"))
}

# # Explore
# rst_stack <- raster::stack(rst)
# mapview::mapview(rst_stack$Woodland,
#                  maxpixels =  592130)
