library(terra)

for(i in c("Veluwe", "Isen", "Tubingen")){
  
  study_area <- vect(list.files("data/created/vectors/",
                                pattern = paste0(i,".shp"),
                                full.names = T))
  
  list_vars <- list.files(paste0("data/created/rasters/",i),
                          full.names = T)
  
  rst <- rast(list_vars[c(4,9,6,1,10,7,2)])
  rst <- mask(rst, study_area)
  plot(rst)
  
  rst_topo <- terrain(rst$Elevation, v = c("slope",
                                            "TRI"))
  names(rst_topo) <- c("Slope", "TRI")
  rst <- c(rst, rst_topo)
  plot(rst)
  
  rst_landcover <- rast(list_vars[c(5)])
  
  for(j in unique(values(rst_landcover))){
    
  }
}

library(terra)     # or raster, depending on what "rst" is
library(mapview)

# Define the class codes, labels, and colors
class_df <- data.frame(
  value = 1:8,
  label = c("Artificial land", "Cropland", "Woodland", "Shrubland",
            "Grassland", "Bare land", "Water/permanent snow/ice", "Wetland"),
  color = c("#CC0303", "#CDB400", "#235123", "#B76124",
            "#92AF1F", "#F7E174", "#2019A4", "#AEC3D6")
)

# If rst is a SpatRaster (terra)
levels(rst) <- class_df[, c("value", "label")]  # set as categorical
rst <- as.factor(rst)

# Plot with mapview using the matching color palette
mapview(rst, col.regions = class_df$color, na.color = "transparent",
        maxpixels = 3794704)
