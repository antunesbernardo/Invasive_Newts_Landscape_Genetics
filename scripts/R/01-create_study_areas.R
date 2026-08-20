library(terra)

df_genetic <- read.csv("data/raw/tables/Genetic_data.csv")
df_release <- read.csv("data/raw/tables/Release_sites.csv")

df_release$crs <- c("EPSG:28992", "EPSG:25832", "EPSG:25832")

for(i in 1:nrow(df_release)){
  region_name <- df_release[i,"Region"]
  region_crs <- df_release[i,"crs"]
  
  df_release_tmp <- df_release[i,]
  df_genetic_tmp <- df_genetic[df_genetic$Region == region_name,]
  
  coords_genetic <- vect(df_genetic_tmp,
                         geom = c("Longitude", "Latitude"),
                         crs = "EPSG:4326")  
  coords_release <- vect(df_release_tmp,
                         geom = c("Longitude", "Latitude"),
                         crs = "EPSG:4326")
  
  max_distance_release <- max(distance(coords_release, coords_genetic))
  
  coords_release_proj <- project(coords_release, region_crs)
  study_area <- buffer(coords_release_proj, width = max_distance_release + 2000)
  
  writeVector(
    study_area,
    paste0(
      "data/created/vectors/",region_name,"/study_area.shp"),
    overwrite = TRUE
  )
  
}
