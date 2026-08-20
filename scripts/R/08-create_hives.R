
library(terra)
library(sf)

df_hives <- data.frame()

for(i in c("Veluwe", "Isen", "Tubingen")){
  
  study_area <- vect(paste0("data/created/vectors/",i,"/study_area.shp"))
  study_area_sf <- st_as_sf(study_area)
  
  hive_sf <- st_make_grid(
    study_area_sf,
    cellsize = 1000,
    square = FALSE
  )
  
  hive <- vect(hive_sf)
  hive <- mask(hive, study_area)
  plot(hive)
  
  cent <- centroids(hive)
  coords <- crds(cent)
  
  hive_df <- data.frame(
    Hive_ID = seq_len(nrow(hive)),
    Longitude = coords[, 1],
    Latitude = coords[, 2],
    Region = i
  )
    
    writeVector(hive, paste0("data/created/vectors/",i,"/study_area_hive.shp"),
                overwrite = T)
  
  df_hives <- rbind(df_hives, hive_df)
    
}

write.table(df_hives, paste0("data/created/tables/df_hives.csv"),
            sep = ",",
            row.names = F)
