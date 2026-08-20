library(terra)
library(tidyverse)

df_hives <- read.csv("data/created/tables/df_hives.csv")
df_release <- read.csv("data/raw/tables/Release_sites.csv")

df_release$crs <- c("EPSG:28992", "EPSG:25832", "EPSG:25832")

for(i in 1:nrow(df_release)){
  region_name <- df_release[i,"Region"]
  region_crs <- df_release[i,"crs"]
  
  df_release_tmp <- df_release[i,]
  df_hives_tmp <- df_hives[df_hives$Region == region_name,]
  
  coords_hives <- vect(df_hives_tmp,
                         geom = c("Longitude", "Latitude"),
                         crs = region_crs)  
  coords_release <- vect(df_release_tmp,
                         geom = c("Longitude", "Latitude"),
                         crs = "EPSG:4326")
  coords_release <- project(coords_release, region_crs)
  
  euclidean_distances <- distance(coords_hives, coords_release)
  df_hives[df_hives$Region == region_name,"Dist_release_km"] <- data.frame(euclidean_distances/1000)
  df_hives[df_hives$Region == region_name,"Longitude_release"] <- df_release[df_release$Region == region_name,"Longitude"]
  df_hives[df_hives$Region == region_name,"Latitude_release"] <- df_release[df_release$Region == region_name,"Latitude"]
  
}

write.table(df_hives,
            "data/created/tables/df_hives_with_distances.csv",
            sep = ",",
            row.names = T)
