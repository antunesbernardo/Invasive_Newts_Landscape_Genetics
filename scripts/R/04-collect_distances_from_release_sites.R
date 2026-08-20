library(terra)
library(tidyverse)

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
  
  euclidean_distances <- distance(coords_genetic, coords_release)
  df_genetic[df_genetic$Region == region_name,"Dist_release_km"] <- data.frame(euclidean_distances/1000)
  df_genetic[df_genetic$Region == region_name,"Longitude_release"] <- df_release[df_release$Region == region_name,"Longitude"]
  df_genetic[df_genetic$Region == region_name,"Latitude_release"] <- df_release[df_release$Region == region_name,"Latitude"]
  
}

write.table(df_genetic,
            "data/created/tables/landscape_genetics_data.csv",
            sep = ",",
            row.names = T)
