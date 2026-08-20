
library(terra)
library(tidyverse)

df <- read.csv("data/created/tables/df_hives_with_distances.csv",
               sep = ",", header = T)

for(i in 1:nrow(df)){
  
  print(paste0("Row ", i))
  
  region <- df[i,"Region"]
  
  rst <- rast(paste0("data/created/rasters/",region,"/All_environmental_variables.tif"))
  
  pop_1 <- vect(df[i,], c("Longitude", "Latitude"),
                crs = crs(rst))
  pop_2 <- vect(df[i,], c("Longitude_release", "Latitude_release"),
                crs = "EPSG:4326")
  pop_2 <- project(pop_2, crs(rst))
  pop_pair <- vect(c(pop_1, pop_2))
  line_pair <- convHull(pop_pair)
  
  for(d in c(1000)){
    
    
    study_area <- buffer(line_pair, width = d) 
    study_area_pop <- buffer(pop_1, width = d) 
    
    # plot(study_area)
    # plot(pop_pair, add = T)
    # plot(line_pair, add = T)
    
    for(j in 1:length(names(rst))){
      
      rst_temp <- rst[[j]]
      
      rst_temp <- mask(crop(rst_temp, study_area), study_area)
      rst_temp_pop <- mask(crop(rst_temp, study_area_pop), study_area_pop)
      # plot(rst_temp, add = T) 
      
      study_area_raster <- terra::rasterize(study_area,
                                            rst_temp, touches = T)
      
      if(length(na.omit(values(rst_temp))) <
         length(na.omit(values(study_area_raster)))){
        
        rst_mean <- NA 
        
        rst_mean_pop <- NA
        
        
      }else{
        
        rst_mean <- mean(na.omit(values(rst_temp)))
        
        rst_mean_pop <- mean(na.omit(values(rst_temp_pop)))
        
        
      }
      
      df[i,names(rst_temp)] <- rst_mean
      df[i,paste0(names(rst_temp),"_pop")] <- rst_mean_pop
      
      
    }
    
  }
}

df[df$Region == "Veluwe", "Time_release"] <- 2025 - 1973
df[df$Region == "Isen", "Time_release"] <- 2025 - 1991
df[df$Region == "Tubingen", "Time_release"] <- 2025 - 1986

write.table(df, "data/created/tables/df_hives_with_distances_and_env.csv",
            sep = ",", row.names = F)
