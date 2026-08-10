
library(terra)

df <- read.csv("data/raw/tables/Release_sites.csv")

for (i in seq_len(nrow(df))) {
  region_name <- df[i, "Region"]
  
  coords_tmp <- vect(
    df[i, ],
    geom = c("Longitude", "Latitude"),
    crs = "EPSG:4326"
  )
  
  study_area <- buffer(
    coords_tmp,
    width = 10000
  )
  
  plot(
    study_area,
    main = region_name
  )
  
  plot(
    coords_tmp,
    add = TRUE
  )
  
  writeVector(
    study_area,
    paste0(
      "data/created/vectors/study_area_",
      region_name,
      ".shp"
    )
  )
}

