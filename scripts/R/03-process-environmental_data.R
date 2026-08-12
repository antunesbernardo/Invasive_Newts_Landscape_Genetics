library(terra)
ndvi <- rast("data/created/rasters/ndvi_median_Isen.tif")
ndwi <- rast("data/created/rasters/ndwi_median_Isen.tif")
mndwi <- rast("data/created/rasters/mndwi_median_Isen.tif")

plot(c(ndvi, ndwi, mndwi))

# Run in R
library(reticulate)
use_condaenv("pythonenv", required = TRUE)
py_config()
