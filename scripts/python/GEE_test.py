# library(reticulate)
# 
# use_condaenv("pythonenv", required = TRUE)
# 
# py_config()

import geopandas as gpd
import geemap
import ee
import datetime

ee.Authenticate()
ee.Initialize(project="gee-b-project")

study_area = gpd.read_file(
    "data/created/vectors/study_area_Veluwe.shp"
)

study_area = study_area.to_crs("EPSG:4326")

study_area_ee = geemap.geopandas_to_ee(study_area).geometry()

# ---------------------------------------------------------
# NDVI
# ---------------------------------------------------------

def add_ndvi(image):
    return image.normalizedDifference(
        ["B8", "B4"]
    ).rename("NDVI")

# ---------------------------------------------------------
# NDWI
# ---------------------------------------------------------

def add_ndwi(image):
    return image.normalizedDifference(
        ["B3", "B8"]
    ).rename("NDWI")

# ---------------------------------------------------------
# Sentinel-2
# ---------------------------------------------------------

s2 = (
    ee.ImageCollection(
        "COPERNICUS/S2_SR_HARMONIZED"
    )
    .filterBounds(study_area_ee)
    .filterDate(
        "2025-04-01",
        "2026-08-01"
    )
    .filterMetadata(
        "CLOUDY_PIXEL_PERCENTAGE",
        "less_than",
        10
    )
)

# ---------------------------------------------------------
# Calculate median NDVI
# ---------------------------------------------------------

ndvi = (
    s2
    .map(add_ndvi)
    .median()
    .clip(study_area_ee)
)

# ---------------------------------------------------------
# Calculate median NDWI
# ---------------------------------------------------------

ndwi = (
    s2
    .map(add_ndwi)
    .median()
    .clip(study_area_ee)
)

# ---------------------------------------------------------
# Map
# ---------------------------------------------------------

Map = geemap.Map()

Map.centerObject(
    study_area_ee,
    10
)

# NDVI
Map.addLayer(
    ndvi,
    {
        "palette": [
            "red",
            "yellow",
            "green"
        ]
    },
    "NDVI"
)

# NDWI
Map.addLayer(
    ndwi,
    {
        "min": -1,
        "max": 1,
        "palette": [
            "brown",
            "white",
            "blue"
        ]
    },
    "NDWI"
)

# ---------------------------------------------------------
# Save map
# ---------------------------------------------------------

Map.to_html(
    "data/created/interactive_maps/study_area_map.html"
)

# Save rasters for analyses in R

# NDVI
geemap.ee_export_image(
    ndvi,
    filename="data/created/rasters/ndvi.tif",
    scale=10,
    region=study_area_ee,
    file_per_band=False
)

# Export NDVI
geemap.ee_export_image(
    ndvi,
    filename="data/created/rasters/ndvi.tif",
    region=study_area_ee,
    scale=10,
    crs="EPSG:4326",
    file_per_band=False
)

# To Google drive and download manually for large rasters
task = ee.batch.Export.image.toDrive(
    image=ndvi,
    description="ndvi",
    folder="GEE_exports",
    fileNamePrefix="ndvi",
    region=study_area_ee,
    scale=100,
    crs="EPSG:4326",
    fileFormat="GeoTIFF",
    maxPixels=1e13
)

task.start()

print("NDVI export started.")
print("Task ID:", task.id)

# NDWI
geemap.ee_export_image(
    ndvi,
    filename="data/created/rasters/ndvi.tif",
    region=study_area_ee,
    scale=10,
    crs="EPSG:4326",
    file_per_band=False
)
