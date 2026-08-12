# Collect remote sensing data from Landsat 05
# For the timewindow 1985-2011 from March to August
# List of variables:

# Landsat
# NDVI
# NDMI
# NDWI
# EVI
# LST
# MNDWI
# BSI

# Topography
# Elevation
# Slope
# TPI
# CTI

# # Run in R
# library(reticulate)
# use_condaenv("pythonenv", required = TRUE)
# py_config()

import geopandas as gpd
import geemap
import ee

ee.Authenticate(auth_mode="localhost")
ee.Initialize(project="gee-b-project")

# Import study area
study_area = gpd.read_file(
  "data/created/vectors/study_area_Tubingen.shp"
)

study_area_ee = geemap.geopandas_to_ee(study_area).geometry()
print(study_area.crs)

# Functions
# Mask by quality
def mask_landsat(image):
  qa = image.select('QA_PIXEL')
  cloud_conf = qa.rightShift(8).bitwiseAnd(3)
  shadow_conf = qa.rightShift(10).bitwiseAnd(3)
  cirrus_conf = qa.rightShift(14).bitwiseAnd(3)
  mask = (
    cloud_conf.lt(2)
    .And(shadow_conf.lt(2))
    .And(cirrus_conf.lt(2))
  )
  return image.updateMask(mask)

# Apply scaling factors
def apply_scale_factors(image):
  optical_bands = image.select('SR_B.').multiply(0.0000275).add(-0.2)
  return image.addBands(optical_bands, None, True)

# Rename
def rename_landsat(image):
  return image.select(
    ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'QA_PIXEL'],
    ['Blue',  'Green', 'Red',   'NIR',   'SWIR1', 'SWIR2', 'QA_PIXEL']
  )

# Compute NDVI
def compute_ndvi(image):
  ndvi = image.normalizedDifference(['NIR', 'Red']).rename('NDVI')
  return image.addBands(ndvi)

# Compute NDWI
def compute_ndwi(image):
  ndwi = image.normalizedDifference(['Green', 'NIR']).rename('NDWI')
  return image.addBands(ndwi)

# MNDWI
def compute_mndwi(image):
    mndwi = image.normalizedDifference(['Green', 'SWIR1']).rename('MNDWI')
    return image.addBands(mndwi)

# Collect images from Landsat 05 (see https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2)
landsat_collection = (
    ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
    .filterBounds(study_area_ee)
    .filterDate("1985-01-01", "2012-01-01")
    .map(mask_landsat)
    .map(apply_scale_factors)
    .map(rename_landsat)
    .map(compute_ndvi)
    .map(compute_ndwi)
    .map(compute_mndwi)
)

# Compute median per year
start_year = 1985
end_year = 2011

years = ee.List.sequence(start_year, end_year)


def compute_median_per_year(year):
    year = ee.Number(year)

    start = ee.Date.fromYMD(year, 3, 1)
    end = ee.Date.fromYMD(year, 8, 1)

    yearly_collection = (
        landsat_collection
        .filterDate(start, end)
        .select(["NDVI", "NDWI", "MNDWI"])
    )

    ndvi_median = (
        yearly_collection
        .select("NDVI")
        .median()
        .rename("NDVI_median")
    )

    ndwi_median = (
        yearly_collection
        .select("NDWI")
        .median()
        .rename("NDWI_median")
    )
    
    mndwi_median = (
        yearly_collection
        .select("MNDWI")
        .median()
        .rename("MNDWI_median")
    )

    yearly_image = (
        ndvi_median
        .addBands(ndwi_median)
        .addBands(mndwi_median)
        .clip(study_area_ee)
        .set("year", year)
    )

    return yearly_image


landsat_per_year = ee.ImageCollection(
    years.map(compute_median_per_year)
)

print("Years processed:", landsat_per_year.size().getInfo())

# Get medians
NDVI_median_image = landsat_per_year.select("NDVI_median").median()
NDWI_median_image = landsat_per_year.select("NDWI_median").median()
MNDWI_median_image = landsat_per_year.select("MNDWI_median").median()

# # Map results
# Map = geemap.Map()
# 
# Map.addLayer(NDVI_median_image)
# 
# Map.centerObject(study_area_ee, 10)
# 
# Map.to_html(
#     "data/created/interactive_maps/study_area_map.html"
# )

# Export rasters
geemap.ee_export_image(
    NDVI_median_image,
    filename="data/created/rasters/ndvi_median_Isen.tif",
    region=study_area_ee,
    scale=30,
    crs="EPSG:25832",
    file_per_band=False
)

geemap.ee_export_image(
    NDWI_median_image,
    filename="data/created/rasters/ndwi_median_Isen.tif",
    region=study_area_ee,
    scale=30,
    crs="EPSG:25832",
    file_per_band=False
)

geemap.ee_export_image(
    MNDWI_median_image,
    filename="data/created/rasters/mndwi_median_Isen.tif",
    region=study_area_ee,
    scale=30,
    crs="EPSG:25832",
    file_per_band=False
)
