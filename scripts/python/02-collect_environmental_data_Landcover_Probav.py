# Collect Land-cover from Probav
# https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_Landcover_100m_Proba-V-C3_Global#bands

# # Run in R
# library(reticulate)
# use_condaenv("pythonenv", required = TRUE)
# py_config()

import geopandas as gpd
import geemap
import ee

ee.Authenticate(auth_mode="localhost")
ee.Initialize(project="gee-b-project")

list_study_areas = ["Veluwe", "Isen", "Tubingen"]

for study_area_id in list_study_areas:
  
  # Import study area
  study_area = gpd.read_file(
    f"data/created/vectors/{study_area_id}/study_area.shp"
  )
  
  study_area_ee = geemap.geopandas_to_ee(study_area).geometry()
  print(study_area.crs)
  
  crs_string = f"EPSG:{study_area.crs.to_epsg()}"
  
  # Land-cover
  bands = ["tree-coverfraction", "crops-coverfraction",
  "urban-coverfraction", "grass-coverfraction"]
  
  name_map = {
    "tree-coverfraction": "Tree",
    "crops-coverfraction": "Crops",
    "urban-coverfraction": "Urban",
    "grass-coverfraction": "Grass"
    }
    
  landcover = (
    ee.Image('COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019')
    .select(bands)
    .clip(study_area_ee)
    )
    
  for band in bands:
    out_name = name_map[band]
    geemap.ee_export_image(
      landcover.select(band),
      filename=f"data/created/rasters/{study_area_id}/{out_name}.tif",
      scale=100,
      region=study_area_ee,
      crs=crs_string,
      file_per_band=False
      )

