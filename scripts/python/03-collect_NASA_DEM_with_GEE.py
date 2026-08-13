# Collect Digital elevation model from NASA/NASADEM_HGT/001

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
        f"data/created/vectors/study_area_{study_area_id}.shp"
    )
    study_area_ee = geemap.geopandas_to_ee(study_area).geometry()
    print(study_area.crs)

    crs_string = f"EPSG:{study_area.crs.to_epsg()}"
    
    # NASA DEM
    elevation = (
    ee.Image("NASA/NASADEM_HGT/001")
    .select("elevation")
    .clip(study_area_ee)
    .rename("Elevation")
    )
    
    geemap.ee_export_image(
    elevation,
    filename=f"data/created/rasters/elevation_{study_area_id}.tif",
    region=study_area_ee,
    scale=30,
    crs=crs_string,
    file_per_band=False
    )

