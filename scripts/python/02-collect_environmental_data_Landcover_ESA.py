# Collect Land-cover

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
    landcover = (
        ee.ImageCollection("ESA/WorldCover/v200")
        .first()
        .select("Map")
        .clip(study_area_ee)
    )

    geemap.ee_export_image(
        landcover,
        filename=f"data/created/rasters/{study_area_id}/Landcover_esa.tif",
        scale=10,
        region=study_area_ee,
        crs=crs_string,
        file_per_band=False
    )
    
    
