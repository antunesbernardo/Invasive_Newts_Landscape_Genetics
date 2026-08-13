# Collect data from Landsat 05
# For the timewindow 1985-2011 from 01 March to 31 July
# List of variables:

# Median and trends for NDVI, MNDWI, BSI, and LST

# # Run in R
# library(reticulate)
# use_condaenv("pythonenv", required = TRUE)
# py_config()

import geopandas as gpd
import geemap
import ee

ee.Authenticate(auth_mode="localhost")
ee.Initialize(project="gee-b-project")

list_study_areas = ["Tubingen"]

for study_area_id in list_study_areas:

    # Import study area
    study_area = gpd.read_file(
        f"data/created/vectors/study_area_{study_area_id}.shp"
    )
    study_area_ee = geemap.geopandas_to_ee(study_area).geometry()
    print(study_area.crs)

    crs_string = f"EPSG:{study_area.crs.to_epsg()}"

    # Functions

    # Mask by quality
    def mask_landsat(image):
        qa = image.select('QA_PIXEL')
        cloud_conf = qa.rightShift(8).bitwiseAnd(3)
        shadow_conf = qa.rightShift(10).bitwiseAnd(3)
        cirrus_conf = qa.rightShift(14).bitwiseAnd(3)
        mask = (
            cloud_conf.lt(3)
            .And(shadow_conf.lt(3))
            .And(cirrus_conf.lt(3))
        )
        return image.updateMask(mask)

    # Apply scaling factors
    def apply_scale_factors(image):
        optical_bands = image.select('SR_B.').multiply(0.0000275).add(-0.2)
        thermal_band = image.select('ST_B6').multiply(0.00341802).add(149.0)

        return (
            image
            .addBands(optical_bands, None, True)
            .addBands(thermal_band, None, True)
        )

    # Rename
    def rename_landsat(image):
        return image.select(
            ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6', 'QA_PIXEL'],
            ['Blue',  'Green', 'Red',   'NIR',   'SWIR1', 'SWIR2', 'Thermal', 'QA_PIXEL']
        )

    # Compute NDVI
    def compute_ndvi(image):
        ndvi = image.normalizedDifference(['NIR', 'Red']).rename('NDVI')
        return image.addBands(ndvi)

    # MNDWI
    def compute_mndwi(image):
        mndwi = image.normalizedDifference(['Green', 'SWIR1']).rename('MNDWI')
        return image.addBands(mndwi)

    # Compute BSI
    def compute_bsi(image):
        bsi = image.expression(
            '((SWIR1 + Red) - (NIR + Blue)) / ((SWIR1 + Red) + (NIR + Blue))',
            {
                'SWIR1': image.select('SWIR1'),
                'Red': image.select('Red'),
                'NIR': image.select('NIR'),
                'Blue': image.select('Blue')
            }
        ).rename('BSI')
        return image.addBands(bsi)

    # Compute LST (Land Surface Temperature, in Celsius)
    def compute_lst(image):
        lst = image.select('Thermal').subtract(273.15).rename('LST')
        return image.addBands(lst)

    # Collect images from Landsat 05 (see https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2)
    landsat_collection = (
        ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
        .filterBounds(study_area_ee)
        .filterDate("1985-01-01", "2012-01-01")
        .map(mask_landsat)
        .map(apply_scale_factors)
        .map(rename_landsat)
        .map(compute_ndvi)
        .map(compute_mndwi)
        .map(compute_bsi)
        .map(compute_lst)
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
            .select(["NDVI", "MNDWI", "BSI", "LST"])
        )

        ndvi_median = (
            yearly_collection
            .select("NDVI")
            .median()
            .rename("NDVI_median")
        )

        mndwi_median = (
            yearly_collection
            .select("MNDWI")
            .median()
            .rename("MNDWI_median")
        )

        bsi_median = (
            yearly_collection
            .select("BSI")
            .median()
            .rename("BSI_median")
        )

        lst_median = (
            yearly_collection
            .select("LST")
            .median()
            .rename("LST_median")
        )

        yearly_image = (
            ndvi_median
            .addBands(mndwi_median)
            .addBands(bsi_median)
            .addBands(lst_median)
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
    MNDWI_median_image = landsat_per_year.select("MNDWI_median").median()
    BSI_median_image = landsat_per_year.select("BSI_median").median()
    LST_median_image = landsat_per_year.select("LST_median").median()

    # # Map results to explore
    # Map = geemap.Map()
    #
    # Map.addLayer(NDVI_median_image, {}, "NDVI")
    # Map.addLayer(MNDWI_median_image, {}, "MNDWI")
    # Map.addLayer(BSI_median_image, {}, "BSI")
    # Map.addLayer(LST_median_image, {}, "LST")
    #
    # Map.centerObject(study_area_ee, 10)
    #
    # Map.to_html(
    #     "data/created/interactive_maps/temporary_map_to_explore.html"
    # )

    # Export rasters
    geemap.ee_export_image(
        NDVI_median_image,
        filename=f"data/created/rasters/ndvi_median_{study_area_id}.tif",
        region=study_area_ee,
        scale=30,
        crs=crs_string,
        file_per_band=False
    )
    geemap.ee_export_image(
        MNDWI_median_image,
        filename=f"data/created/rasters/mndwi_median_{study_area_id}.tif",
        region=study_area_ee,
        scale=30,
        crs=crs_string,
        file_per_band=False
    )
    geemap.ee_export_image(
        BSI_median_image,
        filename=f"data/created/rasters/bsi_median_{study_area_id}.tif",
        region=study_area_ee,
        scale=30,
        crs=crs_string,
        file_per_band=False
    )
    geemap.ee_export_image(
        LST_median_image,
        filename=f"data/created/rasters/lst_median_{study_area_id}.tif",
        region=study_area_ee,
        scale=30,
        crs=crs_string,
        file_per_band=False
    )

    # Characterize changes in NDVI, MNDWI, BSI, and LST
    def add_year_band(image):
        year = ee.Image.constant(image.getNumber("year")).rename("year").float()
        return image.addBands(year)

    NODATA_VALUE = -9999

    indices = ["NDVI", "MNDWI", "BSI", "LST"]

    trend_results = {}  # keep references around in case you need them later

    for index in indices:
        band_name = f"{index}_median"

        yearly = landsat_per_year.select(band_name)
        sorted_yearly = yearly.sort("year")
        with_year = sorted_yearly.map(add_year_band)

        # Linear fit trend
        trend = with_year.select(["year", band_name]).reduce(ee.Reducer.linearFit())
        trend_slope = trend.select("scale").clip(study_area_ee)

        direction = trend_slope.gt(0).rename(f"{index}_direction")
        intensity = trend_slope.abs().rename(f"{index}_intensity")
        trend_components = ee.Image.cat([direction, intensity]).clip(study_area_ee)

        # Pearson correlation / R²
        pearson = with_year.select(["year", band_name]).reduce(ee.Reducer.pearsonsCorrelation())
        r_squared = pearson.select("correlation").pow(2).rename(f"{index}_trend_r2")

        reliable_trend = trend_slope.updateMask(r_squared.gt(0.3))
        reliable_trend_export = reliable_trend.unmask(NODATA_VALUE)

        trend_results[index] = {
            "trend_slope": trend_slope,
            "trend_components": trend_components,
            "r_squared": r_squared,
            "reliable_trend": reliable_trend,
        }

        # Exports
        geemap.ee_export_image(
            reliable_trend_export,
            filename=f"data/created/rasters/{index.lower()}_trend_reliable_{study_area_id}.tif",
            region=study_area_ee,
            scale=30,
            crs=crs_string,
            file_per_band=False
        )

        geemap.ee_export_image(
            trend_slope,
            filename=f"data/created/rasters/{index.lower()}_trend_{study_area_id}.tif",
            region=study_area_ee,
            scale=30,
            crs=crs_string,
            file_per_band=False
        )
        
        
        
        
