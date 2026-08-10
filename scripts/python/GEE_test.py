import geopandas as gpd
import geemap
import ee

ee.Authenticate()
ee.Initialize(project="gee-b-project")

study_area = gpd.read_file(
    "data/created/vectors/study_area_Veluwe.shp"
)

study_area = study_area.to_crs("EPSG:4326")

study_area_ee = geemap.geopandas_to_ee(study_area)

Map = geemap.Map()

Map.addLayer(
    study_area_ee,
    {"color": "red"},
    "Study area"
)

Map.centerObject(
    study_area_ee,
    10
)

Map.to_html("outputs/study_area_map.html")
