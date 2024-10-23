# to-do: 
# 1. list all years where county-level gis is available
library(tidyverse)
library(ipumsr)
library(sf)
ipums_data_collections()
ds <- get_metadata_nhgis('datasets')
tst <- get_metadata_nhgis("time_series_tables")
sf <- get_metadata_nhgis("shapefiles")

a <- sf %>% 
  filter(geographic_level == "County")

# 2. extract and download 1870 shapefile
sf1870 <- define_extract_nhgis(
  shapefiles = 'us_county_1870_tl2008'
) %>% 
  submit_extract() %>% 
  wait_for_extract() %>% 
  download_extract() %>% 
  read_ipums_sf()

# if have submitted the extract before, can download directly
# get_extract_history('nhgis')
# sf1870 <- get_extract_info('nhgis:1') %>% 
#   download_extract() %>% 
#   read_ipums_sf()

# if have downloaded the file, can read directly
# sf1870 <- read_ipums_sf('nhgis0001_shape.zip')

plot(sf1870$geometry)

# 3. calculate the Haversine distance between county centroids
sf1870 <- sf1870 %>% 
  st_transform(crs = 'EPSG:4326') %>% 
  mutate(
    geometry = if_else(st_is_valid(geometry), geometry, st_make_valid(geometry)),
    centroid = st_centroid(geometry) %>% st_coordinates()
  ) %>% 
  st_drop_geometry()

county_dist_1870 <- sf1870 %>% 
  select(DECADE, STATE, COUNTY, ICPSRFIP, STATENAM, COUNTYNAM = NHGISNAM, centroid) %>% 
  left_join(
    sf1870 %>% 
      select(DECADE, STATE, COUNTY, ICPSRFIP, STATENAM, COUNTYNAM = NHGISNAM, centroid),
    join_by(DECADE), relationship = 'many-to-many'
  ) %>% 
  mutate(dist_km = geosphere::distHaversine(centroid.x, centroid.y)/1000, 
         dist_mi = dist_km * 0.62137119223733) %>% 
  select(-centroid.x, -centroid.y) %>% 
  janitor::clean_names() %>% 
  arrange(state_x, county_x, dist_km)

county_dist_1870_50mi <- county_dist_1870 %>% 
  filter(dist_mi <= 50)
county_dist_1870_50mi %>% write_csv('data/county_dist_1870_50mi.csv')
