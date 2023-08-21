# Scratchpad: Tidycensus 2020 ACS import
# Tuesday, March 21, 2023

library(tidyverse)
library(tidycensus)
library(janitor)
library(sf)

library(viridis)
library(hrbrthemes)

options(tigris_use_cache = T)

# ACS 2020 5-yr table key

v20 <- load_variables(2020, "acs5")

# NOTE TO SELF: SELECT VARIABLES OF INTEREST FROM ABOVE
# For now, quick & dirty example with rent burden added

rent_burden_20 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  variable = "B25071_001",
  geometry = TRUE,
  keep_geo_vars = TRUE, # retains detailed TIGER/Line geo variables (useful for joining)
  year = 2020,
  cb = F,     # setting to FALSE gets us the TIGER/Line shapefiles, which I *think* match those of the M.E. project(?)
  cache_table = T) %>% 
  mutate(over30 = (estimate > 30),
         over50 = (estimate > 50)) %>% 
  clean_names() %>% 
  remove_constant()
      # 183 obs of 7 variables

st_crs(rent_burden_20)  # NAD83 / EPSG 4269

# Create JC-only filter (made externally in QGIS as a JC 2020 tract layer)

  # See "SCRATCH.R" script, where I imported this as an sf object:
  # tracts_20 <- st_read("/Users/Sarah/Dropbox/Mac/Desktop/SP23 JC Redline Project/Spatial Data/ESRI_JC_Tract_Layer/jc_2020_tracts.shp") %>% 
  # clean_names() %>% 
  #  remove_constant()

jc_test <- tracts_20 %>% 
  #as.data.frame() %>% 
  select(geoid, namelsad)

census_test <- rent_burden_20 %>% 
  st_transform(crs = st_crs(tracts_20)) %>% 
  st_intersection(tracts_20)

str(rent_burden_20)
str(tracts_20)
str(maj_grades)

# Check CRS
st_crs(rent_burden_20) # 4269

# Quick Map
census_test %>% 
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "median hh rent-burdened (2020 ACS 5-yr)")


