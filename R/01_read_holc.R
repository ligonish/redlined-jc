# Scratchpad: Mapping Inequality's 1939/2020 Tract-Level Data Link
# Source: https://github.com/americanpanorama/Census_HOLC_Research/tree/main/2020_Census_Tracts
# Tuesday, March 21, 2023

library(tidyverse)
library(sf)
library(janitor)

library(viridis)
library(hrbrthemes)

# Load
github_holc <- st_read("data/Mergeable_SHP_Nationwide_Tracts_2020_HOLC/Tracts_2020_HOLC.shp") %>% 
  subset(MAX_city == "Hudson Co.") %>% 
  clean_names()

holc <- st_read("data/HudCo_1939_shapefiles/cartodb-query.shp") %>% 
  clean_names() %>% 
  subset(name == "Jersey City") %>% 
  select(holc_grade, geometry)

# Visualize:
plot(holc)

# Check CRS
st_crs(holc)

# Adjust CRS to match tidycensus import
crs_adj_holc <- st_transform(holc, crs = st_crs(rent_burden_20))

# Calculate overlap between 2020 census & 1939 HOLC maps

overlap <- st_intersection(
  crs_adj_holc, 
  rent_burden_20) %>% # spatial joining
  mutate(area = st_area(geometry)) %>% # calculate block area and add new column
  #as.data.frame() %>%
  select(NAMELSAD, area, holc_grade, geometry) %>% 
  group_by(NAMELSAD) %>%
  mutate(coverage = as.numeric(area / sum(area)) # Calculate coverage
  )

# Assign each 2020 Census tract a majority HOLC grade, if applicable
single_grade <- overlap %>% 
  group_by(NAMELSAD) %>% 
  mutate(max_coverage = max(coverage)) %>% 
  mutate(grade = case_when(
    coverage == max_coverage ~ holc_grade)
  )%>% 
  filter(!is.na(grade))

plot(single_grade)

plot(overlap)

# Basically I just need a list of all JC 2020 Census tracts with their 1939 HOLC grades attached.

jc_2020 <- st_read("data/JC_Census_Tracts_2020/census-tracts-2020.shp")
