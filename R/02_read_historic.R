library(tidyverse)
library(sf)
library(janitor)

library(viridis)
library(hrbrthemes)

# Load
historic <- st_read("data/Historic_Districts/Districts/Historic_Districts_of_New_Jersey.shp") %>% 
  clean_names() %>% 
  filter(status == "LISTED", demolished == "NO") %>%
  select(name, status, nrdate, srdate, localdate, geometry) %>% 
  st_transform(crs = st_crs(tracts_20))

st_crs(historic)  # original == NAD83 New Jersey / EPSG 3424

hist_overlaps <- st_intersection(
  historic, 
  maj_grades) %>% 
  mutate(area = st_area(geometry)) %>% 
  #as.data.frame() %>%      ### KEEP FOR DATA; DROP FOR MAPPING
  #select(geoid, name, area, holc_grade, holc_id, geometry) %>% 
  group_by(name) %>%
  mutate(prop_in_historic = as.numeric(area / sum(area))) %>% 
  ungroup()

maj_historic <- hist_overlaps %>%
  slice(-31) %>%   # Tract 75 falls within both VVP and Paulus Hook, but is still 100% historic
  #filter(!is.na(localdate)) %>%
  filter(str_detect(name, "Historic District")) %>% 
  group_by(geoid) %>% 
  mutate(prop_hist = case_when(
    prop_in_historic < 0.25 ~ "Some portion < 25%",
    prop_in_historic < 0.51 ~ "25%-50%",
    TRUE ~ "Over 50%"
  )) %>% 
  ungroup() %>%   
# select(name, grade, geometry)  ## IF MAPPING
  as.data.frame() 
  #select(geoid, prop_hist)  ## for adding to Tidycensus tables

any_historic <- hist_overlaps %>% 
  filter(str_detect(name, "Historic District")) %>% 
  mutate(any_hist = "Yes") %>%
  distinct(geoid, .keep_all = T) %>% 
  as.data.frame() %>% 
  select(geoid, any_hist)
  
    # 15 tracts are at least partially in a designated "historic district"
    # 11 "D" tracts; 2 "C" tracts; 2 "B" tracts (both "B"s & both "C"s are in newly-designated West Bergen HD)
    # remaining 11 "D" all in the Downtown Four 
  

################ FOR RUNNING THE CENSUS ANALYSIS TOMORROW:

maj_grades <- maj_grades %>% 
  as.data.frame() %>% 
  #left_join(maj_historic) %>% 
  left_join(any_historic) %>% 
  # select(geoid, grade, prop_hist, any_hist) %>% 
  select(geoid, grade, any_hist)

maj_grades %>% 
  group_by(grade, any_hist) %>% 
  summarize(tracts = n_distinct(geoid, na.rm = T))

maj_d <- maj_grades %>% 
  filter(grade == "D")

## IN THE ER: MAKING DO W/ EXISTING TIDYCENSUS INFO (bad wifi)

h_rent_burden_20 <- rent_burden_20 %>% 
  select(-prop_hist) %>% 
  left_join(maj_grades)

# TENURE: hist/not hist -------------------------------

h_rent_burden_20 %>% 
  group_by(grade, any_hist) %>% 
  summarise(rent_burden = median(estimate, na.rm = T))
# Citywide = 27.4%; B = 24%; C = 31.6%; D = 26.5%; Ungraded = 23.8%

rent_burden_20 %>% 
  mutate(majority_redlined = case_when(
    grade == "C" | grade == "D" ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  group_by(majority_redlined) %>% 
  summarise(rent_burden = median(estimate, na.rm = T))
# No = 23.8%
# Yes = 28.4%

# TENURE BY RACE: hist/not hist -------------------------------

h_race_tenure_20 <- race_tenure_20 %>% 
  left_join(maj_grades)

h_r_summary <- h_race_tenure_20 %>% 
  group_by(grade, any_hist) %>% 
  summarise(white_households = median(pct_wh, na.rm = T),
            black_households = median(pct_bl, na.rm = T),
            hplx_households = median(pct_hl, na.rm = T),
            white_own = median(pct_wh_oo, na.rm = T),
            black_own = median(pct_bl_oo, na.rm = T),
            hplx_own = median(pct_hl_oo, na.rm = T),
            white_rent = median(pct_wh_rent, na.rm = T),
            black_rent = median(pct_bl_rent, na.rm = T),
            hplx_rent = median(pct_hl_rent, na.rm = T),
  )

#TENURE, GENERALLY: hist/not hist --------------------------------
  
h_tenure_20 <- tenure_20 %>% 
  left_join(maj_grades)

h_tenure_20 %>% 
  mutate(variable = case_when(
    variable == "B25003_001" ~ "All",
    variable == "B25003_002" ~ "Owner-Occupied",
    variable == "B25003_003" ~ "Renter-Occupied")) %>% 
  filter(variable != "All") %>% 
  group_by(grade, variable, any_hist) %>% 
  summarise(tenure = median(percent, na.rm = T))
