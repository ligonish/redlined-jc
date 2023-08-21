# Checking on a merge of HOLC 1939 & tidycensus API sf dataframes
# Tuesday night, March 21

# [Run scripts 00_ & 01_]


ggplot()+
  geom_sf(data = rent_burden_20)+
  geom_sf(data = holc, aes(fill = first_holc))+
  scale_fill_viridis_d()+
  theme_void()

# Caaaan ... we do both?

rent_burden_20 %>% 
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "median hh rent-burdened (2020 ACS 5-yr)")

ggplot()+
  geom_sf(data = rent_burden_20)+
  geom_sf(data = holc, aes(fill = first_holc))+
  scale_fill_viridis_d()+
  theme_void()


# Literally not quite sure how to do a filtering join on two different sf objs,
# ... but let's test & see whether we can get the HC-wide rent control sf object
# to cut down to just the redlined tracts?

test_cut <- st_join(rent_burden_20, test_merge, join = geometry) %>% 
  filter(!is.na(gisjoin))

# joined <- st_join(rent_burden_20, test_merge, join = st_equals) # this leaves me w/ all NAs in the HOLC data fields

joined <- st_join(rent_burden_20, test_merge, join = st_covers) # 


# works after transforming both sets to use CRS 4269

joined %>% 
  ggplot(aes(fill = first_holc)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_d() + 
  labs(fill = "Shocked if This Works")


# OK, going to bed. Where we left off:
# This merge isn't quite working, but what I have 
# is that the last 5 digits of both -gisjoin- and -GEOID- are the same

# alternatively: via https://walker-data.com/census-r/other-census-and-government-data-resources.html
# could try using ipumsr instead with their read_nhgis function

# OR fuck it just convert both the holc & acs data to df objects & merge using dplyr for the data

# lastly: try the crsuggest() package, ref'd in above link

# Trying again, Sunday March 26

test_merge <- st_transform(holc, crs = st_crs(rent_burden_20))

st_crs(test_merge)

# Ah. OK so part of what's happening is that the original file from GitHub lists up to 5 duplicated gisjoins/geom rows for same census tract, w/ each separated by % of 2020 tract that got a certain HOLC grade. See for instance G3400170000902.

holc_dupes <- holc %>% get_dupes(gisjoin)

# Actually nm, trying this https://rpubs.com/ezazycki/777608

test_merge <- st_transform(holc, crs = st_crs(rent_burden_20))

overlap <- st_intersection(
  test_merge, 
  rent_burden_20 ) %>% # spatial joining
  mutate(area = st_area(geometry)) %>% # calculate block area and add new column
  as.data.frame() %>%
  select(NAME, area, first_holc, geometry) %>% 
  group_by(NAME) %>%
  mutate(coverage = as.numeric(area / sum(area)) # Calculate coverage
  )
