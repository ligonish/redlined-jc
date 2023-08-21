library(ggmap)
library(wesanderson)

# Zissou1 codes
# "#3B9AB2" "#78B7C5" "#EBCC2A" "#E1AF00" "#F21A00"


# Messing with quick 

master_sf %>% 
  subset(designated == "Historic") %>% 
  plot()

master_sf %>% 
  subset(maj_grade == "D") %>% 
  plot()


# thnking about the geoid's that don't have decimal pts bc they come from 2010 instead of 2020 census tracts.

missing_from_dec <- master_df %>% 
  anti_join(decennial_h_d) %>% 
  filter(!is.na(maj_grade))







# Basic sf plot check
plot(master_sf)
plot(historic)
plot(holc["holc_grade"])

# Tracts without a majority HOLC grade
master_sf %>% 
  subset(is.na(maj_grade)) %>% 
  plot()

# Historic + Redlined: Quick Peek
master_sf %>%
  filter(maj_grade == "D" & designated == "Historic") %>% 
  ggplot(aes(fill = maj_grade)) + 
  geom_sf(color = "white") + 
  scale_fill_viridis_d(alpha = .8, option = "E", na.value = "gray90")+
  theme_void()


# Load JC municipal boundaries shapefile
jc_boundaries <- st_read("Spatial Data/jc_boundaries_njgin/Municipal_Boundaries_of_NJ.shp") %>% 
  select(geometry)

maj_d %>% 
  st_intersection(jc_boundaries) %>% 
  plot()

# 1: Sample JC basemap
# Bounding box eyeballed via https://boundingbox.klokantech.com/ > click "TSV" when done

jc_bbox <- c(left = -74.116687, bottom = 40.665, right = -74.025,	top = 40.7725)

# Greyed-out Stamen Watercolor: gorgeous but still too busy
#jc_map <- get_stamenmap(jc_bbox,
#                        maptype = "watercolor",
#                        zoom = 13,
#                        color = "bw",
#                        force = TRUE)

# Terrain Lines: prob best for this application
jc_map <- get_stamenmap(jc_bbox,
                        maptype = "terrain-lines",
                        zoom = 14)
# Tracts + HOLC over basemap
#ggmap(jc_map)+ 
ggmap(jc_map, darken = c(.6, "white"))+
  geom_sf(data = jc_boundaries, 
          fill = NA,
          lwd = 3,
          color = "white",
          inherit.aes = FALSE)+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          inherit.aes = FALSE,
          lwd = 0,
          alpha = .6)+
  geom_sf(data = st_intersection(jc_tracts_20, jc_boundaries), 
          fill = NA,
          lwd = .5,
          color = "grey30",
          inherit.aes = FALSE)+
  coord_sf(crs = st_crs(4326))+ 
  scale_fill_manual(
    values = c("#3B9AB2", "#EBCC2A", "#F21A00"),
    name = "1939 HOLC Grades")+
  labs (title = "1939 HOLC redlined areas of 2020 Jersey City, NJ",
        subtitle = "",
        caption = "Source: U.S. Census TIGER/Line tract shapefiles (2020), NJGIN municipal boundary shapefiles, and Mapping Inequality shapefiles of 1939-era Hudson County HOLC grade polygon coordinates.",
        x = "",
        y = "")+
  theme_minimal(base_family = "Roboto Condensed")+
  theme(plot.title = element_text(family = "Roboto Condensed", 
                                  face = "bold",
                                  size = 16),
        axis.text = element_blank(),
        legend.title = element_text(family = "Roboto Condensed"))
  

# Municipal Boundary + HOLC over basemap
#ggmap(jc_map)+ 
ggmap(jc_map, darken = c(.6, "white"))+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          inherit.aes = FALSE,
          lwd = 0,
          alpha = .7)+
  geom_sf(data = jc_boundaries, 
          fill = NA,
          lwd = 3,
          color = "white",
          inherit.aes = FALSE)+
  geom_sf(data = jc_boundaries, 
          fill = NA,
          lwd = .7,
          color = "grey40",
          inherit.aes = FALSE)+
  coord_sf(crs = st_crs(4326))+ 
  scale_fill_manual(
    values = c("#3B9AB2", "#EBCC2A", "#F21A00"),
    name = "1939 HOLC Grades")+
  theme_void(base_family = "Roboto Condensed")

# Tracts + HOLC without basemap
ggplot()+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          inherit.aes = FALSE,
          lwd = 0,
          alpha = .6)+
  geom_sf(data = st_intersection(jc_tracts_20, jc_boundaries), 
          fill = NA,
          lwd = .5,
          color = "grey30",
          inherit.aes = FALSE)+
  coord_sf(crs = st_crs(4326))+ 
  scale_fill_manual(
    values = c("#3B9AB2", "#EBCC2A", "#F21A00"),
    name = "1939 HOLC Grades")+
  theme_void(base_family = "Roboto Condensed")


# Super pale grayed-out terrain lines:
ggmap(jc_map, darken = c(0.7, "white"))+
  theme_void()

# Standard terrain lines:
ggmap(jc_map)+
  theme_void()

# Model for how to change overlay CRS back to fit ggmap stamen baselayer's WGS84:
# via http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/mapping.html

ggmap(man_basemap) +
  geom_sf(data = man_noise_rate_sf, aes(fill=noise_rate_cat), inherit.aes = FALSE) +
  scale_fill_brewer(palette = "OrRd") +
  coord_sf(crs = st_crs(4326))

# 2: 2020 Census tract boundaries overlaid w/ 1939 redline areas
ggplot()+
  geom_sf(data = st_intersection(jc_tracts_20, jc_boundaries), 
          fill = "white")+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          lwd = 0,
          alpha = .6)+
  scale_fill_manual(values = c("#3B9AB2", "#EBCC2A", "#F21A00"))+
  theme_void()

# 3: Tract boundaries & HOLC grades over Stamen basemap:
ggmap(jc_map, darken = c(0.5, "white"))+  
  geom_sf(data = st_intersection(jc_tracts_20, jc_boundaries), 
          fill = NA,
          inherit.aes = FALSE)+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          inherit.aes = FALSE,
          lwd = 0,
          alpha = .6)+
  coord_sf(crs = st_crs(4326))+ 
  scale_fill_manual(values = c("#3B9AB2", "#EBCC2A", "#F21A00"))+
  theme_void()




ggmap(jc_map, darken = c(0.7, "white"))+
  geom_sf(data = holc, 
          aes(fill = holc_grade),
          lwd = 0,
          alpha = .6,
          inherit.aes = FALSE)+
  scale_fill_manual(values = c("#3B9AB2", "#EBCC2A", "#F21A00"))+
  coord_sf(crs = st_crs(4326))+  
  theme_void()

rent_burden_20 %>% 
  #mutate(majority_redlined = case_when(
  # maj_grade == "C" | maj_grade == "D" ~ "Redlined",
  #TRUE ~ "Not Redlined"
  #)) %>% 
  #group_by(maj_redlined, designated) %>%
  group_by(maj_redlined) %>% 
  summarise(rent_burden = median(estimate, na.rm = T)) %>%  
  ggplot()+
  geom_bar(aes(x = maj_redlined,
               y = rent_burden,
               fill = maj_redlined),
           #fill = designated),
           position = "dodge", 
           stat = "identity",
           show.legend = T)+
  labs (x = NULL, y = "Gross rent as % of median income",
        title = "Rent Takes 4% More Out of Household Income in Jersey City Redlined Neighborhoods.",
        subtitle = "Gross rent as percentage of household income Census tracts that were majority-graded 'Hazardous' or 'Definitely Declining' by the HOLC.",        
        caption = "Source: 2020 ACS 5-yr estimates; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change Database; tract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  geom_text(aes(label = rent_burden,
                x = maj_redlined,
                y = rent_burden),
            vjust = -.5,
            family = "Roboto Condensed",
            fontface = "bold",
            size = 5) +
  scale_fill_manual(values = c("#3B9AB2", "#E1AF00"))+
  expand_limits(y = c(0, 35))+
  guides(color = "none")+  
  theme_ipsum_rc(grid = "F")

rent_burden_20 %>% 
  group_by(maj_redlined, designated) %>%
  summarise(rent_burden = median(estimate, na.rm = T)) %>%  
  ggplot()+
  geom_bar(aes(x = maj_redlined,
               y = rent_burden,
               fill = designated),
           position = "dodge", 
           stat = "identity",
           show.legend = T, )+
  labs (x = NULL, y = "Gross rent as % of median income",
        title = "... but within redlined areas, historic district \nstatus means gaining or losing twice that.",
        subtitle = "Gross rent as percentage of household income in Census tracts \nmajority-graded 'Hazardous' or 'Definitely Declining' by the HOLC.",        
        caption = "Source: U.S. Census Bureau 2020 ACS 5-yr estimates; tract-level HOLC grades of 'C' or 'D' \ndigitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  geom_text(aes(label = rent_burden,
                x = maj_redlined,
                y = rent_burden + 2),
            #vjust = -.5,
            #hjust = -.5,
            position = position_dodge2(width = 0.9, preserve = "single"), 
            family = "Roboto Condensed",
            fontface = "bold",
            size = 5) +
  scale_fill_manual(values = c("#3B9AB2", "#E1AF00"))+
  expand_limits(y = c(0, 35))+
  labs(color = NULL)+
  theme_ipsum_rc(grid = "F")+
  theme(legend.position="top", legend.title = element_blank())
