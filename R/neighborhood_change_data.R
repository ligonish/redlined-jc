# Reading in Geolytics' Neighborhood Change Database values for decennial census, JC

library(tidyverse)
library(janitor)
library(tidycensus)

library(viridis)
library(hrbrthemes)

hh_inc_2010 <- read_csv("data/Neighborhood_Change_Database/avg_hh_inc_10.csv") %>% 
  clean_names() %>% 
  mutate(geoid = as.character(areakey))  # missed this & had to re-download the variable for 2010 ACS

decennial_h_d <- read_csv("data/Neighborhood_Change_Database/jc_decennial_1970_2010.csv") %>% 
  clean_names() %>% 
  mutate(geoid = as.character(areakey)) %>% 
  inner_join(maj_d) %>% 
  mutate(any_hist = case_when(
    any_hist == "No" ~ "No",
    areakey == 34017004102 ~ "No",  # West Bergen wasn't designated till 2015/2016
    TRUE ~ "Yes"
  )) %>% 
  inner_join(hh_inc_2010)
  #filter(areakey %in% c(34017002200, 34017002300, 34017002400, 34017003500, 34017004102, 34017006400, 34017006500, 34017007000, 34017007200, 34017007500))

rm(hh_inc_2010)

timeline_80 <- decennial_h_d %>% 
  group_by(any_hist) %>% 
  mutate(hh_inc_usd_2020 = avhhin8 * 3.78) %>% 
  summarize(year = 1980,
            hh_inc = median(hh_inc_usd_2020),
            pct_wh = median(100 * shrnhw8),
            pct_bl = median(100 * shrblk8),
            pct_hplx = median(100 * shrhsp8),
            wh_own = median(ownoccw8),
            bl_own = median(ownoccb8),
            hplx_own = median(ownocch8),
            wh_rent = median(rntoccw8),
            bl_rent = median(rntoccb8),
            hplx_rent = median(rntocch8)
  )

timeline_90 <- decennial_h_d %>% 
  group_by(any_hist) %>% 
  mutate(hh_inc_usd_2020 = avhhin9 * 2.13) %>% 
  summarize(year = 1990,
            hh_inc = median(hh_inc_usd_2020),
            pct_wh = median(100 * shrnhw9),
            pct_bl = median(100 * shrblk9),
            pct_hplx = median(100 * shrhsp9),
            wh_own = median(ownoccx9),
            bl_own = median(ownoccb9),
            hplx_own = median(ownocch9),
            wh_rent = median(rntoccx9),
            bl_rent = median(rntoccb9),
            hplx_rent = median(rntocch9)
  )

timeline_00 <- decennial_h_d %>% 
  group_by(any_hist) %>%
  mutate(hh_inc_usd_2020 = avhhin0 * 1.57) %>% 
  summarize(year = 2000,
            hh_inc = median(hh_inc_usd_2020),
            pct_wh = median(100 * shrnhw0),
            pct_bl = median(100 * shrblk0),
            pct_hplx = median(100 * shrhsp0),
            wh_own = median(ownoccx0),
            bl_own = median(ownoccb0),
            hplx_own = median(ownocch0),
            wh_rent = median(rntoccx0),
            bl_rent = median(rntoccb0),
            hplx_rent = median(rntocch0)
  )


timeline_10 <- decennial_h_d %>%
  group_by(any_hist) %>% 
  mutate(hh_inc_usd_2020 = avhhin1a * 1.22) %>% 
  summarize(year = 2010,
            hh_inc = median(hh_inc_usd_2020),
            pct_wh = median(100 * shrnhw1),
            pct_bl = median(100 * shrblk1),
            pct_hplx = median(100 * shrhsp1),
            wh_own = median(ownoccx1),
            bl_own = median(ownoccb1),
            hplx_own = median(ownocch1),
            wh_rent = median(rntoccx1),
            bl_rent = median(rntoccb1),
            hplx_rent = median(rntocch1)
  )


timeline <- decennial_h_d %>% 
  group_by(any_hist) %>% 
  mutate(hh_inc_usd_2020 = avhhin7 * 7.25) %>% 
  summarize(year = 1970,
            hh_inc = median(hh_inc_usd_2020),
            pct_wh = median(100 * shrwht7),
            pct_bl = median(100 * shrblk7),
            pct_hplx = median(100 * shrhsp7),
            wh_own = "NA",
            bl_own = median(ownoccb7),
            hplx_own = median(ownocch7),
            wh_rent = "NA",
            bl_rent = median(rntoccb7),
            hplx_rent = median(rntocch7)) %>% 
  mutate(wh_own = as.numeric(na_if(wh_own, "NA")),
         wh_rent = as.numeric(na_if(wh_rent, "NA"))) %>% 
  bind_rows(timeline_80, timeline_90, timeline_00, timeline_10)

# add 2020 census estimates for same approx variables

timeline_20 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  variables = c("B19025_001",   # Total households
                "B25003_001",   # Aggregate tract income (to impute a decennial-ish "average hh inc")
                "B01003_001",   # Total pop
                "B01001H_001",  # Pop white, not Hispanic/Latinx 
                "B01001B_001",  # Pop Black
                "B01001I_001",  # Pop Hispanic/Latinx
                "B25003H_002",  # White owner-occ
                "B25003H_003",  # White renter-occ
                "B25003B_002",  # Bl owner-occ
                "B25003B_003",  # Bl renter-occ
                "B25003I_002",  # Hplx owner-occ
                "B25003I_003"   # Hplx renter-occ
                ),
  geometry = F,
  year = 2020,
  cb = FALSE,
  output = "wide",
  cache_table = T) %>% 
  mutate(avhhin2 = B19025_001E / B25003_001E,
         pct_wh = 100 * (B01001H_001E / B01003_001E),
         pct_bl = 100 * (B01001B_001E / B01003_001E),
         pct_hplx = 100 * (B01001I_001E / B01003_001E)) %>% 
  clean_names() %>% 
  remove_constant() %>%
  inner_join(maj_d) %>% 
  group_by(any_hist) %>% 
  mutate(any_hist = case_when(
    any_hist == "No" ~ "No",
    geoid == 34017004102 ~ "No",  # West Bergen wasn't designated till 2015/2016
    TRUE ~ "Yes"
  )) %>% 
  summarise(year = 2020,
            hh_inc = median(avhhin2, na.rm = T),
            pct_bl = median(pct_bl, na.rm = T),
            pct_wh = median(pct_wh, na.rm = T),
            pct_hplx = median(pct_hplx, na.rm = T),
            wh_own = median(b25003h_002e, na.rm = T),
            bl_own = median(b25003b_002e, na.rm = T),
            hplx_own = median(b25003i_002e, na.rm = T),
            wh_rent = median(b25003h_003e, na.rm = T),
            bl_rent = median(b25003b_003e, na.rm = T),
            hplx_rent = median(b25003i_003e, na.rm = T)
            )

timeline <- timeline %>% 
  bind_rows(timeline_20)
  
# saving some basics so I don't lose my mind/data ------------------------------

write_csv(maj_d, "data_build/maj_d.csv")
write_csv(timeline, "data_build/timeline.csv")
write_csv(maj_grades, "data_build/maj_grades.csv")
write_csv(tracts, "data_build/tracts.csv")

# Plot ------------------------------------------------------------------------

#### USE THIS: Income in Redlined Historic/Not Historic, 1970-2020
timeline %>%
  filter(maj_redlined == "Redlined") %>% 
  ggplot(aes(x = year,
             y = hh_inc,
             group = designated,
             color = designated))+
  geom_line(size = 2,
            alpha = .8)+
  geom_vline(xintercept = 1980, color = "black", size = .3, linetype = "dashed")+
  labs (x = NULL, y = "Median Household Income in Inflation-Adjusted 2020 USD",
        title = "Redlined Tract Incomes Spiked After Historic Distric Designation.",
        subtitle = "Median 2020 household income estimates across Jersey City census tracts graded 'D' by HOLC.",        
        caption = "Source: 2020 ACS 5-yr estimates; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change Database; \ntract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  geom_text(aes(label = ifelse(year == 2020, designated, NA),
                y = hh_inc,
                color = designated),
            hjust=-.3,
            family = "Roboto Condensed",
            fontface = "bold",
            size = 4) +
  expand_limits(x = c(1970, 2030))+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_color_manual(values = c("#3B9AB2", "#E1AF00"))+
  #scale_color_viridis(option = "D", discrete = T, end = .7)+
  guides(color = "none")+  
  theme_ipsum_rc(axis = "x", grid = "Y")

#### Homeownership in Redlined Historic/Not Historic, 1970-2020
timeline %>%
  ggplot(aes(group = designated,
             color = designated))+
  geom_line(aes(x = year,
                y = own),
            size = 1.5,
            alpha = .6)

timeline %>%
  mutate(pct_wh_own = wh_own / (rent + own)) %>% 
  ggplot(aes(x = year,
             y = pct_wh_own,
             group = designated,
             color = designated))+
  geom_line(size = 1.5,
            alpha = .6)

timeline %>%
  mutate(pct_bl_own = bl_own / own) %>% 
  ggplot(aes(x = year,
             y = bl_own,
             group = designated,
             color = designated))+
  geom_line(size = 1.5,
            alpha = .6)


  geom_vline(xintercept = 1980, color = "black", size = .3, linetype = "dashed")+
  labs (x = NULL, y = "Average owner-occupied households per formerly-redlined tract",
        title = "Redlined Homeownership After Historic Designation.",
        subtitle = "Average median household incomes across majority-redlined Jersey City censusthat were and were not designated historic districts.",        
        caption = "Source: 2020 ACS 5-yr estimates; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change Database; tract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  geom_text(aes(label = ifelse(year == 2020, own, NA),
                y = own,
                color = designated),
            hjust=-.7,
            family = "Roboto Condensed",
            fontface = "bold",
            size = 4) +
  expand_limits(x = c(1970, 2030))+
  #scale_y_continuous(labels = scales::dollar_format())+
  scale_color_viridis(discrete = T)+
  guides(color = "none")+  
  theme_ipsum_rc(axis = "x", grid = "Y")








# Pct_races

timeline %>%
  filter(maj_redlined == "Redlined") %>% 
  ggplot(aes(x = year,
             y = pct_bl,
             group = designated,
             color = designated))+
  geom_line(size = 2,
            alpha = .6)+
  labs (x = NULL, y = "Percent Black Residents",
        title = "Redlined Jersey City Census Tracts' Average Percentages of \nBlack Residents, by Historic District Designation Status",
        subtitle = "Via decennial Census estimates of tract-level black population, 1970-2020.",
        caption = "Source: 2020 estimates imputed from ACS 5-yr; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change \nDatabase; tract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+  
  geom_text(aes(label = ifelse(year == 2020, designated, NA),
                y = pct_bl,
                color = designated),
            hjust=-.3,
            family = "Roboto Condensed",
            fontface = "bold",
            size = 4) +
  scale_color_manual(values = c("#3B9AB2", "#F21A00"))+
  guides(color = "none")+
  expand_limits(x = c(1970, 2030))+
  theme_ipsum_rc()

# Tenure by race, just starting
timeline %>%
  filter(maj_redlined == "Redlined") %>% 
  ggplot(aes(x = year,
             y = wh_own,
             group = designated,
             color = designated))+
  geom_line(size = 1.5,
            alpha = .6)+
  labs (x = NULL, y = "Owner-Occupied Units",
        title = "White Homeowners: Hist/Not Hist",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc()
  
# Try faceting by tenure

timeline %>%
  filter(maj_redlined == "Redlined") %>% 
  pivot_longer(cols = 13:18, names_to = "tenure", values_to = "n_hh") %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = designated,
             color = designated))+
  geom_line(size = 2,
            alpha = .7)+
  facet_wrap(~tenure, ncol = 2) +
  labs (x = NULL, y = "Households",
        title = "Historic designation status ",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_manual(values = c("#3B9AB2", "#EBCC2A"))+
  theme_ipsum_rc(grid = "XY")






# or faceting by historic status [AAAAHHH NOOOO]
# Homeownership, by historic designation & race
timeline %>%
  filter(maj_redlined == "Redlined") %>%
  pivot_longer(cols = 13:18, names_to = "tenure", values_to = "n_hh") %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_rent",
         tenure != "wh_rent",
         tenure != "hplx_rent") %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = tenure,
             color = tenure))+
  scale_color_manual(values = c("#3B9AB2", "#F21A00", "#E1AF00"),
                     labels = c("Black", "Hispanic/Latinx", "White"))+
  facet_wrap(~designated) +
  geom_line(size = 2,
            alpha = .6)+
  labs (x = NULL, y = "Households",
        title = "Redlined Households by Tenure, Race, & Historic Designation Status: Homeowners",
        subtitle = "Average household count across all Jersey City census tracts graded 'C' or 'D' in 1939.",
        caption = "Source: 2020 ACS 5-yr estimates; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change Database; \ntract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  theme_ipsum_rc(grid = "XY")+
  theme(legend.title = element_blank())

 
# Renters, by historic designation & race
timeline %>%
  filter(maj_redlined == "Redlined") %>%
  pivot_longer(cols = 13:18, names_to = "tenure", values_to = "n_hh") %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_own",
         tenure != "wh_own",
         tenure != "hplx_own") %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = tenure,
             color = tenure))+
  scale_color_manual(values = c("#3B9AB2", "#F21A00", "#E1AF00"),
                     labels = c("Black", "Hispanic/Latinx", "White"))+
  facet_wrap(~designated) +
  geom_line(size = 2,
            alpha = .6)+
  labs (x = NULL, y = "Households",
        title = "Redlined Households by Tenure, Race, & Historic Designation Status: Renters",
        subtitle = "Average household count across all Jersey City census tracts graded 'C' or 'D' in 1939.",
        caption = "Source: 2020 ACS 5-yr estimates; 1970-2010 U.S. Census decennial estimates fitted to 2010 census boundaries by Neighborhood Change Database; \ntract-level HOLC grades digitized by University of Richmond; Jersey City historic district boundaries via data.jerseycitynj.gov.")+
  theme_ipsum_rc(grid = "XY")+
  theme(legend.title = element_blank())

# As barplots

timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_rent",
         tenure != "wh_rent",
         tenure != "hplx_rent") %>% 
  ggplot(aes(x = tenure,
             y = n_hh,
             #group = designated,
             fill = designated))+
  facet_wrap(~year) +
  geom_bar(position = "dodge", 
           stat = "identity",
           show.legend = T)+
  labs (
        title = "Homeowners Hist/Not Hist",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc()

# Stacked Area? I should go to bed.

timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_rent",
         tenure != "wh_rent",
         tenure != "hplx_rent") %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = tenure,
             fill = tenure))+
  facet_wrap(~designated) +
  geom_bar(position = "dodge", 
           stat = "identity",
           show.legend = T)+
  labs (
    title = "Homeowners Hist/Not Hist",
    subtitle = "Here is a subtitle.",
    caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc()


# I think it's simpler just to separate out the race variable bc, tidy

test <- timeline %>% 
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  separate(tenure, sep = "_", into = c("race", "tenure"))
  
timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_rent",
         tenure != "wh_rent",
         tenure != "hplx_rent") %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = designated,
             color = designated))+
  facet_wrap(~tenure) +  
  geom_line(size = 1.5,
            alpha = .6)+
  labs (x = NULL, y = "Owner-Occupied Units",
        title = "Homeowners: Hist/Not Hist",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid = "XY")

timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure)) %>% 
  filter(tenure != "bl_own",
         tenure != "wh_own",
         tenure != "hplx_own") %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = designated,
             color = designated))+
  facet_wrap(~tenure) +  
  geom_line(size = 1.5,
            alpha = .6)+
  labs (x = NULL, y = "Renter-Occupied Units",
        title = "Renters: Hist/Not Hist",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid: "XY")

# Last one goddammit

timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure, levels = c("bl_own", "hplx_own", "wh_own", "bl_rent", "hplx_rent", "wh_rent"))) %>% 
  ggplot(aes(x = year,
             y = n_hh,
             group = designated,
             color = designated))+
  facet_wrap(~tenure, nrow = 2, strip.position="top") +  
  geom_line(size = 1.5,
            alpha = .6)+
  geom_vline(xintercept = 1980, color = "black", size = .2, linetype = "dashed")+
  labs (x = NULL, y = "Housing Units",
        title = "Tenure by Race: Hist/Not Hist",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid = F, axis = "X")

# With gghighlight -----------------------------------

library(gghighlight)

timeline %>%
  filter(maj_redlined == "Redlined") %>% 
  select(!ends_with("_rent")) %>% 
  pivot_longer(cols = 12:14, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure, levels = c("bl_own", "hplx_own", "wh_own", "bl_rent", "hplx_rent", "wh_rent"))) %>% 
  filter(designated == "Not Historic") %>% 
  ggplot()+
  geom_line(aes(x = year,
                y = n_hh,
                color = tenure),
            size = 1.5,
            alpha = .6)+
  geom_vline(xintercept = 1980, color = "black", size = .2, linetype = "dashed")+
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey80", 1))) +
  facet_wrap(~tenure, nrow = 1, strip.position="top") +  
  labs (x = NULL, y = "Housing Units",
        title = "Tenure by Race: Not Historic",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid = F, axis = "X")

timeline %>%
  select(!ends_with("_own")) %>% 
  pivot_longer(cols = 10:12, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure, levels = c("bl_own", "hplx_own", "wh_own", "bl_rent", "hplx_rent", "wh_rent"))) %>% 
  filter(designated == "Historic") %>% 
  ggplot()+
  geom_line(aes(x = year,
                y = n_hh,
                color = tenure),
            size = 1.5,
            alpha = .6)+
  geom_vline(xintercept = 1980, color = "black", size = .2, linetype = "dashed")+
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  facet_wrap(~tenure, nrow = 2, strip.position="top") +  
  labs (x = NULL, y = "Housing Units",
        title = "Tenure by Race: Not Historic",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid = F, axis = "X")

timeline %>%
  pivot_longer(cols = 11:16, names_to = "tenure", values_to = "n_hh") %>% 
  #separate(tenure, sep = "_", into = c("race", "tenure")) %>% 
  mutate(designated = factor(designated),
         tenure = factor(tenure, levels = c("bl_own", "hplx_own", "wh_own", "bl_rent", "hplx_rent", "wh_rent"))) %>% 
  filter(designated == "Historic") %>% 
  ggplot()+
  geom_line(aes(x = year,
                y = n_hh,
                color = tenure),
            size = 1.5,
            alpha = .6)+
  geom_vline(xintercept = 1980, color = "black", size = .2, linetype = "dashed")+
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  facet_wrap(~tenure, nrow = 2, strip.position="top") +  
  labs (x = NULL, y = "Housing Units",
        title = "Tenure by Race: Historic",
        subtitle = "Here is a subtitle.",
        caption = "Source: Census blah blah blah HOLC")+
  scale_color_viridis(discrete = T)+
  theme_ipsum_rc(grid = F, axis = "X")

