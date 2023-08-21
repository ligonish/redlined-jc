# Thursday March 30, 2023
# Testing st_intersection function so I understand it better
# See https://stackoverflow.com/questions/62442150/why-use-st-intersection-rather-than-st-intersects

library(sf)
library(dplyr)

# create square
s <- rbind(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc

plot(s)

# create rectangle
r <- rbind(c(-1, 2), c(11, 2), c(11, 4), c(-1, 4), c(-1, 2)) %>% 
  list %>% 
  st_polygon %>% 
  st_sfc

plot(r, add= TRUE, lty = 2)     # Funny; "lty" is just Base R's "linetype", but spelling out the word "linetype" gives you something else & doesn't work? Meh.

# intersect points and square with st_intersection
i <- st_intersection(s, r)

plot(i, lty = 2, col = "red")
