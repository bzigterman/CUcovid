library(maps)
library(sf)
library(tidyverse)
library(usmap)
library(tmap)

plot_usmap("counties", data = countypov, values = "pct_pov_2014", include = "IL") +
  ggplot2::scale_fill_continuous(low = "green", high = "red", guide = FALSE)

data("World")
tm_shape(World) +
  tm_polygons("HPI")

plot(st_geometry(nc))
?st_geometry
?geom_sf

# read shp file

# map data to shp file

# select only nearby counties

# plot map