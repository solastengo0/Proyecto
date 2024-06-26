library(ggplot2)
library(maps)
library(dplyr)
library(maptools)
library(rgdal)
library(here)
library(sf)
library(terra)

sp_depto <- st_read(here("data", "ine_ccz_mvd.shp"))

dframe_depto <- ggplot2::fortify(sp_depto)

ggplot(data = dframe_depto, aes(x = long, y = lat, group = group)) +
  geom_polygon()
