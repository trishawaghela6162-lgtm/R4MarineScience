library(sf)
library(terra)
library(tmap)
dat <- read.csv("data/workshop4/copepods_raw.csv")

head(dat)
library(ggplot2)

ggplot(dat, aes(
  x = longitude,
  y = latitude,
  colour = richness_raw
)) +
  geom_point()

ggplot(dat, aes(x = latitude, y = richness_raw)) +
  geom_point() +
  stat_smooth()

sdat <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

st_crs(sdat)

plot(sdat["richness_raw"])

tmap_mode("plot")

tm_shape(sdat) +
  tm_dots(fill = "richness_raw")

aussie <- st_read("data/workshop4/spatial-data/Aussie/Aussie.shp")

tm_shape(aussie) +
  tm_polygons(fill = "lightgrey") +
  tm_shape(sdat) +
  tm_dots(fill = "richness_raw")

shelf <- st_read("data/workshop4/spatial-data/aus_shelf/aus_shelf.shp")
tm_shape(shelf) +
  tm_borders(col = "orange", lwd = 1.5) +
  tm_shape(aussie) +
  tm_polygons(fill = "lightgrey") +
  tm_shape(sdat) +
  tm_dots(fill = "richness_raw")

map4 <- tm_shape(shelf) +
  tm_borders(col = "orange", lwd = 1.5) +
  tm_shape(aussie) +
  tm_polygons(fill = "lightgrey") +
  tm_shape(sdat) +
  tm_dots(fill = "richness_raw")

tmap_save(
  map4,
  filename = "outputs/Workshop_04_Copepod_Map.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

tmap_style("beaver")
map4

tmap_style("beaver")
map4

tmap_save(
  map4,
  "outputs/Workshop_04_Copepod_Map_Final.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)