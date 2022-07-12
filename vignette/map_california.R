library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(weatherQC)

world <- ne_countries(scale = "medium", returnclass = "sf")

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

sites_ca <- st_as_sf(rbind(target_info, neighbour_info), coords = c("Longitude", "Latitude"), 
                     crs = 4326, agr = "constant")

sites_ca$target_station <- c('target station', rep('auxillary station', 12))

library(elevatr)
CA <- states[states$ID %in% c('california','oregon'),]

elevation <- get_elev_raster(CA, z = 6, neg_to_na = TRUE)
tm_shape(elevation, bbox = extent(-81.75, -34.25, -56, 12.75))

library(raster)
library(tmap)
library(rgdal)

states <- readOGR('../masterarbeit/raingen-and-chill/data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')

map_california <- tm_shape(elevation, bbox = extent(-125, -114, 32.5, 42.5)) +
  tm_raster(breaks = seq(0, 4500, 1500), style = "cont", title = "Elevation (a.s.l.)",
            legend.format = list(suffix = " m", text.align = "center")) +
  tm_shape(states) + 
  tm_borders(col = 'grey40') +
  tm_graticules(lines = FALSE, labels.size = 0.7, labels.col = "black") +
  tm_shape(sites_ca) +
  tm_symbols(size = 0.3, shape = 'target_station',col = 'target_station', 
             palette=c('auxillary station' ='grey70', 'target station' ='red'),
             shapes = c(21, 25), legend.col.show = FALSE, legend.shape.show = F)+
  tm_compass(position = c(0.12, 0.15), text.size = 1, text.color = 'white') +
  tm_scale_bar(position = c(0.06, 0.02), bg.color = 'transparent', text.size = 1, color.dark = "grey20",text.color = 'white',) +
  tm_add_legend(type = "symbol", labels = c("target stations", "auxillary stations"),
                shape = c(25,21),col = c('red', 'grey70'), 
                title = "Stations for Analysis\nof Quality Control Methods")+
  tm_layout(legend.outside = FALSE,
            legend.position = c(0.7, 0.7),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.text.color = 'black',
            legend.title.color = 'black',
            bg.color = "black",
            legend.bg.color = 'white',
            legend.bg.alpha = 0.75,
            attr.color = "white",
            outer.bg.color = "white")

tmap_save(map_california, "vignette/figures/map_california_qc.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)
