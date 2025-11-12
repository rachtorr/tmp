# cal fire data - potential plots for poster 

# shapefile and timeseries 

library(tidyverse)
library(readr)
library(lubridate)
library(sf)

setwd("C:/Users/rdt47/Documents/GitHub/six_rivers")

# load shapefile 
ca_fires_shp <- st_read("California_Historic_Fire_Perimeters_-247493000743518952")

# load six rivers shapefile
sr_shp <- st_read("six_rivers/")

# filter to Six Rivers NF 
srf = ca_fires_shp %>% dplyr::filter(UNIT_ID=="SRF")

# save shapefile 
# write_sf(srf, "../six_rivers/srf_historic_fire_perimeters.shp")

# plot for fires occurring after 2010 
srf %>% dplyr::filter(YEAR_>2010) %>% 
  ggplot(aes(fill=as.factor(CAUSE))) + 
  geom_sf() 

unique(srf$CAUSE)

# match cause numbers to text 
causes <- read.csv("fires data - Cause codes.csv") %>% 
  rename(CAUSE=code, cause_text=X)

srf_c <- left_join(srf, causes, by='CAUSE')

# plot for fires occurring after 2010 
srf_c %>% dplyr::filter(YEAR_>2010) %>% 
  ggplot(aes(fill=cause_text)) + 
  geom_sf() + 
  scale_fill_brewer(palette='Set2') # changed color because rainbow is hard to read 

# other option - interactive
# package leaflet 
library(mapview)
# plot 
six_rivers = srf_c %>% dplyr::filter(YEAR_>2010)
mapview(six_rivers, 
        zcol = "cause_text")     


# options to plot over a static basemap
install.packages("ggmap")
library(ggmap)

# Source data - transform center of all points 
xy <- st_coordinates(srf$geometry) %>% 
  as.data.frame() %>% 
  dplyr::select(X,Y) %>%
  summarize_all(mean) %>% 
  st_as_sf(coords=c("X","Y"), crs=3857) %>% 
  st_transform((crs=4236))

# get API for basemap - need credit card 
register_google()

base = get_map(location=c(lon=st_coordinates(xy)[1], 
                          lat=st_coordinates(xy)[2]))

# other option - static - basemap package 
# package basemap 
library(basemap)
# Set the basemap extent based on your data
basemap_ggplot(six_rivers) + geom_sf(data=six_rivers)

# other option - static - ggspatial package 
library(ggspatial)
# plot like regular ggplot 
ggplot() +
  annotation_map_tile(type = "osm", zoom=10) +
  geom_sf(data=sr_shp, fill="grey") + 
  geom_sf(data = six_rivers, aes(fill = cause_text), color = "white", alpha = 0.9) +
  coord_sf() +
  #scale_fill_brewer(palette="Set2")
  theme_minimal() +
  # spatial-aware automagic scale bar
  annotation_scale(location = "tl") +
  # spatial-aware automagic north arrow
  annotation_north_arrow(location = "br", which_north = "true")


## time series plots - causes over time 

srf_c %>% as.data.frame %>% 
  rename(year = YEAR_,
         acres=GIS_ACRES) %>% 
  ggplot(aes(x=year, y=acres, fill=cause_text)) +
  geom_col() 

# filtered years 
srf_c %>% as.data.frame %>%
  rename(year = YEAR_,
         acres=GIS_ACRES, 
         cause= cause_text) %>% 
  filter(year>2000) %>% 
  ggplot(aes(x=year, y=acres, fill=cause)) +
  geom_col() +
  scale_fill_brewer(palette="Set2")
