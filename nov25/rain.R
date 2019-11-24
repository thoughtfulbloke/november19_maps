# ERA5 hourly data on single levels from 1979 to present, monthly average nov 1979 and 2018
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
# netCDF format
ncfile <- "1979.nc"
nc <- ncdf4::nc_open(ncfile)
datum1979 <- ncdf4::ncvar_get(nc)
#matrix of long (easterly), lat(northly), with val=precipertation
ncfile <- "2018.nc"
nc <- ncdf4::nc_open(ncfile)
datum2018 <- ncdf4::ncvar_get(nc)
increase <- (datum2018 - datum1979) * 10000 
asdf <- as.data.frame(increase)
library(tidyr)
library(dplyr)
se <- asdf %>% mutate(easting = row_number()) %>%
  gather(northing, increase, V1:V721) %>%
  mutate(northery = as.numeric(gsub("V","", northing))) %>%
 # filter(northery < 361, easting < 721) %>%
  mutate(latitude=northery/4 - 90, longitude=easting/4,
         longitude = ifelse(longitude > 180, longitude-360, longitude))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_coastline(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_point(data=se,aes(x=longitude,y=latitude,colour=increase),size=0.5)+
  geom_sf() + scale_colour_viridis_c(direction = -1) +
  coord_sf(ylim = c(-50, 10), xlim = c(100, 180), expand = FALSE) +
  theme(legend.position = "none")
