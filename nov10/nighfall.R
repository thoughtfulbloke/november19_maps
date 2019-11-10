library(geosphere)
library(suncalc)
library(lubridate)
library(dplyr)
library(ggplot2)
library(maps)
library(cowplot)
# nz bounding box
top_left = c(166,-33)
top_right = c(179,-33)
bottom_left = c(166,-48)
bottom_right = c(179,-48)
distVincentyEllipsoid(top_left,bottom_left) 
up <- seq(from=-48, to=-33, length.out = 16655/5)
distVincentyEllipsoid(top_left,top_right) 
across <- seq(from=166, to=179, length.out = 12140/5)
pairs <- expand.grid(across,up)
# shortest night 2019-12-22
# longest night 2019-6-22
summer <- data.frame(lat=pairs$Var2, lon=pairs$Var1)
summer$date <- as.Date("2019-12-22")
summer_nights <- getSunlightTimes(data=summer, keep="night") #takes a while
nightfall_begins <- min(summer_nights$night)
nightfall_ends <- max(summer_nights$night)
nightfall_length <- (difftime(nightfall_ends,nightfall_begins, units="secs"))
nightfall_mid <- nightfall_begins + nightfall_length / 2
nightline <- summer_nights %>% filter(nightfall_mid > night) %>%
  arrange(lat, lon) %>%
  group_by(lat) %>% slice(1) %>% ungroup() %>% select(lon,lat)
summernight <- bind_rows(nightline,
                         data.frame(lon=c(top_left[1], top_right[1]),
                                    lat=c(top_left[2], top_right[2])))
nz <- map_data("nz")
shortest <- ggplot() + 
  geom_polygon(data=summernight, aes(lon,lat), fill="black") +
  geom_polygon(data=nz, aes(long, lat, group = group), fill="white", 
               colour="black", size=.2) +
  geom_polygon(data=summernight, aes(lon,lat), fill=NA,colour="black",size=2) +
  theme_void() + coord_quickmap() + ggtitle("Halfway into nightfall, shortest night")

winter <- data.frame(lat=pairs$Var2, lon=pairs$Var1)
winter$date <- as.Date("2019-6-22")
winter_nights <- getSunlightTimes(data=winter, keep="night") #takes a while
nightfall_begins <- min(winter_nights$night)
nightfall_ends <- max(winter_nights$night)
nightfall_length <- (difftime(nightfall_ends,nightfall_begins, units="secs"))
nightfall_mid <- nightfall_begins + nightfall_length / 2
nightline <- winter_nights %>% filter(nightfall_mid > night) %>%
  arrange(lat, lon) %>%
  group_by(lat) %>% slice(1) %>% ungroup() %>% select(lon,lat)
winternight <- bind_rows(nightline,
                         data.frame(lon=c(top_right[1], bottom_right[1]),
                                    lat=c(top_right[2], bottom_right[2])))
longest <- ggplot() + 
  geom_polygon(data=winternight, aes(lon,lat), fill="black") +
  geom_polygon(data=nz, aes(long, lat, group = group), fill="white", 
               colour="black", size=.2) +
  geom_polygon(data=winternight, aes(lon,lat), fill=NA,colour="black",size=2) +
  theme_void() + coord_quickmap() + ggtitle("Halfway into nightfall, longest night")

plot_grid(shortest, longest)


