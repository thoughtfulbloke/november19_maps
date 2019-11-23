# occupational count from figure.nz
# https://figure.nz/table/cjmQpad7xQTh3bu7
# regional boundaries from koordinates
# https://koordinates.com/from/datafinder.stats.govt.nz/layer/87753/

library(sf)
library(rmapshaper)
library(ggplot2)
regional_kml <- st_read("kx-regional-council-2015-v1-00-clipped-KML/regional-council-2015-v1-00-clipped.kml", quiet=TRUE)
regional <- ms_simplify(regional_kml,  keep=0.05) 
regional$pop2013 <- c(3, 324, 48, 30, 3, 9, 9, 36, 555, 0, 183, 33, 12, 12, 9, 0, NA)
ggplot(regional, aes(geometry=geometry, fill=pop2013)) + 
  geom_sf(size=0.01, colour="white") +
  scale_fill_viridis_c(name = "2013 NZ population of analyst programmers,\ncartographers, and statisticians") + 
  theme_void() + ylim(-48,-34.8) + xlim(167,180)

