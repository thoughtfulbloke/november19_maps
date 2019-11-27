# occupational count from figure.nz
# https://figure.nz/table/cjmQpad7xQTh3bu7
# regional boundaries from koordinates
# https://koordinates.com/from/datafinder.stats.govt.nz/layer/87753/

library(sf)
library(rmapshaper)
library(ggplot2)
library(dplyr)

regional_kml <- st_read("../kx-regional-council-2015-v1-00-clipped-KML/regional-council-2015-v1-00-clipped.kml", quiet=TRUE)
regional <- ms_simplify(regional_kml,  keep=0.05) 
censusOc <- read.csv("../Census_Detailed_occupation_by_sex_and_Region_2013.csv",
                     stringsAsFactors = FALSE)
fun <- censusOc %>%
  filter(Occupation.Level.5 %in% c("Entertainer or Variety Artist","Prison Officer"),
         Sex=="Total", Region !=	"Total New Zealand") %>%
  arrange(Region, Occupation.Level.5) %>%
  group_by(Region) %>% summarise(ratio = Value[1] - Value[2], comedy=Value[1], serious=Value[2]) %>%
  ungroup() %>%
  select(Name=Region, ratio) %>% arrange(Name)


regional %>% filter(Name != "Area Outside Region") %>% arrange(Name) %>%
  mutate(comedy = fun$ratio) %>%
  ggplot(aes(geometry=geometry, fill=comedy)) + 
  geom_sf(size=0.01, colour="white") +
  scale_fill_viridis_c(name = "Comedy vs. Tragedy") + 
  theme_void() + ylim(-48,-34.8) + xlim(167,180) 


