# occupational count from figure.nz
# https://figure.nz/table/cjmQpad7xQTh3bu7
# regional boundaries from koordinates
# https://koordinates.com/from/datafinder.stats.govt.nz/layer/87753/

library(sf)
library(rmapshaper)
library(dplyr)
library(ggplot2)
library(viridis)
regional_kml <- st_read("../kx-regional-council-2015-v1-00-clipped-KML/regional-council-2015-v1-00-clipped.kml", quiet=TRUE)
regional <- ms_simplify(regional_kml,  keep=0.05) 
jobs <- read.csv("../Census_Detailed_occupation_by_sex_and_Region_2013.csv", stringsAsFactors = FALSE)

lvl <- c('0 - 9', '10 - 19', '20 - 29', '30 - 39', '40 - 49', '50 - 59', '60 - 69', '70 - 79', '80 - 89', '90 - 99', '100 - 109', '110 - 119', '120 - 129', '130 - 139', '140 - 149', '150 - 159', '160 - 169', '170 - 179', '180 - 189', '190 - 199', '200 - 209', '210 - 219', '220 - 229', '230 - 239', '240 - 249', '250 - 259', '260 - 269', '270 - 279', '280 - 289', '290 - 299', '300 - 309')
fillsteps <- c("black",viridis_pal()(length(lvl)))
statmath <- jobs %>% filter(Occupation.Level.5 %in% c("Statistician", "Mathematician"),
                            Sex == "Total", Region !=	"Total New Zealand") %>%
  arrange(Region, Occupation.Level.5) %>% group_by(Region) %>%
  summarise(statsgreater = Value[2] - Value[1], isnone = Value[2] == 0 & Value[1] == 0) %>%
  ungroup() %>%
  mutate(fillvar = ifelse(isnone, "none of either", paste(10* floor(statsgreater/10), "-", 10*(floor(statsgreater/10))+9)),
         fillfact = factor(fillvar, levels = c("none of either", lvl))) %>% select(Name=Region, fillfact) %>%
  arrange(Name)

regional %>% filter(Name != "Area Outside Region") %>% arrange(Name) %>%
  mutate(fillfact = statmath$fillfact) %>%
  ggplot(aes(geometry=geometry, fill=fillfact)) + 
  geom_sf(size=0.01, colour="white") +
  scale_fill_manual(name = "2013 NZ census how many\nmore statisticians than\n mathematicians", 
                    values=fillsteps[c(1,2,3,13,14,31)]) + 
  theme_void() + ylim(-48,-34.8) + xlim(167,180) 

