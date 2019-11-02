library(readr)
library(dplyr)
library(stringr)
library(sf)
library(rmapshaper)
library(ggplot2)
# vehicle register from NZTA
# https://opendata-nzta.opendata.arcgis.com/datasets/motor-vehicle-register-1/data
# NZ TLA boundaries via koordinates
# https://koordinates.com/layer/2213-nz-territorial-local-authority-boundaries-apr-12/
cars <- read_csv("/Users/davidhood/Downloads/Motor_Vehicle_Register.csv")
powr <- cars %>% select(MOTIVE_POWER, TLA) %>% filter(!is.na(TLA))
#commonest car is silver
tlac <- powr %>% 
  group_by(TLA) %>% 
  summarise(percent = 100*sum(!is.na(MOTIVE_POWER) &
                                (str_detect(MOTIVE_POWER, fixed("ELECTR", ignore_case=TRUE))|
                                   str_detect(MOTIVE_POWER, fixed("HYBRID", ignore_case=TRUE))))/n()) 
tla_kml <- st_read("../nov03data/kx-nz-territorial-local-authority-boundaries-apr-12-KML/nz-territorial-local-authority-boundaries-apr-12.kml", quiet=TRUE)
tlas <- ms_simplify(tla_kml,  keep=0.05) 

# checking join columns
# tlac$TLA[!tlac$TLA %in% toupper(tlas$Name)]
# tlas$Name[!toupper(tlas$Name) %in% tlac$TLA]

combined <- tlas %>% mutate(TLA =case_when(Name == "Wanganui District" ~ "WHANGANUI DISTRICT",
                                       TRUE ~ toupper(as.character(Name)))) %>%
  left_join(tlac, by="TLA") %>% filter(!is.na(percent))

# this expresses the full colour range on the range of results
ggplot(combined, aes(geometry=geometry, fill=percent)) + 
  geom_sf(size=0.01, colour="white") +
         scale_fill_viridis_c(name = "Electric or Hybrid Vehicles\nas percent of total fleet") + 
  theme_void()

# or colour scale out of 100 percent, with the results within that
ggplot(combined, aes(geometry=geometry, fill=percent)) + 
  geom_sf(size=0.01, colour="white") +
  scale_fill_viridis_c(name = "Electric or Hybrid Vehicles\nas percent of total fleet",
                       end = max(combined$percent/100)) + 
  theme_void()


