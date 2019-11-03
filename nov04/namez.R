# New Zealand Gazetteer of place names
# https://www.linz.govt.nz/regulatory/place-names/find-place-name/new-zealand-gazetteer-place-names

library(conflicted)
library(vroom)
library(dplyr)
library(ggplot2)
library(hexbin)
conflict_prefer("filter", "dplyr", quiet=TRUE)
everywhere <- vroom("../nov04data/gaz_names.csv", delim=",",
                    col_types = cols(
                      name_id = col_double(),
                      name = col_character(),
                      status = col_character(),
                      feat_id = col_double(),
                      feat_type = col_character(),
                      nzgb_ref = col_character(),
                      land_district = col_character(),
                      crd_projection = col_character(),
                      crd_north = col_double(),
                      crd_east = col_double(),
                      crd_datum = col_character(),
                      crd_latitude = col_double(),
                      crd_longitude = col_double(),
                      info_ref = col_character(),
                      info_origin = col_character(),
                      info_description = col_character(),
                      info_note = col_character(),
                      feat_note = col_character(),
                      maori_name = col_character(),
                      cpa_legislation = col_character(),
                      conservancy = col_character(),
                      doc_cons_unit_no = col_character(),
                      doc_gaz_ref = col_character(),
                      treaty_legislation = col_character(),
                      geom_type = col_character(),
                      accuracy = col_character(),
                      gebco = col_character(),
                      region = col_character(),
                      scufn = col_character(),
                      height = col_character(),
                      ant_pn_ref = col_logical(),
                      ant_pgaz_ref = col_logical(),
                      scar_id = col_character(),
                      scar_rec_by = col_character(),
                      accuracy_rating = col_double(),
                      desc_code = col_character(),
                      rev_gaz_ref = col_character(),
                      rev_treaty_legislation = col_character()
                    ))

everywhere %>% filter(!is.na(crd_east), !is.na(crd_north)) %>%
ggplot(aes(x=crd_east, y=crd_north)) +
  geom_hex(aes(alpha=..count..), fill="darkgreen") +
  theme_void() + scale_alpha_continuous(range=c(0,1)) +
  theme(plot.margin = margin(6, 6, 6, 6, "pt")) +
  coord_fixed(xlim=c(849547, 2239532)) 
