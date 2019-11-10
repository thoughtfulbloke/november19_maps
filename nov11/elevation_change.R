Times <- as.POSIXct('2019-11-11', tz = 'UTC') + (0:96)*(15*60)
library(earthtide)
library(dplyr)
library(ggplot2)
library(ggthemes)
kaitaia <- calc_earthtide(utc = Times,
               method = c("vertical_displacement", "n_s_displacement"),
               latitude = -35.1173,
               longitude = 173.2676)
names(kaitaia)[2:3] <- c("k_vertical", "k_horizontal")
bluff <- calc_earthtide(utc = Times,
                          method = c("vertical_displacement", "n_s_displacement"),
                          latitude = -46.5996,
                          longitude = 168.3457)
names(bluff)[2:3] <- c("b_vertical", "b_horizontal")
both <- bind_cols(kaitaia, bluff[,2:3])
both %>%
  mutate(kv = k_vertical - k_vertical[1],
         bv = b_vertical - b_vertical[1],
         kh = k_horizontal - k_horizontal[1],
         bh = b_horizontal - b_horizontal[1],
         difv = kv - bv, difh = kh - bh,
         difv2 = lead(difv), difh2 = lead(difh),
         event = c("24 hour start", rep("",95),"24 hour end"),
         event = factor(event, levels = c("24 hour start", "","24 hour end"))
         ) %>%
ggplot(aes(x=difh, y=difv, xend=difh2, yend=difv2,
           shape=event, colour=event)) + 
  geom_segment() + geom_point() + theme_tufte() +
  ylab("difference in elevation (mm)") +
  xlab("difference in north-south distance (mm)") +
  ggtitle("Difference in North-South distance and elevation (mm)
between Kaitaia & Bluff for 2019-11-11 UTC (15 minute steps)
relative to start of 24 hours") + scale_colour_colorblind()
  
  
