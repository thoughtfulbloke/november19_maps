# solid earth tide
# data from using the DOS exe 'solid' from
# https://geodesyworld.github.io/SOFTS/solid.htm
# using robbie bruns statue, Dunedin, 2019-11-2 as input

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)

nov2rob <- read.table("solid.txt", skip=2)
names(nov2rob) <- c("second","m_north", "m_east", "m_up")
nov2rob %>% mutate(to_north=lead(m_north), to_east=lead(m_east),
                   when = case_when(second == 21600 ~ "6 am",
                                    second == 43200 ~ "noon",
                                    second == 64800 ~ "6 pm",
                                    TRUE ~ "")) %>%
  ggplot(aes(x=m_east,y=m_north, colour=m_up, xend=to_east, yend=to_north, label=when)) + 
  geom_segment(size=0.3) + scale_color_viridis_c("metres up") + coord_equal() + 
  geom_text_repel(nudge_x = -0.003) + theme_tufte() +
  ggtitle("Map of the 60 second solid earth tide generated location changes of Robert Burns statue, Dunedin, New Zealand for the day of 2nd November 2019 (UTC)") + xlab("position East (metres)") +
  ylab("position North (metres)")
