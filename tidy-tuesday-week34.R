library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(tidytuesdayR)
library(stringr)

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions

extinction <- plants %>% 
  group_by(continent, year_last_seen) %>% 
  summarize(na = sum(action_NA), n = n()) %>%
  mutate(prop_action = (n-na)/n) %>%
  filter(!is.na(year_last_seen)) %>%
  mutate(year_last_seen = str_replace(year_last_seen, "-", "-\n"),
         year_last_seen = factor(year_last_seen, 
                                 levels = c("Before 1900", 
                                            "1900-\n1919", 
                                            "1920-\n1939", 
                                            "1940-\n1959", 
                                            "1960-\n1979", 
                                            "1980-\n1999", 
                                            "2000-\n2020")),
         continent = factor(continent,
                            levels = c("North America",
                                       "Europe",
                                       "Asia",
                                       "South America",
                                       "Africa",
                                       "Oceania"))
         )




ggplot() +
  geom_bar(data = extinction, aes(x = year_last_seen, y = n, fill = prop_action), stat = "identity") +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  scale_x_discrete(label = function(year_last_seen) str_wrap(year_last_seen, width = 4)) +
  scale_fill_gradient2(low = "#B99C6B", 
                       high = "#668D3C", 
                       mid = "#BDD09F", 
                       midpoint = 0.5,
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               frame.colour = "black",
                                               barwidth = 15,
                                               barheight = 1.2)
                       ) +
  facet_wrap(~continent) +
  theme_minimal() +
  labs(x = NULL, 
       y = "Number of extinct plants\n",
       fill = "Proportion of species with any action taken") +
  theme(strip.text.x = element_text(hjust = 0, size = 14, family = "Cambria"),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "grey70"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "grey90"),
        legend.position = "bottom")

ggsave("tidy-tuesday-w34.png", plot = last_plot(), device = "png", width = 10, height = 8, dpi = 200)
