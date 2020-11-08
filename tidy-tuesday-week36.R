library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(tidytuesdayR)
library(stringr)

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

land <- tuesdata$arable_land_pin
yield_fert <- tuesdata$cereal_crop_yield_vs_fertilizer_application
yield_trac <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
yield_key <- tuesdata$key_crop_yields
yield_change <- tuesdata$land_use_vs_yield_change_in_cereal_production

entities <- distinct(yield_fert, Entity)

yield_cont <- yield_fert %>%
  filter(Entity %in% c("Asia",
                       "Europe",
                       "Oceania",
                       "Africa",
                       "Northern America",
                       "South America"
                       #"Americas"
                       )
         ) %>%
  mutate(yield = `Cereal yield (tonnes per hectare)`,
         fert = `Nitrogen fertilizer use (kilograms per hectare)`) %>%
  select(Entity, Year, yield) %>%
  group_by(Entity) %>%
  mutate(yield_lag = lag(yield),
         percent_change = ((yield/yield_lag)-1)*100,
         sign = sign(percent_change),
         Entity = factor(Entity, levels = c("Asia", "Europe", "Africa", "Northern America", "South America", "Oceania")
                         )
        )

ggplot(data = yield_cont) +
  geom_line(aes(x = Year, y = yield, colour = yield)) +
  facet_wrap(~Entity, ncol = 1) +
  theme_minimal()

p1 <- ggplot(data = yield_cont) +
        geom_col(aes(x = Year, y = percent_change, fill = as.character(sign))) +
        scale_y_continuous(breaks = c(-50, 0, 50)) +
        facet_wrap(~Entity, ncol = 1) +
        labs(x = "Year",
             y = "Year-over-year % change",
             title = "THe Growth in Growing",
             subtitle = "Year-over-year change in cereal yields (tonnes/hectare) \nby continent") +
        theme(legend.position = 0,
              panel.grid = element_blank(),
              panel.background = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_text(hjust = 0, size = 14, family = "Tahoma"),
              plot.title = element_text(family = "Tahoma"),
              plot.subtitle = element_text(size = 9, family = "Tahoma")
              #axis.title.y = element_blank()
              )

annotations <- tribble(~Entity, ~label, ~change,
                       "Asia", "Net change (from 1961):", "+349%",
                       "Europe", "", "+308%",
                       "Africa", "", "+200%",
                       "Northern America", "", "+347%",
                       "South America", "", "+345%",
                       "Oceania", "", "+187%"
                       ) %>%
  mutate(Entity = factor(Entity, levels = c("Asia", "Europe", "Africa", "Northern America", "South America", "Oceania")))

p1 + geom_text(data = annotations,
               aes(x = 2008.5, y = 60, label = label),
               hjust = 1) +
  geom_label(data = annotations,
             aes(x = 2020, y = 60, label = change),
             fill = "green",
             hjust = 1)

ggsave("tidy-tuesday-w36.png", plot = last_plot(), device = "png", width = 4, height = 10, dpi = 200)
