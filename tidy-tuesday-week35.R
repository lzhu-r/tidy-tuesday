library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(tidytuesdayR)
library(stringr)

tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped
