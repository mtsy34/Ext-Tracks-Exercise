library(readr)
library(dplyr)
library(tidyr)

ext_tracks_file <- "http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt"

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day", "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind", "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"), paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"), "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf(ext_tracks_file,fwf_widths(ext_tracks_widths, ext_tracks_colnames),na = "-99")


ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
ext_tracks <- read_fwf(ext_tracks_file,  fwf_widths(ext_tracks_widths, ext_tracks_colnames), na = "-99")

write.csv(ext_tracks, "./data/data.csv")

# Piping/Without piping
# Without piping

katrina <- filter(ext_tracks, storm_name=="KATRINA")
katrina_reduced <- select(katrina, month, day, hour, max_wind)
head(katrina_reduced, 3)

# With piping

ext_tracks %>%
  filter(storm_name=="KATRINA") %>%
  select(month, day, hour, max_wind) %>%
  head(3)

# Summarizing data

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

# Writing a function to perform conversion 

knots_to_mph <- function(knots){
  mph <- 1.152 * knots 
}

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = knots_to_mph(max(max_wind)),
            worst_pressure = min(min_pressure))

# Grouping data by one of its column variable

ext_tracks %>%
  group_by(storm_name, year) %>%
  head()

ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

# Plotting histogram of grouped data

library(ggplot2)
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram()

# Selecting columns with select

ext_tracks %>%
  select(storm_name, month, day, hour, year, latitude, longitude, max_wind)

ext_tracks %>%
  select(storm_name, latitude, longitude, starts_with("radius_34"))

ext_tracks %>%
  select(storm_name, latitude, longitude, ends_with("ne"))

ext_tracks %>%
  select(storm_name, latitude, longitude, contains("34"))

ext_tracks %>%
  select(storm_name, latitude, longitude, matches("_[0-9][0-9]_"))

# Filtering rows with filter

ext_tracks %>%
  select(storm_name, hour, max_wind) %>%
  filter(hour == "00") %>%
  head(9)

ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 160)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == "ANDREW" & max_wind >= 137)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name %in% c("ANDREW", "KATRINA") & max_wind >= 137)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind, contains("ne")) %>%
  filter(is.na(radius_34_ne))

# Working with dates, times, time zones
install.packages("tidyverse")
library(tidyverse)
library(lubridate) 

ymd("2006-03-12")
ymd("'06 March 12")
ymd_hm("06/3/12 6:30PM")
