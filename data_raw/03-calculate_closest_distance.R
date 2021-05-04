# This code was developed starting from similar code in our `hurricaneexposuredata`
# package.

library(sp)
library(dplyr)
library(lubridate)
library(hurricaneexposure)
library(usethis)

data(study_zips_df_2018, package = "hurricanezip")
data(study_zips_df_2017, package = "hurricanezip")
data(hurr_tracks, package = "hurricanezip")

library(stormwindmodel)
library(tidyr)
library(purrr)
library(stringr)
library(tidyverse)

## Interpolate storm tracks to every 15 minutes
all_tracks <- hurr_tracks %>%
  mutate(date_time = ymd_hm(date)) %>%
  group_by(storm_id, usa_atcf_id) %>%
  mutate(start_time = first(date_time)) %>%
  mutate(track_time = difftime(date_time, first(date_time), units = "hours"),
         track_time_simple = as.numeric(track_time)) %>%
  ungroup() %>%
  group_by(storm_id, usa_atcf_id, start_time) %>%
  nest() %>%
  mutate(interp_time = purrr::map(data, ~ seq(from = first(.x$track_time_simple),
                                              to = last(.x$track_time_simple),
                                              by = 0.25))) %>%
  # Interpolate latitude and longitude using natural cubic splines
  mutate(interp_lat = map2(data, interp_time,
                           ~ spline(x = .x$track_time_simple,
                                    y = .x$latitude,
                                    xout = .y,
                                    method = "natural")$y)) %>%
  mutate(interp_lon = map2(data, interp_time,
                           ~ spline(x = .x$track_time_simple,
                                    y = .x$longitude,
                                    xout = .y,
                                    method = "natural")$y)) %>%
  select(-data) %>%
  unnest(interp_time:interp_lon) %>%
  ungroup() %>%
  mutate(date = start_time + minutes(60 * interp_time)) %>%
  select(storm_id:usa_atcf_id, date, interp_lat:interp_lon) %>%
  rename(tclon = interp_lon,
         tclat = interp_lat)

calc_closest_dist <- function(this_storm = "Florence-2018",
                              study_zips = study_zips_df_2018){
  print(this_storm)
  storm_tracks <- subset(all_tracks, storm_id == this_storm)
  this_id <- storm_tracks$usa_atcf_id[1]

  # Calculate distance from county center to storm path
  storm_zip_distances <- spDists(
    as.matrix(study_zips[,c("long", "lat")]),
    as.matrix(storm_tracks[,c("tclon", "tclat")]),
    longlat = TRUE) # Return distance in kilometers

  min_locs <- apply(storm_zip_distances, 1, which.min)
  min_dists <- apply(storm_zip_distances, 1, min)

  closest_dist <- mutate(study_zips,
                         closest_date = storm_tracks$date[min_locs],
                         storm_lat = storm_tracks$tclat[min_locs],
                         storm_long = storm_tracks$tclon[min_locs],
                         storm_dist = min_dists) %>%
    mutate(closest_date = format(closest_date, "%Y%m%d%H%M"),
           storm_id = this_storm, usa_atcf_id = this_id) %>%
    select(storm_id, usa_atcf_id, postal_code, closest_date, storm_dist)

  return(closest_dist)
}

## Next step: add local time based on time zone

# Apply to all hurricane tracks
hurrs <- as.character(unique(hurr_tracks$storm_id))
hurrs_2019 <- str_subset(hurrs, "2019") # Later, expand to all years
hurrs_2018 <- str_subset(hurrs, "2018") # Later, expand to all years
hurrs_2017 <- str_subset(hurrs, "2017") # Later, expand to all years
hurrs_2016 <- str_subset(hurrs, "2016") # Later, expand to all years

closest_dist_2019 <- lapply(hurrs_2019, calc_closest_dist,
                            study_zips = study_zips_df_2019)
closest_dist_2019 <- do.call("rbind", closest_dist_2019)
use_data(closest_dist_2019, overwrite = TRUE)

closest_dist_2018 <- lapply(hurrs_2018, calc_closest_dist,
                            study_zips = study_zips_df_2018)
closest_dist_2018 <- do.call("rbind", closest_dist_2018)
use_data(closest_dist_2018, overwrite = TRUE)

closest_dist_2017 <- lapply(hurrs_2017, calc_closest_dist,
                            study_zips = study_zips_df_2017)
closest_dist_2017 <- do.call("rbind", closest_dist_2017)
use_data(closest_dist_2017, overwrite = TRUE)

closest_dist_2016 <- lapply(hurrs_2016, calc_closest_dist,
                            study_zips = study_zips_df_2016)
closest_dist_2016 <- do.call("rbind", closest_dist_2016)
use_data(closest_dist_2016, overwrite = TRUE)


##########################################################################
## Save some files for James to use as input

all_tracks_2018 <- all_tracks %>%
  filter(year(date) == 2018)

write_csv(all_tracks_2018, "for_james/interpolated_tracks_2018.csv")
write_csv(closest_dist_2018, "for_james/closest_distance_2018.csv")

all_tracks_2017 <- all_tracks %>%
  filter(year(date) == 2017)

write_csv(all_tracks_2017, "for_james/interpolated_tracks_2017.csv")
write_csv(closest_dist_2017, "for_james/closest_distance_2017.csv")
