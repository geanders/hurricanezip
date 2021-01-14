# This code was developed starting from similar code in our `hurricaneexposuredata`
# package.

library(sp)
library(dplyr)
library(lubridate)
library(hurricaneexposure)

data(study_zips_df_2018, package = "hurricanezip")
data(hurr_tracks, package = "hurricanezip")

library(stormwindmodel)
library(tidyr)
library(purrr)
library(stringr)


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

calc_closest_dist <- function(this_storm = "Florence-2018"){
  print(this_storm)
  storm_tracks <- subset(all_tracks, storm_id == this_storm)
  this_id <- storm_tracks$usa_atcf_id[1]

  # Calculate distance from county center to storm path
  storm_zip_distances <- spDists(
    as.matrix(study_zips_df_2018[,c("long", "lat")]),
    as.matrix(storm_tracks[,c("tclon", "tclat")]),
    longlat = TRUE) # Return distance in kilometers

  min_locs <- apply(storm_zip_distances, 1, which.min)
  min_dists <- apply(storm_zip_distances, 1, min)

  closest_dist <- mutate(study_zips_df_2018,
                         closest_date = storm_tracks$date[min_locs],
                         storm_lat = storm_tracks$tclat[min_locs],
                         storm_long = storm_tracks$tclon[min_locs],
                         storm_dist = min_dists) %>%
    mutate(closest_date = format(closest_date, "%Y%m%d%H%M"),
           storm_id = this_storm, usa_atcf_id = this_id) %>%
    select(storm_id, usa_atcf_id, postal_code, closest_date, storm_dist)

  return(closest_dist)
}

# Apply to all hurricane tracks
hurrs <- as.character(unique(hurr_tracks$storm_id))
hurrs_2018 <- str_subset(hurrs, "2018") # Later, expand to all years

closest_dist <- lapply(hurrs_2018, calc_closest_dist)
closest_dist <- do.call("rbind", closest_dist)
