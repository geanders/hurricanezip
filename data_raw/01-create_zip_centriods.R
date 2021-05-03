library(tigris)
library(tidyverse)
library(sf)
library(usethis)

study_states <- c("Alabama", "Arkansas", "Connecticut", "Delaware",
                  "District of Columbia", "Florida", "Georgia",
                  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                  "Louisiana", "Maine", "Maryland", "Massachusetts",
                  "Michigan", "Mississippi", "Missouri",
                  "New Hampshire", "New Jersey", "New York", "North Carolina",
                  "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                  "South Carolina", "Tennessee", "Texas", "Vermont",
                  "Virginia", "West Virginia", "Wisconsin")

# This dataset only used to check data from `tigris` for ZIP code
# centroids. The `tigris` data (ZCTAs) will be used as the primary
# values.
zc_centers <- read_delim("data_raw/US/US.txt", delim = "\t",
                         col_names = c("country_code", "postal_code",
                                       "place_name", "state_name",
                                       "state_abbr", "county",
                                       "county_code", "X8", "X9",
                                       "latitude", "longitude",
                                       "accuracy")) %>%
  filter(state_name %in% study_states) %>%
  select(c(-X8, -X9)) %>%
  mutate(postal_code = str_pad(postal_code, width = 5, side = "left", pad = "0"),
         zip_start = str_sub(postal_code, 1, 3))

# Get all three-digit codes for study states
zc_starts <- zc_centers %>%
  pull(zip_start) %>%
  unique()

########################################################################

# Get all ZIP codes that start with these codes as of 2018
study_zips <- zctas(cb = TRUE, starts_with = zc_starts, year = 2018)

# Change to a projection to calculate centroids then change back to lat-long
study_zip_centroids <- study_zips %>%
  st_transform(crs = 2163) %>%
  st_centroid() %>%
  st_transform(., "+proj=longlat +ellps=WGS84 +no_defs")

# Convert to a dataframe of lat-long values for each ZIP code
study_zips_df_2018 <- study_zip_centroids %>%
  rename(postal_code = ZCTA5CE10) %>%
  select(postal_code, geometry) %>%
  as_tibble() %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  select(-geometry)

# Read in timezone shapefiles
# Time zone shapefiles are from
tz_sf <- st_read("data_raw/timezones.shapefile/combined-shapefile.shp") %>%
  filter(tzid %in% c("America/Chicago",
                     "America/Detroit",
                     "America/Indiana/Indianapolis",
                     "America/Indiana/Knox",
                     "America/Indiana/Marengo",
                     "America/Indiana/Petersburg",
                     "America/Indiana/Tell_City",
                     "America/Indiana/Vevay",
                     "America/Indiana/Vincennes",
                     "America/Indiana/Winamac",
                     "America/Kentucky/Louisville",
                     "America/Kentucky/Monticello",
                     "America/New_York"))

# Simplify the resolution of the borders to make object size
# a bit smaller
tz_sf_2 <- tz_sf %>%
  st_transform(2163) %>%
  st_simplify(dTolerance = 1000)

# Next step: determine the time zone for each ZIP based on centroid

use_data(study_zips_df_2018, overwrite = TRUE)
write_csv(study_zips_df_2018,
          file = "for_james/zip_code_centers_2018.csv")

########################################################################

# Get all ZIP codes that start with these codes as of 2017
study_zips <- zctas(cb = TRUE, starts_with = zc_starts, year = 2017)

# Change to a projection to calculate centroids then change back to lat-long
study_zip_centroids <- study_zips %>%
  st_transform(crs = 2163) %>%
  st_centroid() %>%
  st_transform(., "+proj=longlat +ellps=WGS84 +no_defs")

# Convert to a dataframe of lat-long values for each ZIP code
study_zips_df_2017 <- study_zip_centroids %>%
  rename(postal_code = ZCTA5CE10) %>%
  select(postal_code, geometry) %>%
  as_tibble() %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  select(-geometry)

# Read in timezone shapefiles
# Time zone shapefiles are from
tz_sf <- st_read("data_raw/timezones.shapefile/combined-shapefile.shp") %>%
  filter(tzid %in% c("America/Chicago",
                     "America/Detroit",
                     "America/Indiana/Indianapolis",
                     "America/Indiana/Knox",
                     "America/Indiana/Marengo",
                     "America/Indiana/Petersburg",
                     "America/Indiana/Tell_City",
                     "America/Indiana/Vevay",
                     "America/Indiana/Vincennes",
                     "America/Indiana/Winamac",
                     "America/Kentucky/Louisville",
                     "America/Kentucky/Monticello",
                     "America/New_York"))

# Simplify the resolution of the borders to make object size
# a bit smaller
tz_sf_2 <- tz_sf %>%
  st_transform(2163) %>%
  st_simplify(dTolerance = 1000)

# Next step: determine the time zone for each ZIP based on centroid

use_data(study_zips_df_2017, overwrite = TRUE)
write_csv(study_zips_df_2017,
          file = "for_james/zip_code_centers_2017.csv")


##############################################################################

## Code to check data

study_state_sf <- states(cb = TRUE, resolution = "20m", year = 2018) %>%
  filter(NAME %in% study_states)

# Takes a while to run with all the ZIP codes!
# ggplot() +
#  geom_sf(data = study_state_sf, fill = "aliceblue") +
#  geom_sf(data = study_zips, fill = "red", color = "red")

# Check a subset of ZIP codes
ggplot() +
  geom_sf(data = filter(study_zips,
                        str_sub(ZCTA5CE10, 1, 2) == "23"),
          fill = "aliceblue", color = "red", size = 0.2) +
  geom_sf(data = filter(study_zip_centroids,
                        str_sub(ZCTA5CE10, 1, 2) == "23"),
          color = "brown")

# Compare to other resource for ZIP code centroid data
zc_centers %>%
  right_join(study_zips_df, by = "postal_code") %>%
  ggplot(aes(x = longitude, y = long)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

zc_centers %>%
  right_join(study_zips_df, by = "postal_code") %>%
  mutate(long_diff = abs(longitude - long)) %>%
  arrange(desc(long_diff))

# Checking the most different longitude point, it looks like the
# `tigris` data is more correctly (this is our primary data)
ggplot() +
  geom_sf(data = filter(study_zips, ZCTA5CE10 == "79942"),
          fill = "aliceblue", color = "red", size = 0.2) +
  geom_sf(data = filter(study_zip_centroids, ZCTA5CE10 == "79942"),
          color = "brown") +
  geom_sf(data = st_as_sf(zc_centers,
                          coords = c("longitude", "latitude"),
                          crs = "+proj=longlat +ellps=WGS84 +no_defs") %>%
            filter(postal_code == "79942"))


zc_centers %>%
  right_join(study_zips_df, by = "postal_code") %>%
  ggplot(aes(x = latitude, y = lat)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

## Note: It looks like the `tigris` data on ZCTAs is better than the
# dataset in the "US" folder. Right now, we're only using the "US"
# data to get the prefixes for our study states, so we save time by
# not pulling shape files for other states. However, if we could get the
# right ZIP code prefixes for our states in a simpler dataset, it might
# make sense to make that transition to use a simpler dataset and get
# rid of the "US" file in the raw data folder.

