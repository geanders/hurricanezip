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

## Note: It looks like the `tigris` data on ZCTAs is better than the
# dataset in the "US" folder. Right now, we're only using the "US"
# data to get the prefixes for our study states, so we save time by
# not pulling shape files for other states. However, if we could get the
# right ZIP code prefixes for our states in a simpler dataset, it might
# make sense to make that transition to use a simpler dataset and get
# rid of the "US" file in the raw data folder.

# Read in shapefiles for timezones in this part of the country
tz_sf <- st_read("data_raw/timezones.shapefile/") %>%
  filter(tzid %in% c("America/Chicago", "America/Denver",
                     "America/Detroit", "America/Indiana/Indianapolis",
                     "America/Indiana/Knox", "America/Indiana/Marengo",
                     "America/Monterrey",
                     "America/Indiana/Petersburg", "America/Indiana/Tell_City",
                     "America/Indiana/Vevay", "America/Indiana/Vincennes",
                     "America/Indiana/Winamac", "America/Kentucky/Louisville",
                     "America/Kentucky/Monticello", "America/Menominee",
                     "America/New_York")) %>%
  st_transform(., "+proj=longlat +ellps=WGS84 +no_defs")


########################################################################

make_year_zcta_centers <- function(zcta_year){
  # Get all ZIP codes that start with these codes as of the year
  # Other than 2012, there's a lower-resolution version that's faster to process.
  if(zcta_year == 2012){
    study_zips <- zctas(starts_with = zc_starts, year = zcta_year)
  } else {
    study_zips <- zctas(cb = TRUE, starts_with = zc_starts, year = zcta_year)
  }

  # Change to a projection to calculate centroids then change back to lat-long
  study_zip_centroids <- study_zips %>%
    st_transform(crs = 2163) %>%
    st_centroid() %>%
    st_transform(., "+proj=longlat +ellps=WGS84 +no_defs")

  # Convert to a dataframe of lat-long values for each ZIP code
  if(zcta_year == 2000){
    study_zips_df_year <- st_join(study_zip_centroids, tz_sf, join = st_within) %>%
      rename(postal_code = ZCTA) %>%
      select(postal_code, tzid, geometry) %>%
      as_tibble() %>%
      mutate(long = unlist(map(.$geometry,1)),
             lat = unlist(map(.$geometry,2))) %>%
      select(-geometry) %>%
      filter(!str_detect(postal_code, "[A-Z]"))
    # From the US Census' info on ZCTAs: "For the Census 2000 ZCTAs the Census Bureau
    # created ZCTAs that ended in "XX" to represent large areas of land without
    # ZIP Codes or "HH" to represent large areas of water without ZIP Codes. For the
    # 2010 Census, large water bodies and large unpopulated land areas do not have ZCTAs."
  } else {
    study_zips_df_year <- st_join(study_zip_centroids, tz_sf, join = st_within) %>%
      rename(postal_code = ZCTA5CE10) %>%
      select(postal_code, tzid, geometry) %>%
      as_tibble() %>%
      mutate(long = unlist(map(.$geometry,1)),
             lat = unlist(map(.$geometry,2))) %>%
      select(-geometry)

  }

  return(study_zips_df_year)
}

# Sounds like there are no ZCTAs before 2000

study_zips_df_2000 <- make_year_zcta_centers(zcta_year = 2000)
use_data(study_zips_df_2000, overwrite = TRUE)
write_csv(study_zips_df_2000,
          file = "for_james/zip_code_centers_2000.csv")

# Several additional years here

study_zips_df_2010 <- make_year_zcta_centers(zcta_year = 2010)
use_data(study_zips_df_2010, overwrite = TRUE)
write_csv(study_zips_df_2010,
          file = "for_james/zip_code_centers_2010.csv")

# 2011

study_zips_df_2012 <- make_year_zcta_centers(zcta_year = 2012)
use_data(study_zips_df_2012, overwrite = TRUE)
write_csv(study_zips_df_2012,
          file = "for_james/zip_code_centers_2012.csv")

study_zips_df_2013 <- make_year_zcta_centers(zcta_year = 2013)
use_data(study_zips_df_2013, overwrite = TRUE)
write_csv(study_zips_df_2013,
          file = "for_james/zip_code_centers_2013.csv")

study_zips_df_2014 <- make_year_zcta_centers(zcta_year = 2014)
use_data(study_zips_df_2014, overwrite = TRUE)
write_csv(study_zips_df_2014,
          file = "for_james/zip_code_centers_2014.csv")

study_zips_df_2015 <- make_year_zcta_centers(zcta_year = 2015)
use_data(study_zips_df_2015, overwrite = TRUE)
write_csv(study_zips_df_2015,
          file = "for_james/zip_code_centers_2015.csv")

study_zips_df_2016 <- make_year_zcta_centers(zcta_year = 2016)
use_data(study_zips_df_2016, overwrite = TRUE)
write_csv(study_zips_df_2016,
          file = "for_james/zip_code_centers_2016.csv")

study_zips_df_2017 <- make_year_zcta_centers(zcta_year = 2017)
use_data(study_zips_df_2017, overwrite = TRUE)
write_csv(study_zips_df_2017,
          file = "for_james/zip_code_centers_2017.csv")

study_zips_df_2018 <- make_year_zcta_centers(zcta_year = 2018)
use_data(study_zips_df_2018, overwrite = TRUE)
write_csv(study_zips_df_2018,
          file = "for_james/zip_code_centers_2018.csv")

study_zips_df_2019 <- make_year_zcta_centers(zcta_year = 2019)
use_data(study_zips_df_2019, overwrite = TRUE)
write_csv(study_zips_df_2019,
          file = "for_james/zip_code_centers_2019.csv")


