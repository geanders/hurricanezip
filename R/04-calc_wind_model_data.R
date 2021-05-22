library(tidyverse)
library(stormwindmodel)
data(hurr_tracks)

## 2016 data
data(study_zips_df_2016)

hurr_tracks_2016 <- hurr_tracks %>%
  filter(str_sub(date, 1, 4) == "2016")
storms_2016 <- unique(hurr_tracks_2016$usa_atcf_id)
storm_id_table_2016 <- hurr_tracks_2016 %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

model_zips <- study_zips_df_2016 %>%
  rename(gridid = postal_code,
         glon = long,
         glat = lat) %>%
  select(gridid, glat, glon)

storm_winds_2016 <- vector("list",
                           length = length(storms_2016))

Sys.time()
for(i in 1:length(storm_winds_2016)){
  print(Sys.time())
  print(storms_2016[i])
  storm_track <- subset(hurr_tracks_2016, usa_atcf_id == storms_2016[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = model_zips) %>%
    dplyr::select(-glat, -glon) %>%
    dplyr::rename(postal_code = gridid) %>%
    dplyr::mutate(usa_atcf_id = storms_2016[i],
                  storm_id = storm_id_table_2016$storm_id[storm_id_table_2016$usa_atcf_id == storms_2016[i]])
  storm_winds_2016[[i]] <- winds
  print(Sys.time())
}
Sys.time()

storm_winds_2016 <- do.call("rbind", storm_winds_2016)
usethis::use_data(storm_winds_2016, overwrite = TRUE)
write_csv(storm_winds_2016, file = "for_james/storm_winds_2016.csv")

## 2017 data
data(study_zips_df_2017)

hurr_tracks_2017 <- hurr_tracks %>%
  filter(str_sub(date, 1, 4) == "2017")
storms_2017 <- unique(hurr_tracks_2017$usa_atcf_id)
storm_id_table_2017 <- hurr_tracks_2017 %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

model_zips <- study_zips_df_2017 %>%
  rename(gridid = postal_code,
         glon = long,
         glat = lat) %>%
  select(gridid, glat, glon)

storm_winds_2017 <- vector("list",
                           length = length(storms_2017))

Sys.time()
for(i in 1:length(storm_winds_2017)){
  print(Sys.time())
  print(storms_2017[i])
  storm_track <- subset(hurr_tracks_2017, usa_atcf_id == storms_2017[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = model_zips) %>%
    dplyr::select(-glat, -glon) %>%
    dplyr::rename(postal_code = gridid) %>%
    dplyr::mutate(usa_atcf_id = storms_2017[i],
                  storm_id = storm_id_table_2017$storm_id[storm_id_table_2017$usa_atcf_id == storms_2017[i]])
  storm_winds_2017[[i]] <- winds
  print(Sys.time())
}
Sys.time()

storm_winds_2017 <- do.call("rbind", storm_winds_2017)
usethis::use_data(storm_winds_2017, overwrite = TRUE)
write_csv(storm_winds_2017, file = "for_james/storm_winds_2017.csv")

## 2018 data
data(study_zips_df_2018)

hurr_tracks_2018 <- hurr_tracks %>%
  filter(str_sub(date, 1, 4) == "2018")
storms_2018 <- unique(hurr_tracks_2018$usa_atcf_id)
storm_id_table_2018 <- hurr_tracks_2018 %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

model_zips <- study_zips_df_2018 %>%
  rename(gridid = postal_code,
         glon = long,
         glat = lat) %>%
  select(gridid, glat, glon)

storm_winds_2018 <- vector("list",
                      length = length(storms_2018))

Sys.time()
for(i in 1:length(storm_winds_2018)){
  print(Sys.time())
  print(storms_2018[i])
  storm_track <- subset(hurr_tracks_2018, usa_atcf_id == storms_2018[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = model_zips) %>%
    dplyr::select(-glat, -glon) %>%
    dplyr::rename(postal_code = gridid) %>%
    dplyr::mutate(usa_atcf_id = storms_2018[i],
                  storm_id = storm_id_table_2018$storm_id[storm_id_table_2018$usa_atcf_id == storms_2018[i]])
  storm_winds_2018[[i]] <- winds
  print(Sys.time())
}
Sys.time()

storm_winds_2018 <- do.call("rbind", storm_winds_2018)
usethis::use_data(storm_winds_2018, overwrite = TRUE)
write_csv(storm_winds_2018, file = "for_james/storm_winds_2018.csv")

## 2019 data
data(study_zips_df_2019)

hurr_tracks_2019 <- hurr_tracks %>%
  filter(str_sub(date, 1, 4) == "2019")
storms_2019 <- unique(hurr_tracks_2019$usa_atcf_id)
storm_id_table_2019 <- hurr_tracks_2019 %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

model_zips <- study_zips_df_2019 %>%
  rename(gridid = postal_code,
         glon = long,
         glat = lat) %>%
  select(gridid, glat, glon)

storm_winds_2019 <- vector("list",
                           length = length(storms_2019))

Sys.time()
for(i in 1:length(storm_winds_2019)){
  print(Sys.time())
  print(storms_2019[i])
  storm_track <- subset(hurr_tracks_2019, usa_atcf_id == storms_2019[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = model_zips) %>%
    dplyr::select(-glat, -glon) %>%
    dplyr::rename(postal_code = gridid) %>%
    dplyr::mutate(usa_atcf_id = storms_2019[i],
                  storm_id = storm_id_table_2019$storm_id[storm_id_table_2019$usa_atcf_id == storms_2019[i]])
  storm_winds_2019[[i]] <- winds
  print(Sys.time())
}
Sys.time()

storm_winds_2019 <- do.call("rbind", storm_winds_2019)
usethis::use_data(storm_winds_2019, overwrite = TRUE)
write_csv(storm_winds_2019, file = "for_james/storm_winds_2019.csv")


## Check with mapping
library(tigris)
library(viridis)
nc_zips_sf <- zctas(cb = TRUE, starts_with = c("27", "28"), year = 2018) %>%
  left_join(filter(storm_winds_2018, storm_id == "Florence-2018"),
            by = c("ZCTA5CE10" = "postal_code"))

ggplot() +
  geom_sf(data = nc_zips_sf, aes(fill = vmax_sust, color = vmax_sust)) +
  scale_fill_viridis() +
  scale_color_viridis()
