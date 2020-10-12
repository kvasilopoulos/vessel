## code to prepare `ships` dataset goes here

library(geosphere)

zip_url <- "https://drive.google.com/file/d/160JnqoQysqzvR1GBBnKJFKAew_v6TYli"
googledrive::drive_download(zip_url, "data-raw/ships_data.zip")

ships <- readr::read_csv(unz("data-raw/ships_data.zip", "ships.csv")) %>% 
  select(lat = LAT, lon = LON, ship_name = SHIPNAME, ship_type, datetime = DATETIME, date, destination = DESTINATION) %>% 
  group_by(date) %>% # unless there are two trips in the same date
  mutate(lat1 = dplyr::lag(lat), lon1 = dplyr::lag(lon)) %>% 
  mutate(distance = distHaversine(cbind(lon, lat), cbind(lon1, lat1))) %>% 
  ungroup()

usethis::use_data(ships, overwrite = TRUE)
