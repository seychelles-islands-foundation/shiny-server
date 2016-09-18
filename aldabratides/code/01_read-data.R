library(magrittr)

# read data
tides <- list.files("../data/raw/pressure_data/", full.names = T) %>% 
  lapply(read.csv)
# make sensible column names
tides <- lapply(tides, `names<-`, c("date", "time", "pressure"))

clean_data <- . %>%
  # get a proper date
  dplyr::filter(!is.na(pressure)) %>%
  dplyr::mutate(date = stringr::str_extract(as.character(date), "[^ ]*"),
                date = paste(date, as.character(time)),
                date = as.POSIXct(date, format = "%d/%m/%y %I:%M:%S %p",
                                  tz = "Indian/Mahe")) %>%
  # remove first and day altogether
  dplyr::filter(as.Date(date, tz = "Indian/Mahe") != 
                  min(as.Date(date, tz = "Indian/Mahe")),
                as.Date(date, tz = "Indian/Mahe") != 
                  max(as.Date(date, tz = "Indian/Mahe"))) %>%
  # convert time to UTC
  dplyr::mutate(dateUTC = as.POSIXct(as.POSIXlt(date, tz = "UTC")))

depth_from_pressure <- . %>%
  dplyr::mutate(pressure = marelac::convert_p(pressure * 1000, unit = "Pa")$bar, 
                depth = marelac::sw_depth(P = pressure, lat = -9.4))

tides <- lapply(tides, clean_data)
tides <- lapply(tides, depth_from_pressure)

dir.create('../data/processed')
saveRDS(tides, "../data/processed/depth_time-series.rds")
