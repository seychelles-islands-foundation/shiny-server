args <- commandArgs(trailingOnly = TRUE)

folder <- dirname(args[1])
out_file <- args[2]

library(magrittr)
library(dplyr)

message("Looking for files in ", folder)
message("To write the file ", out_file)

table_names <- list.files(folder, ".csv") %>% stringr::str_sub(start = 4, end = -5)

db <- list.files(folder, ".csv", full.names = T) %>%
  lapply(readr::read_csv) %>% 
  `names<-`(table_names)


for(i in 1:length(db)){
  fixed_names <- 
    stringr::str_replace(string = names(db[[i]]), 
                         pattern = "^ID$", 
                         replacement = paste(names(db)[i], "ID", sep = "_"))
  names(db[[i]]) <- fixed_names
}

message(names(db$FoodSecurityZone))

db$FoodSecurityZone %<>%
  mutate(FSZ_Name = sub("\\..*$", "", FSZ_Name))

db$Species %<>%
  mutate(sci_name_short = paste0(
    stringr::str_sub(Scientific_names, 1, 1), ". ", 
    sub("^[A-Za-z]+", "", Scientific_names)
  ))


db$Trip %<>%
  mutate(TripDate = as.POSIXct(TripDate, 
                               format = "%d/%m/%y %H:%M",
                               tz = "Indian/Mahe"))

db$Session %<>%
  mutate_at(vars(Start_Session, End_Session),
            funs(as.POSIXct), 
            format = "%d/%m/%Y %H:%M:%S",
            tz = "Indian/Mahe") %>%
  mutate(session_duration = as.numeric(difftime(End_Session, Start_Session, units = "hours")))

trip <- db$Trip %>% 
  inner_join(db$Boat) %>%
  inner_join(db$Session) %>% 
  inner_join(db$FishingMethod) %>% 
  group_by(Trip_ID, Fishing_method) %>%
  mutate(Trip_ID_effort = sum(session_duration)) %>%
  group_by() %>%
  mutate(month = lubridate::floor_date(TripDate, "month"), 
         year = lubridate::floor_date(TripDate, "year"))

month_effort <- trip %>%
  group_by(month, Fishing_method) %>%
  summarise(month_effort = sum(session_duration, na.rm = T))

year_effort <- trip %>%
  group_by(year, Fishing_method) %>%
  summarise(year_effort = sum(session_duration, na.rm = T))

trip %<>% 
  inner_join(month_effort) %>% 
  inner_join(year_effort) %>%
  inner_join(db$FoodSecurityZone) %>%
  inner_join(db$FishCaught) %>% # Catch ID? Date Caught
  inner_join(db$Species) %>%
  inner_join(db$FishUse) %>% 
  mutate(CalcWeight = Parameter_a * (Length ^ Parameter_b))

saveRDS(trip, out_file)

parameters <- list()
parameters$min_date <- min(trip$TripDate)
parameters$max_date <- max(trip$TripDate)

saveRDS(parameters, file.path(dirname(out_file), "parameters.rds"))
