library(magrittr)
library(foreach)
library(parallel)
library(doMC)
library(TideHarmonics)
library(RSQLite)
library(dplyr)
registerDoMC(cores = 4)

source("../dashboard_functions.R")
load(file ="../data/processed/tide_models.rds")

file.remove('../data/processed/tide_predictions.sqlite3')
tide_pred <- src_sqlite('../data/processed/tide_predictions.sqlite3', create = T)
# tide_pred <- dbConnect("SQLite", dbname='./data/processed/tide_predictions.sqlite3')
indexes_tide <- list(c('year', 'month', 'day'), 'loc_id', 'date')

# predict water height for a time interval at a specified resolution
predict_height <- function(dr, c_model, t_res = t_resolution){
  dplyr::data_frame(
    time = seq(dr[1], dr[2], by = paste(t_res, "min")),
    height = predict(c_model, dr[1], dr[2], 
                     by =1/(60/t_res))) %>%
    dplyr::mutate(height = height - c_model$features1[1], 
                  time = as.POSIXct(as.POSIXlt(time, tz = "Indian/Mahe")))
}

t_res <- 1  # resolution for predictions

# to make it more manegeable split it by month
time_frame <- data.frame(ds = seq(as.POSIXct("1979-01-01 00:00:00", tz = "Indian/Mahe"),
                    as.POSIXct("2020-01-01 00:00:00", tz = "Indian/Mahe"), 
                    "month")) %>% 
  dplyr::mutate(de = dplyr::lead(ds),
                year =lubridate::year(ds),
                month = lubridate::month(ds)) %>%
  dplyr::filter(!is.na(de))

for(x in length(tide_models)){
  plyr::d_ply(time_frame, "year", function(y){
  	message('location ', x, ' - ', y$year[1])
    o <- y %>%
      plyr::ddply("month", function(z){
        h <- predict_height(c(z$ds, z$de), tide_models[[x]], t_res)
        h[1:(nrow(h)-1), ]
      }, .progress = 'text') %>% 
    	dplyr::select(-month) %>%
    	dplyr::mutate(year = as.integer(lubridate::year(time)),
    								month = as.integer(lubridate::month(time)),
    								day = as.integer(lubridate::day(time)),
    								date = as.integer(as.numeric(time)), 
    								time_n = as.integer(as.numeric(difftime(time, trunc(time, 'day'), units = 'sec'))),
    								loc_id = as.integer(x)) %>% 
    	dplyr::select(-time)
    if (length(dbListTables(tide_pred$con))== 0) {
    	copy_to(tide_pred, 
    					df = o, name = 'tide_prediction', temporary = FALSE,
    					indexes = indexes_tide)
    	copy_to(tide_pred, 
    					df = get_high_low_table(o), name = 'tide_high_low', temporary = FALSE,
    					indexes = indexes_tide)
    } else {
    	tide_pred <- src_sqlite('../data/processed/tide_predictions.sqlite3', create = T)
    	dbWriteTable(tide_pred$con, 'tide_prediction', o, append = TRUE, row.names = F)
    	dbWriteTable(tide_pred$con, 'tide_high_low', get_high_low_table(o), append = TRUE, row.names = F)
    }
  })
}
  
