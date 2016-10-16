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
indexes_tide <- list('loc_id', 'date')

# predict water height for a time interval at a specified resolution
predict_height <- function(dr, c_model, t_res = t_resolution){
  dplyr::data_frame(
    time = seq(dr[1], dr[2], by = paste(t_res, "min")),
    height = predict(c_model, dr[1], dr[2], 
                     by =1/(60/t_res))) %>%
    dplyr::mutate(height = height - c_model$features1[1], 
                  time = as.POSIXct(as.POSIXlt(time, tz = "Indian/Mahe")))
}

t_res <- as.integer(commandArgs(trailingOnly = T)[1])  # resolution for predictions

# to make it more manegeable split it by month
time_frame <- data.frame(ds = seq(as.POSIXct("1978-01-01 00:00:00", tz = "Indian/Mahe"),
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
      }, .parallel = T) %>% 
    	dplyr::select(-month) %>%
    	dplyr::mutate(date = as.integer(as.numeric(time)), 
    								year = as.integer(lubridate::year(time)),
    								loc_id = as.integer(x)) 
    if (length(dbListTables(tide_pred$con))== 0) {
    	copy_to(tide_pred, 
    					df = o %>%
    						dplyr::filter((lubridate::minute(time) %% 10) == 0) %>% 
    						dplyr::select(-time), name = 'tide_prediction', temporary = FALSE,
    					indexes = indexes_tide)
    	copy_to(tide_pred, 
    					df = get_high_low_table(o) %>% dplyr::select(-time), name = 'tide_high_low', temporary = FALSE,
    					indexes = indexes_tide)
    } else {
    	tide_pred <- src_sqlite('../data/processed/tide_predictions.sqlite3', create = T)
    	dbWriteTable(tide_pred$con, 'tide_prediction', o %>%
    							 	dplyr::filter((lubridate::minute(time) %% 10) == 0) %>% 
    							 	dplyr::select(-time), append = TRUE, row.names = F)
    	dbWriteTable(tide_pred$con, 'tide_high_low', get_high_low_table(o) %>% dplyr::select(-time), append = TRUE, row.names = F)
    }
  })
}
  
dbGetQuery(tide_pred$con, 'ANALYZE')
dbDisconnect(tide_pred$con)
