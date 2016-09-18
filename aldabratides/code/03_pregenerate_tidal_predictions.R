library(magrittr)
library(foreach)
library(parallel)
library(doMC)
library(TideHarmonics)
registerDoMC(cores = 4)

source("../dashboard_functions.R")
load(file ="../data/processed/tide_models.rds")

# predict water height for a time interval at a specified resolution
predict_height <- function(dr, c_model, t_res = t_resolution){
  dplyr::data_frame(
    time = seq(dr[1], dr[2], by = paste(t_res, "min")),
    height = predict(c_model, dr[1], dr[2], 
                     by =1/(60/t_res))) %>%
    dplyr::mutate(height = height - c_model$features1[1], 
                  time = as.POSIXct(as.POSIXlt(time, tz = "Indian/Mahe")))
}

t_res <- 10  # resolution for predictions

# to make it more manegeable split it by month
time_frame <- data.frame(ds = seq(as.POSIXct("2016-01-01 00:00:00", tz = "Indian/Mahe"),
                    as.POSIXct("2018-01-01 00:00:00", tz = "Indian/Mahe"), 
                    "month")) %>% 
  dplyr::mutate(de = dplyr::lead(ds),
                year =lubridate::year(ds),
                month = lubridate::month(ds)) %>%
  dplyr::filter(!is.na(de))

dir.create(paste0("../data/processed/tide_pred"))
for(x in length(tide_models)){
  plyr::d_ply(time_frame, "year", function(y){
    dir.create(paste0("../data/processed/tide_pred/loc_", x))
    y %>%
      plyr::ddply("month", function(z){
        h <- predict_height(c(z$ds, z$de), tide_models[[x]], t_res)
        h[1:(nrow(h)-1), ]
      }, .parallel = T) %>%
      saveRDS(file = paste0("../data/processed/tide_pred/loc_", x, "/", y$year[1], ".rds"))
  }, .progress = "text")
}
  
