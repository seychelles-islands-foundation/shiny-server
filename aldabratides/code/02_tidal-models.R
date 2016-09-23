library(TideHarmonics)
library(magrittr)

tide <- readRDS("../data/processed/depth_time-series.rds")

# for each site
tide_models <- lapply(tide, function(x){
  # fit several models of tides (with more or less constituents)
  tm <- list(hc60, hc37, hc7, hc4) %>%
    lapply(function(y) ftide(x$depth, x$dateUTC, hcn = y)) 
  # chose the best by AIC
  best <-
  	lapply(tm, AIC) %>% unlist()  %>% which.min()
  tm <- tm[[best]]
  # remove parts of the object to reduce the file size
  tm$qr <- NULL
  tm$model <- NULL
  return(tm)
  })
# 
# d <- data.frame(time = seq(trunc(Sys.time(), 'day'), trunc(Sys.time() + 3600*24, 'day'), 'min'))
# 
# tide_models[[1]] %>% plyr::ldply(function(x){
# 	d %>% plyr::mutate(height = predict(x, from = min(d$time), to = max(d$time), 1/60), 
# 										 h = length(x$coefficients)/2)
# 	
# }) %T>% ggplot2::qplot(x = time, y = height, data = ., colour = as.factor(h), geom = "line") %>%
# 	plyr::ddply('h', get_high_low_table) %>% plyr::arrange(time)

save(tide_models, file ="../data/processed/tide_models.rds", compress = T)

tide_models %>%
  lapply(function(x){
    list(features1 = x$features1,
         features2 = x$features2,
         msl = x$msl)
  }) %>%
  saveRDS("../data/processed/tide_models_params.rds")