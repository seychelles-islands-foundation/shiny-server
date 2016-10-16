# tinkering
library(TideHarmonics)
library(magrittr)
load("data/processed/tide_models.rds")

local_max_min <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  # y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

ct <- Sys.time()

pred <- predict(tide_models[[as.integer(input$Loc)]], ct - 12*3600, ct + 12*3600) -
  tide_models[[as.integer(input$Loc)]]$features2[1]

y <- seq(ct - 12*3600, ct + 12*3600, lendigth.out = 1000)

l_m_m <- local_max_min(pred)
dif_times <- difftime(ct, y[l_m_m], units = "s")

current_low_high <- data.frame(y = y[l_m_m[order(abs(dif_times))[1:2]]], 
           pred = pred[l_m_m[order(abs(dif_times))[1:2]]]) %>%
  dplyr::arrange(y)


if(current_low_high$pred[1] < current_low_high$pred[2]){
  water_is <- "raising"
} else {
  water_is <- "falling"
}

current_low_high %<>% dplyr::arrange(pred)



# Calendar ----------------------------------------------------------------

cal <- function(month, year) {
  
  if(!require(chron)) stop('Unable to load chron package')
  
  if(missing(year) && missing(month)) {
    tmp <- month.day.year(Sys.Date())
    year <- tmp$year
    month <- tmp$month
  }
  
  
  if(missing(year) || missing(month)){  # year calendar
    if(missing(year)) year <- month
    par(mfrow=c(4,3))
    tmp <- seq.dates( from=julian(1,1,year), to=julian(12,31,year) )
    tmp2 <- month.day.year(tmp)
    wd <- do.call(day.of.week, tmp2)
    par(mar=c(1.5,1.5,2.5,1.5))
    for(i in 1:12){
      w <- tmp2$month == i
      cs <- cumsum(wd[w]==0)
      if(cs[1] > 0) cs <- cs - 1
      nr <- max( cs ) + 1
      plot.new()
      plot.window( xlim=c(0,6), ylim=c(0,nr+1) )
      text( wd[w], nr - cs -0.5 , tmp2$day[w] )
      title( main=month.name[i] )
      text( 0:6, nr+0.5, c('S','M','T','W','T','F','S') )
    }
    
  } else {  # month calendar
    
    ld <- seq.dates( from=julian(month,1,year), length=2, by='months')[2]-1
    days <- seq.dates( from=julian(month,1,year), to=ld)
    tmp <- month.day.year(days)
    wd <- do.call(day.of.week, tmp)
    cs <- cumsum(wd == 0)
    if(cs[1] > 0) cs <- cs - 1
    nr <- max(cs) + 1
    par(oma=c(0.1,0.1,4.6,0.1))
    par(mfrow=c(nr,7))
    par(mar=c(0,0,0,0))
    for(i in seq_len(wd[1])){ 
      plot.new()
      #box()
    }
    day.name <- c('Sun','Mon','Tues','Wed','Thur','Fri','Sat')
    for(i in tmp$day){
      plot.new()
      box()
      text(0,1, i, adj=c(0,1))
      if(i < 8) mtext( day.name[wd[i]+1], line=0.5,
                       at=grconvertX(0.5,to='ndc'), outer=TRUE ) 
    }
    mtext(month.name[month], line=2.5, at=0.5, cex=1.75, outer=TRUE)
    #box('inner') #optional 
  }
}

cal(10,2011)
par(mfg=c(3,2))  # monday oct 10
text(.5,.5, 'Some\nText', cex=2)

par(mfg=c(2,3)) #Tues oct 4
text(1,1, 'Top Right', adj=c(1,1))

par(mfg=c(2,4)) # Wed oct 5
text(0,0, 'Bottom Left', adj=c(0,0))

par(mfg=c(6,2)) # oct 31
tmp.x <- runif(25)
tmp.y <- rnorm(25,tmp.x,.1)
par(usr=c( range(tmp.x), range(tmp.y) ) )
points(tmp.x,tmp.y)


  

get_high_low_table(height) %>%
  format_tide_table()


data.frame(time = height$time[l_m_m],
           height = hei)


dif_times <- difftime(ct, y[l_m_m], units = "s")

current_low_high <- data.frame(y = y[l_m_m[order(abs(dif_times))[1:2]]], 
                               pred = pred[l_m_m[order(abs(dif_times))[1:2]]]) %>%
  dplyr::arrange(y)





############
datum <- function(){
  12.93
}
t2014 <- predict_height(as.POSIXct(as.Date(c("2014-01-01", "2015-01-01"))), tide_models[[1]], 1)

get_high_low_table(t2014)


# calendar ----------------------------------------------------------------


input <- as.Date("2016-08-01", tz = "Indian/Mahe") # input date

st <- as.Date(cut(input, "month")) # calculate start of month
dates31 <- st + 0:30 # st and next 30 dates (31 in all)
dates <- dates31[months(dates31) == months(st)] # keep only dates in st's month

calendar <- data.frame(date = dates) %>%
  dplyr::mutate(day = as.numeric(format(date, "%d")),
                week = as.numeric(format(date, "%U")),
                dow = factor(format(date, "%A"),
                             c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) 

tides <- a %>%
  dplyr::filter(time >= trunc(as.POSIXct(min(calendar$date)- 7, tz = "Indian/Mahe"), "day"),
                time <= trunc(as.POSIXct(max(calendar$date) + 7, tz = "Indian/Mahe"), "day"))

# 
# p <- ggplot(calendar, aes(x = dow, y = week, fill = above)) +
#   geom_tile(colour = "black") +
#   geom_text(aes(label = day), nudge_y = 0.5, vjust = 1.8) +
#   theme_bw() +
#   theme(axis.ticks = element_blank()) +
#   theme(axis.title = element_blank()) +
#   theme(legend.position = "none") +
#   scale_x_discrete(expand = c(0,0))
# p

tides %<>%
  dplyr::mutate(week = as.numeric(format(time, "%U")),
                dow = factor(format(time, "%A"),
                             c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                date = time, 
                time = lubridate::hour(time) + lubridate::minute(time)/60) %>%
  dplyr::slice(1:(nrow(tides)-1)) %>%
  dplyr::filter(week %in% unique(calendar$week)) %>%
  dplyr::mutate(this_month = as.Date(date) %in% calendar$date)

h_l <- get_high_low_table(tides) %>% 
  dplyr::mutate(h_l = plyr::mapvalues(h_l, c(T, F), c("H", "L")),
                height = round(height, 1),
                height = format(height, digits = 1, justify = "right"),
                high_low = paste(format(date, "%H:%M"), height, h_l, sep = ""),
                date = as.Date(trunc(date, "day"))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(month = mean(month),
                   week = mean(week),
                   dow = unique(dow),
                   this_month = any(this_month),
                   h_l_t = do.call(paste, as.list(c(high_low, sep = "\n")))) 

p <- tides %>%
  # dplyr::slice(seq(1, nrow(tides), 3)) %>%
  dplyr::filter(lubridate::hour(date) >=7,
                lubridate::hour(date) < 19) %>%
  dplyr::mutate(boating = cut(height, c(-10, 1.6, 2, 10))) %>%
  ggplot(aes(x = time, y = height)) +
  geom_rect(fill = "white", xmin = 6, xmax = 20, ymin = -5, ymax = 0, alpha = 0.5) +
  # geom_rect(fill = "white", xmin = 6, xmax = 20, ymin = 2.8, ymax = 5, alpha = 0.5) +
  geom_tile(aes(y = 0, fill = boating, alpha = this_month), height = 0.25) +
  geom_line(aes(alpha = this_month), size = 0.7) +
  # geom_point(data = calendar, x = 7, y = 2.8, size =3, alpha = 1, colour = "white") + 
  geom_text(data = calendar, aes(label = day), x = 7, y = -2.5,
            hjust = "left", size =4, vjust = "inward",
            family = "Courier") + 
  geom_text(data = dplyr::filter(h_l, T), 
            aes(label = h_l_t, x = 19, y = -2.5, alpha = this_month), 
            vjust = "inward", hjust = "right", size = 3.7,family = "Courier",
            lineheight = 0.8) +
  # geom_text(data = dplyr::filter(h_l, h_l == "L"), 
  #           aes(label = h_l_t, x = 19, y = -1.25), 
  #           vjust = "inward", hjust = "right", size = 2.6,family = "Courier",
  #           lineheight = 0.8) +
  # coord_cartesian(ylim= c(-2.5,3)) +
  scale_x_continuous(breaks = seq(6,20, 2)) +
  scale_y_continuous(breaks = 0:3) +
  facet_grid(week~ dow, scales = "free_x") +
  scale_fill_manual(values = c("#e41a1c", "#ccebc5", "#4daf4a")) +
  scale_alpha_manual(values = c(0.25, 0.75)) +
  theme_bw() + 
  theme(legend.position = "none",
        strip.text.y = element_blank(), 
        axis.text = element_text(family = "Courier", size = 8),
        axis.title = element_text(size = 9),
        panel.margin = grid::unit(0.2, "lines"),
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 12),
        panel.grid.major = element_line(colour = "grey80"))


ggsave("plot.pdf", plot = p +ggtitle("August"), width = 11, height = 8.5, units = "in")

