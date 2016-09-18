
# to find high and low tide times -----------------------------------------

# use first derivative to find inflexion points
inflection <- function(x) {
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

# using second derivative to determine wether inflexion points are maxima or minima
max_or_min <- function(x, z = inflection(x)) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x), differences = 2) > 0L
  !y[z]
}

# use inflection and max/min functions to find high and low tide times
get_high_low_table <- function(height){
  l_m_m <- inflection(height$height)
  
  height %>%
    dplyr::slice(l_m_m) %>%
    dplyr::mutate(l_m_m, 
                  h_l = max_or_min(height$height, l_m_m)) %>%
    dplyr::filter(!is.na(h_l), l_m_m != 1)
}


# For formatting ----------------------------------------------------------


# format tide table for rendering
format_tide_table <- . %>%
  dplyr::mutate(date = format(time, "%d/%m/%y"),
                day = format(time, "%a"),
                time = format(time, "%I:%M %p"),
                h_l = plyr::mapvalues(h_l, c(T, F), c("H", "L")),
                height = round(height, 2),
                height = paste(height, h_l)) %>%
  dplyr::select(date, day, time, height)


mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

#empty plot
library(ggplot2)
empty_plot <- ggplot(data.frame()) + 
  geom_point() + xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())
  