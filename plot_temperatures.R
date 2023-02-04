
plot_temperatures <- function(zz, tvar)
{
  tvar <- match.arg(tvar, c("tmin", "tmax"))
  
  title_var <- c("tmin" = "Lows", "tmax" = "Highs")
  label_var <- c("tmin" = "minimum", "tmax" = "maximum")
  currentYear <- get_current_year()
  # find the last day of current year data
  lastDayOfData <- zz$monDayYr[min(which(is.na(zz$cTmp))) - 1]
  
  # compute the y-range to use based on the data
  yRange <- range(zz$max, zz$min, zz$cTmp, na.rm = TRUE)
  yRange[1] <- floor(yRange[1] / 10) * 10 # round down to nearest 10
  yRange[2] <- ceiling(yRange[2] / 10) * 10 # round up to nearest 10
  yLabs <- seq(yRange[1], yRange[2],10)
  
  eomDays <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  
  titleCol <- "#3c3c3c" # title/subtitle color
  colAnn <- "grey30" # annotation color
  colNewHigh <- "firebrick3" # new record high
  colNewLow <- "blue3" # new record low
  colCurrent <- "grey40" # the current daily high temp line
  colYAxis <- "wheat4"
  
  #colMaxMin <- "wheat2" # the historical min/max color
  #col90 <- "peru" # color of the 5th-95th percentile
  #col50 <- "wheat4" # 25th-50th percentile color
  
  #"#"    f3d49b
  colMaxMin <- "#f1b373"  
  col90 <- "#d85d56"
  col50 <- "#8d4646"
  
  gg <- ggplot(zz) +
    theme(
      plot.background = element_rect(fill = 'white'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(color = colYAxis, linewidth = 1),
      plot.title = element_text(face = 'bold', color = titleCol),
      plot.subtitle = element_text(face = 'bold', size = 9, color = titleCol),
      plot.caption = element_text(face = "italic", size = 8, color = titleCol)
    ) +
    geom_linerange(aes(x = newDay, ymin = min, ymax = max), color = colMaxMin) +
    geom_linerange(aes(x = newDay, ymin = ll, ymax = ul), color = col90) +
    geom_linerange(aes(x = newDay, ymin = q25, ymax = q75), color = col50) +
    geom_line(aes(newDay, cTmp), linewidth = .75, color = colCurrent) +
    scale_x_continuous(expand = c(0,0), labels = month.name, 
                       breaks = c(15,45,75,105,135,165,195,228,258,288,320,350)) +
    coord_cartesian(ylim = range(yLabs)) +
    scale_y_continuous(labels = dgr_fmt(yLabs), breaks = yLabs, expand = c(0,0)) +
    geom_hline(yintercept = yLabs, color = 'white') +
    geom_vline(xintercept = eomDays, color = colYAxis, linetype = 3, size = .5) +
    geom_point(aes(x = newDay, y = recordHigh), color = colNewHigh) +
    geom_point(aes(x = newDay, y = recordLow), color = colNewLow) +
    labs(title = paste("Boulder's Daily", title_var[tvar], "in", currentYear), 
         subtitle = 'Temperature',
         caption = paste("Last updated:", now()))
  
  hist_years <- get_hist_years()
  
  
  annText <- paste(
    paste("Data represent", label_var[tvar], "daily temperatures."), 
    paste("Historical data available for",
      paste0(paste(hist_years, collapse = "-"), ".")
    ), 
    paste(currentYear, "data included through:", lastDayOfData), 
    sep = "\n"
  )
  
  legData <- data.frame(x = 176:181, y = c(17,15,18,22,20,23)-2)
  if (tvar == "tmin")
    legData$y <- legData$y - 20
  
  bracket_bottom <- get_legend_y(tvar, "hist_75_25", "stop")
  bracket_top <- get_legend_y(tvar, "hist_75_25", "start") + 0.2
  
  gg <- gg + 
    geom_label(
      aes(x = 8, y = max(yLabs), label = annText), 
      color = colAnn, 
      size = 3, 
      hjust = 0, 
      vjust = 1, 
      fill = "white", 
      label.size = 0
    ) +
    # legend bars get_legend_y <- function(tvar, bar, ss)
    annotate(
      'segment', 
      x = 181, xend = 181, 
      y = get_legend_y(tvar, "hist_max_min", "start"), 
      yend = get_legend_y(tvar, "hist_max_min", "stop"), 
      color = colMaxMin, size = 3
    ) +
    annotate(
      "segment", 
      x = 181, xend = 181, 
      y = get_legend_y(tvar, "hist_95_5", "start"), 
      yend = get_legend_y(tvar, "hist_95_5", "stop"), 
      color = col90, size = 3
    ) +
    annotate(
      "segment", 
      x = 181, xend = 181, 
      y = get_legend_y(tvar, "hist_75_25", "start"), 
      yend = get_legend_y(tvar, "hist_75_25", "stop"), 
      color = col50, size = 3
    ) +
    annotate(
      "point", 
      x = 181, y = get_legend_y(tvar, "record_high", "start"), 
      color = colNewHigh
    ) +
    annotate(
      "point", 
      x = 181, y = get_legend_y(tvar, "record_low", "start"), 
      color = colNewLow
    ) +
    # legend current line
    geom_line(data = legData, aes(x = x, y = y), color = colCurrent, size = .75) +
    # legend bracket
    annotate(
      "segment", 
      x = 183, xend = 185, y = bracket_bottom, yend = bracket_bottom, 
      color = col50, size = 0.5
    ) +
    annotate(
      "segment", 
      x = 183, xend = 185, y = bracket_top, yend = bracket_top, 
      color = col50, size=.5
    ) +
    annotate(
      "segment", 
      x = 185, xend = 185, y = bracket_top, yend = bracket_bottom, 
      color = col50, size = 0.5
    ) +
    # legend labels
    geom_label(
      aes(
        x = 186, y = get_legend_y(tvar, "med_label", "start"), 
        label = "NORMAL RANGE\n25TH-75TH PERCENTILE"
      ), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 175, y = get_legend_y(tvar, "current_label", "start"), 
        label = paste(currentYear, "TEMPERATURE")
      ), 
      size = 2, 
      colour = colAnn, 
      hjust = 1, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "hist_max_label", "start"), 
        label = "HISTORICAL RECORD HIGH"
      ), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "record_low_label", "start"), 
        label = "NEW RECORD LOW"
      ), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "hist_min_label", "start"), 
        label = "HISTORICAL RECORD LOW"
      ), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "hist_5_label", "start"), 
        label = "5TH PERCENTILE"
      ), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "hist_95_label", "start"), 
        label = "95TH PERCENTILE"
      ), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(
        x = 183, y = get_legend_y(tvar, "record_high_label", "start"), 
        label = "NEW RECORD HIGH"
      ), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    )
  
  gg
}

# function to turn y-axis labels into degree formatted values
# from: http://rpubs.com/bradleyboehmke/weather_graphic
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

#' @param bar The name of the place in the legend bar, e.g., record_high, 
#'   hist_max, record_label
#' @param ss "start" or "stop"
get_legend_y <- function(tvar, bar, ss)
{
  tvar <- match.arg(tvar, c("tmin", "tmax"))
  ss <- match.arg(ss, c("start", "stop"))
  
  bar_vals <- c("record_high", "hist_max_min", "hist_95_5", "hist_75_25", 
                "record_low", "record_high_label",
                "hist_max_label", "hist_95_label", "med_label", "hist_5_label",
                "hist_min_label", "record_low_label", "current_label")
  
  assertthat::assert_that(
    bar %in% bar_vals,
    msg = paste(bar, "is not a valid `bar`")  
  )
  
  # for tmax
  y_vals <- if (ss == "start") {
    c(34, -2, 5, 12, -4, 34, 31, 25, 15, 5, -1, -4, 15)
  } else {
    c(34, 32, 25, 18, -4, 34, 31, 25, 15, 5, -1, -4, 15)
  }
  
  if (tvar == "tmin")
    y_vals <- y_vals - 20
 
  
  names(y_vals) <- bar_vals
  
  y_vals[bar]
}

get_hist_years <- function() {
  hData <- read.table('data/boulderdaily.complete.txt', sep = '', skip = 15)
  names(hData) <- c('year', 'mon', 'day', 'tmax', 'tmin', 'precip', 'snow', 
                    'snowcover')
  
  c(min(hData$year), max(hData$year))
}
