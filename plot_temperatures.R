
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
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(color = colYAxis, size = 1),
      plot.title = element_text(face = 'bold', color = titleCol),
      plot.subtitle = element_text(face = 'bold', size = 9, color = titleCol),
      plot.caption = element_text(face = "italic", size = 8, color = titleCol)
    ) +
    geom_linerange(aes(x = newDay, ymin = min, ymax = max), color = colMaxMin) +
    geom_linerange(aes(x = newDay, ymin = ll, ymax = ul), color = col90) +
    geom_linerange(aes(x = newDay, ymin = q25, ymax = q75), color = col50) +
    geom_line(aes(newDay, cTmp), size = .75, color = colCurrent) +
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
  
  annText <- paste(
    "Data represent", label_var[tvar], 
    "daily temperatures. Historical data available for 1896-2019.", 
    currentYear, "data included through:", lastDayOfData
  )
  
  legData <- data.frame(x = 176:181, y = c(17,15,18,22,20,23)-2)
  
  gg <- gg + 
    geom_label(
      aes(x = 8, y = max(yLabs), label = stringr::str_wrap(annText, 50)), 
      color = colAnn, 
      size = 3, 
      hjust = 0, 
      vjust = 1, 
      fill = "white", 
      label.size = 0
    ) +
    annotate('segment', x = 181, xend = 181, y = -2, yend = 32, color = colMaxMin, size = 3) +
    annotate("segment", x = 181, xend = 181, y = 5, yend = 25, color = col90, size = 3) +
    annotate("segment", x = 181, xend = 181, y = 12, yend = 18, color = col50, size = 3) +
    annotate("point", x = 181, y = 34, color = colNewHigh) +
    annotate("point", x = 181, y = -4, color = colNewLow) +
    geom_line(data = legData, aes(x = x, y = y), color = colCurrent, size = .75) +
    annotate("segment", x = 183, xend = 185, y = 18, yend = 18, color = col50, size=.5) +
    annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, color = col50, size=.5) +
    annotate("segment", x = 185, xend = 185, y = 12.2, yend = 18, color = col50, size=.5) +
    geom_label(
      aes(x = 186, y = 15, label = "NORMAL RANGE\n25TH-75TH PERCENTILE"), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 175, y = 15, label = paste(currentYear, "TEMPERATURE")), 
      size = 2, 
      colour = colAnn, 
      hjust = 1, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = 31, label = "HISTORICAL RECORD HIGH"), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = -4, label = "NEW RECORD LOW"), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = -1, label = "HISTORICAL RECORD LOW"), 
      size = 2, 
      colour = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = 5, label = "5TH PERCENTILE"), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = 25, label = "95TH PERCENTILE"), 
      size = 2, 
      color = colAnn, 
      hjust = 0, 
      vjust = .5,
      label.size = 0
    ) +
    geom_label(
      aes(x = 183, y = 34, label = "NEW RECORD HIGH"), 
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
