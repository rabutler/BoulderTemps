suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(stringr)
  library(svglite)
})
source('getData.R')
source("compute_historical_stats.R")
source("plot_temperatures.R")
source("helpers.R")

# get data through yesterday
day2 <- as.character(Sys.Date() - 1)

# tmax ----------------------
message("get tmax data")
# get historical data
hmax <- compute_historical_stats("tmax")

# get current year data
cTmp <- getTempsThroughToday(day2)

# combine historical and current
hmax <- combine_historical_current(cTmp, hmax)

message("plot tmax")
gg_high <- plot_temperatures(hmax, "tmax")

# tmin ------------------------
# get historical data
message("get tmin data")
hmin <- compute_historical_stats("tmin")

# get current year data
cTmp <- getTempsThroughToday(day2, dataId = "TMIN")

# combine historical and current
hmin <- combine_historical_current(cTmp, hmin)

message("plot tmin")
gg_low <- plot_temperatures(hmin, "tmin")

# save figures ----------------
message("save figures")
file_high <- paste0("figs/boulderHighs_",today(),".png")
file_low <- paste0("figs/boulderLows_",today(),".png")
  
ggsave(
  file_high, 
  plot = gg_high, 
  device = "png", 
  width = 8,
  height = 6, 
  units = "in"
)

ggsave(
  file_low, 
  plot = gg_low, 
  device = "png", 
  width = 8,
  height = 6, 
  units = "in"
)

# copy figures
message("copy figures to website")
siteDir <- "C:/Users/Alan/Documents/projects/rabutler.github.io/images/boulderTemps"

stopifnot(dir.exists(siteDir))

file.copy(
  file_high, 
  file.path(siteDir, "boulderHighs_current.png"), 
  overwrite = TRUE
)

file.copy(
  file_low, 
  file.path(siteDir, "boulderLows_current.png"), 
  overwrite = TRUE
)
