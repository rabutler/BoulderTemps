library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(svglite)
source('getData.R')

# function to turn y-axis labels into degree formatted values
# from: http://rpubs.com/bradleyboehmke/weather_graphic
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}


hData <- read.table('data/boulderdaily.complete.txt', sep = '', skip = 15)
names(hData) <- c('year', 'mon', 'day', 'tmax', 'tmin', 'precip', 'snow', 'snowcover')

# format the historical data

currentYear <- as.numeric(format(Sys.Date(), "%Y"))

h2 <- hData %>%
  filter(year >= 1898, year < currentYear) %>%
  mutate(monDay = paste(mon,day,sep = '-')) %>%
  # remove February 29 and data that is missing
  filter(monDay != '2-29', tmax != -998) %>%
  group_by(monDay) %>%
  summarize(tmax.max = max(tmax), tmax.min = min(tmax), tmax.75 = quantile(tmax, .75),
            tmax.25 = quantile(tmax,.25), tmax.ul = quantile(tmax, .95),
            tmax.ll = quantile(tmax, .05), tavg = mean(tmax)) %>%
  mutate(monDay = mdy(monDay, truncated = TRUE)) %>%
  arrange(monDay) %>%
  mutate(newDay = seq(1, length(monDay)))

day2 <- as.character(Sys.Date() - 1)
# **** to do
# make the conversion to newDay a bit safer-- maybe
# also, add in the variable to see if the current day value is greater than or less than the
# record; probably combine with h2 so we don't have to use two different data frames
cTmp <- getTempsThroughToday(day2)
cTmp$data <- cTmp$data %>%
  mutate(monDayYr = ymd(str_split_fixed(date, 'T', 2)[,1])) %>%
  arrange(monDayYr) %>%
  mutate(newDay = seq(1, length(date))) %>%
  select(monDayYr, newDay, value) %>%
  rename(cTmp = value)

# join the current data with the historical data
h2 <- h2 %>%
  left_join(cTmp$data, by = 'newDay') %>%
  # if the current days high temp is a record high, or record low, set it to cTmp
  mutate(recordHigh = if_else(cTmp > tmax.max, cTmp, NaN),
         recordLow = if_else(cTmp < tmax.min, cTmp, NaN)) %>%
  arrange(newDay) # make sure it is ordered

# find the last day of 2017 data
lastDayOfData <- cTmp$data$monDayYr[min(which(is.na(h2$cTmp))) - 1]

# compute the y-range to use based on the data
yRange <- range(h2$tmax.max, h2$tmax.min, h2$cTmp, na.rm = TRUE)
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

gg <- ggplot(h2) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line.y = element_line(color = colYAxis, size = 1),
        plot.title = element_text(face = 'bold', color = titleCol),
        plot.subtitle = element_text(face = 'bold', size = 9, color = titleCol),
        plot.caption = element_text(face = "italic", size = 8, color = titleCol)) +
  geom_linerange(aes(x = newDay, ymin=tmax.min, ymax=tmax.max), color = colMaxMin) +
  geom_linerange(aes(x = newDay, ymin = tmax.ll, ymax = tmax.ul), color = col90) +
  geom_linerange(aes(x=newDay, ymin=tmax.25, ymax=tmax.75), color = col50) +
  geom_line(aes(newDay, cTmp), size = .75, color = colCurrent) +
  scale_x_continuous(expand = c(0,0), labels = month.name, 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350)) +
  coord_cartesian(ylim = range(yLabs)) +
  scale_y_continuous(labels = dgr_fmt(yLabs), breaks = yLabs, expand = c(0,0)) +
  geom_hline(yintercept = yLabs, color = 'white') +
  geom_vline(xintercept = eomDays, color = colYAxis, linetype = 3, size = .5) +
  geom_point(aes(x = newDay, y = recordHigh), color = colNewHigh) +
  geom_point(aes(x = newDay, y = recordLow), color = colNewLow) +
  labs(title = paste("Boulder's Daily Highs in", currentYear), 
       subtitle = 'Temperature',
       caption = paste("Last updated:", now()))

annText <- paste("Data represent maximum daily temperatures. Historical data available for 1896-2016.", 
                 currentYear, "data included through:", lastDayOfData)

legData <- data.frame(x = 176:181, y = c(17,15,18,22,20,23)-2)

gg <- gg + 
  annotate('text', x = 8, y = max(yLabs), label = stringr::str_wrap(annText, 50), 
            color = colAnn, size = 3, hjust = 0, vjust = 1) +
  annotate('segment', x = 181, xend = 181, y = -2, yend = 32, color = colMaxMin, size = 3) +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, color = col90, size = 3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, color = col50, size = 3) +
  annotate("point", x = 181, y = 34, color = colNewHigh) +
  annotate("point", x = 181, y = -4, color = colNewLow) +
  geom_line(data = legData, aes(x = x, y = y), color = colCurrent, size = .75) +
  annotate("segment", x = 183, xend = 185, y = 18, yend = 18, color = col50, size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, color = col50, size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 18, color = col50, size=.5) +
  annotate("text", x = 186, y = 15, label = "NORMAL RANGE\n25TH-75TH PERCENTILE", size=2, colour=colAnn, hjust = 0, vjust = .5) +
  annotate("text", x = 175, y = 15, label = paste(currentYear, "TEMPERATURE"), size=2, colour=colAnn, hjust = 1, vjust = .5) +
  annotate("text", x = 183, y = 31, label = "HISTORICAL RECORD HIGH", size=2, colour=colAnn, hjust = 0, vjust = .5) +
  annotate("text", x = 183, y = -1, label = "HISTORICAL RECORD LOW", size=2, colour=colAnn, hjust = 0, vjust = .5) +
  annotate("text", x = 183, y = 5, label = "5TH PERCENTILE", size = 2, color = colAnn, hjust = 0, vjust = .5) +
  annotate("text", x = 183, y = 25, label = "95TH PERCENTILE", size = 2, color = colAnn, hjust = 0, vjust = .5) +
  annotate("text", x = 183, y = c(34, -4), label = c("NEW RECORD HIGH", "NEW RECORD LOW"), size = 2, color = colAnn, hjust = 0, vjust = .5)

ggsave(paste0("figs/boulderHighs_",today(),".png"), plot = gg, device = "png", width = 8,
       height = 6, units = "in")

siteDir <- "C:/Users/Alan/Documents/projects/site/images/boulderTemps"
ggsave(file.path(siteDir, "boulderHighs_current.png"), plot = gg, device = "png", width = 8,
       height = 6, units = "in")
