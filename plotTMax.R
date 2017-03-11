library(dplyr)
library(ggplot2)
library(lubridate)
source('getData.R')

hData <- read.table('data/boulderdaily.complete.txt', sep = '', skip = 15)
names(hData) <- c('year', 'mon', 'day', 'tmax', 'tmin', 'precip', 'snow', 'snowcover')

# format the historical data

h2 <- hData %>%
  filter(year >= 1898, year < 2017) %>%
  mutate(monDay = paste(mon,day,sep = '-')) %>%
  # remove February 29 and data that is missing
  filter(monDay != '2-29', tmax != -998) %>%
  group_by(monDay) %>%
  summarize(tmax.max = max(tmax), tmax.min = min(tmax), tmax.75 = quantile(tmax, .75),
            tmax.25 = quantile(tmax,.25), tavg = mean(tmax)) %>%
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
  mutate(newDay = seq(1, length(date)))

yRange <- range(h2$tmax.max, h2$tmax.min, cTmp$data$value)
yRange[1] <- floor(yRange[1] / 10) * 10 # round down to nearest 10
yRange[2] <- ceiling(yRange[2] / 10) * 10 # round up to nearest 10
yLabs <- seq(yRange[1], yRange[2],10)
  
eomDays <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)

ggplot(h2) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line.y = element_line(color = 'wheat4', size = 1)) +
  geom_linerange(aes(x = newDay, ymin=tmax.min, ymax=tmax.max), color = "wheat2") +
  geom_linerange(aes(x=newDay, ymin=tmax.25, ymax=tmax.75), colour = "wheat4") +
  geom_line(data = cTmp$data, aes(newDay, value), size = .75, color = 'grey40') +
  scale_x_continuous(expand = c(0,0), labels = month.name, 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350)) +
  coord_cartesian(ylim = range(yLabs)) +
  scale_y_continuous(labels = yLabs, breaks = yLabs) +
  geom_hline(yintercept = yLabs, color = 'white') +
  geom_vline(xintercept = eomDays, color = 'wheat4', linetype = 3, size = .5)

  