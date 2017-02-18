library(dplyr)
library(ggplot2)
library(lubridate)

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

ggplot(h2) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(aes(x = newDay, ymin=tmax.min, ymax=tmax.max), color = "wheat2") +
  geom_linerange(aes(x=newDay, ymin=tmax.25, ymax=tmax.75), colour = "wheat4")

