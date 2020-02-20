
# combines the current year and historical data into one data frame and adds in
# indicators for record highs and record lows for the year
combine_historical_current <- function(cTmp, h2)
{
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
    mutate(recordHigh = if_else(cTmp > max, cTmp, NaN),
           recordLow = if_else(cTmp < min, cTmp, NaN)) %>%
    arrange(newDay) # make sure it is ordered
  
  h2
}

get_current_year <- function()
{
  as.numeric(format(Sys.Date(), "%Y"))
}
