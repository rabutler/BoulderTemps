#' Computes the historical statistics for the specified `t_var`
#' @param t_var "tmax" or "tmin"
compute_historical_stats <- function(t_var)
{
  t_var <- match.arg(t_var, c("tmax", "tmin"))
  
  # read in full historical data
  hData <- read.table('data/boulderdaily.complete.txt', sep = '', skip = 15)
  names(hData) <- c('year', 'mon', 'day', 'tmax', 'tmin', 'precip', 'snow', 
                    'snowcover')
  
  currentYear <- get_current_year()
  
  hData <- hData %>%
    filter(year >= 1898, year < currentYear) %>%
    mutate(monDay = paste(mon, day, sep = '-')) %>%
    # remove February 29 and data that is missing
    filter(monDay != '2-29') %>%
    filter_at(vars(all_of(t_var)), any_vars(. > -997)) %>%
    group_by(monDay) %>%
    summarize_at(
      vars(all_of(t_var)), 
      list(
        ~max(.), 
        ~min(.), 
        ~quantile(., .75),
        ~quantile(., .25), 
        ~quantile(., .95), 
        ~quantile(., .05), 
        ~mean(.)
      )
    ) %>%
    rename(q75 = quantile..3, q25 = quantile..4, ul = quantile..5, 
           ll = quantile..6, avg = mean) %>% 
    mutate(monDay = mdy(monDay, truncated = TRUE)) %>%
    arrange(monDay) %>%
    mutate(newDay = seq(1, length(monDay)))
    
    hData
}