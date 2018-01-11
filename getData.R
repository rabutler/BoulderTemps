library(rnoaa)
options(noaakey = Sys.getenv('NOAAAPIKEY'))

#tmp <- ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='desc')$data

# cityId defaults to boulder, colorado ID
# dataId could also be TMIN
getTempsThroughToday <- function(day2, cityId = 'GHCND:USC00050848', dataId = 'TMAX')
{
  #out1 <- ncdc(datasetid='GHCND', stationid=cityId, datatypeid='PRCP', startdate = '2010-03-01', enddate = '2010-05-31', limit=500)

  # average temp, max temp, min
  # Per https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, the units are tenths of
  # degrees C

  day1 <- paste(as.numeric(format(Sys.Date(), "%Y")), "01", "01", sep = "-")

  out2 <- ncdc(datasetid='GHCND', stationid=cityId, datatypeid=dataId, 
               startdate = day1, enddate = day2, limit=365)

  # convert to degress F from tenths of degrees C
  out2$data$value <- out2$data$value / 10 * 9/5 +32

  out2    
}
