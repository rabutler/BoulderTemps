library(rnoaa)

options(noaakey = Sys.getenv('NOAAAPIKEY'))

tmp <- ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='desc')$data

# boulder, colorado ID
cityId <- 'GHCND:USC00050848'

out1 <- ncdc(datasetid='GHCND', stationid=cityId, datatypeid='PRCP', startdate = '2010-03-01', enddate = '2010-05-31', limit=500)

# average temp, max temp, min
# Per https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt, the units are tenths of
# degrees C
dataId <- c('TMAX','TMIN')

day1 <- '2016-12-01'
day2 <- '2016-12-31'

out2 <- ncdc(datasetid='GHCND', stationid=cityId, datatypeid=dataId[1], 
             startdate = day1, enddate = day2, limit=365)

# convert to degress F from tenths of degrees C
out2$data$value <- out2$data$value / 10 * 9/5 +32

ncdc_stations(datasetid='GHCND', stationid='cityId')
