# Example usage of subset ASCII service
# Described at http://disc.sci.gsfc.nasa.gov/precipitation/tovas
# Here are the acceptable inputs:
# SERVICE=TRMM_ASCII
#
# BBOX=slat,wlon,nlat,elon
#
# TIME=STARTTIME%2FENDTIME (please retain the same format of YYYY-MM-DDTHH:MM:SS)
#
# FLAGS=Product_version,Plot type (plot type is 'Area' or 'Time', see next line for acceptable Product_version)
#
# SHORTNAME=Product_version (acceptable values are the following: 3B42RT_V7, 3B42RT_Daily, 3B42_V6_Daily, 3B42_V6, 3B43_V6, 3B42_V7, 3B42_V7_Daily, 3B43_V7)
#
# VARIABLES=variable (acceptable values is 'Rain' or 'AccRain')
# http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?SERVICE=TRMM_ASCII&BBOX=2,-54,21,-14&TIME=2012-03-31T21:00:00%2F2012-03-31T21:00:00&FLAGS=3B42_V7,Area&SHORTNAME=3B42_V7&VARIABLES=Rain


# Example below:

# trmm_call <- "http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?SERVICE=TRMM_ASCII&BBOX=10.839,-85.618,10.839,-85.618&TIME=1998-01-01T00:00:00%2F2015-03-31T23:59:59&FLAGS=3B43_V7,Time&SHORTNAME=3B43_V7&VARIABLES=AccRain"
#
# temp <- tbl_df(read.table(trmm_call, skip = 6, header = TRUE))
#
# temp1 <- temp %>%
#   mutate(date_of = parse_date_time(Time.year.month., "%Y:%m:%d:H")) %>%
#   select(date_of, acc_rain_mm = AccRain)


# See also example here for gridded data: https://matinbrandt.wordpress.com/tag/rainfall-data/
