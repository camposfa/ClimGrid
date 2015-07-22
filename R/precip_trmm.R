#' Obtain accumulated precipitation from TRMM satellite for a point location.
#'
#' Returns a named list of dplyr::tbl_df objects.
#'
#' @param site Data frame of point locations for which to obtain TRMM data.
#' `sites` should have three columns
#' \enumerate{
#' \item Name of location
#' \item Latitude of location
#' \item Longitude of location
#' }
#' @param start_date Date of beginning of time period. TRMM data begins on 1998-01-01.
#' @param end_date Date of beginning of time period. Last reliable month is March 2015.
#' @export
#' @examples
#' trmm_data <- trmm_points(df)
trmm_points <- function(sites, start_date = as.Date("1998-01-01"),
                        end_date = as.Date("2015-03-31")){

  if (ncol(sites) != 3 | !is.numeric(sites[1, 2]) | !is.numeric(sites[1, 3])) {
    message("Error reading site.")
    return(NULL)
  }

  if (is.na(lubridate::ymd(start_date)) | is.na(lubridate::ymd(end_date))) {
    message("Error reading start and end dates.")
    return(NULL)
  }

  sites <- as.matrix(sites)

  trmm <- list()

  for (i in 1:nrow(sites)) {
    message(paste("Reading TRMM data for site", sites[i, 1]))

    lat <- sites[i, 2]
    lon <- sites[i, 3]

    call1 <- "http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?SERVICE=TRMM_ASCII&BBOX="
    box <- gsub(" ", "", paste(lat, lon, lat, lon, sep = ","))
    call2 <- "&TIME="
    times <- paste(as.Date(start_date), "T00:00:00%2F", as.Date(end_date), "T23:59:59", sep = "")
    call3 <- "&FLAGS=3B43_V7,Time&SHORTNAME=3B43_V7&VARIABLES=AccRain"

    trmm_call <- paste(call1, box, call2, times, call3, sep = "")

    temp <- dplyr::tbl_df(read.table(trmm_call, skip = 6, header = TRUE))

    temp1 <- temp %>%
      dplyr::mutate(date_of = lubridate::parse_date_time(Time.year.month., "%Y:%m:%d:H")) %>%
      dplyr::select(date_of, acc_rain_mm = AccRain)

    temp1$site <- sites[i, 1]

    temp1 <- dplyr::select(temp1, site, date_of, acc_rain_mm)

    trmm[[i]] <- temp1

  }

  return(trmm)

}
