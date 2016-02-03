#' Read a nc grid at specific sites
#' @export
extract_nc_values <- function(x, sites, x_var, y_var, t_var, v_var,
                               convert_0_to_360 = FALSE, t_unit = "days",
                               t_origin) {

  sites <- dplyr::mutate(sites, lat_ind = 0, lon_ind = 0)

  longitude <- ncdf4::ncvar_get(x, varid = x_var)
  latitude <- ncdf4::ncvar_get(x, varid = y_var)

  if (t_unit == "monthly") {
    time <- 1:12
  }
  else {
    time <- ncdf4::ncvar_get(x, varid = t_var)
  }

  inds_lon <- (1:dim(longitude))
  inds_lat <- (1:dim(latitude))

  # Convert longitude to degrees east (0 to 360)
  if (convert_0_to_360) {
    sites[sites$lon < 0, ]$lon <- 360 + sites[sites$lon < 0, ]$lon
  }

  for (i in seq_along(sites$site)) {
    sites[i, ]$lat_ind <- which.min(abs(sites[i, ]$lat - latitude))
    sites[i, ]$lon_ind <- which.min(abs(sites[i, ]$lon - longitude))
  }

  res_sites_f <- list()

  # Full data set (in "t")
  for (i in seq_along(sites$site)) {
    temp <- ncdf4::ncvar_get(x, varid = v_var,
                             start = c(sites[i, ]$lon_ind,
                                       sites[i, ]$lat_ind,
                                       1),
                             count = c(1, 1, -1))
    res_sites_f[[i]] <- dplyr::data_frame(t_step = time,
                                          v_var = temp,
                                          site = sites[i, ]$site)
  }

  res_sites_f <- dplyr::bind_rows(res_sites_f)

  if (t_unit == "days") {
    res_sites_f <- res_sites_f %>%
      dplyr::mutate(date_of = t_origin + lubridate::days(t_step))
  }
  else if (t_unit == "hours") {
    res_sites_f <- res_sites_f %>%
      dplyr::mutate(date_of = t_origin + lubridate::hours(t_step))
  }
  else if (t_unit == "date_decimal") {
    res_sites_f <- res_sites_f %>%
      dplyr::mutate(date_of = lubridate::date_decimal(t_step))
  }

  if (t_unit != "monthly") {
    res_sites_f <- res_sites_f %>%
      dplyr::mutate(year_of = lubridate::year(date_of),
                    month_of = lubridate::month(date_of),
                    v_var = as.numeric(v_var)) %>%
      dplyr::select(site, date_of, year_of, month_of, v_var)
  }

  return(res_sites_f)

}