#' Obtain data on large-scale climate oscillation indices.
#'
#' Returns a named list of dplyr::tbl_df objects.
#'
#' @param index One or more climate indices.
#' Supported indices include.
#' \itemize{
#' \item Niño 1+2 Monthly ERSSTv4 ("nino1.2")
#' \item Niño 3 Monthly ERSSTv4 (""nino3")
#' \item Niño 4 Monthly ERSSTv4 ("nino4")
#' \item Niño 3.4 Monthly ERSSTv4 ("nino3.4")
#' \item Southern Oscillation Index ("soi")
#' \item Oceanic Niño Index ("oni"), same as 3-month running average in Niño 3.4
#' \item Multivariate ENSO Index ("mei")
#' \item Dipole Mode index ("dmi")
#' \item Pacific Decadal Oscillation ("pdo")
#' \item Atlantic Multidecadal Oscillation ("amo")
#' \item North Atlantic Oscillation ("nao")
#' \item Southern Annular Mode ("sam")
#' }
#' @export
#' @examples
#' indices <- load_climate_index(c("mei", "oni"))
load_climate_index <- function(index){

  `%ni%` = Negate(`%in%`)

  res <- list()

  if ("dmi" %in% index) {
    message("Reading DMI data from http://www.jamstec.go.jp/frcgc/research/d1/iod/DATA/dmi.monthly.txt")
    dmi <- dplyr::tbl_df(read.table("http://www.jamstec.go.jp/frcgc/research/d1/iod/DATA/dmi.monthly.txt",
                                    header = TRUE))
    names(dmi) <- c("date_of", "west", "east", "value")

    if (nrow(dmi) > 0) {
      dmi <- dmi %>%
        dplyr::mutate(date_of = lubridate::parse_date_time2(as.character(date_of), "Y:m:d:H"),
                      index = "dmi") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["dmi"]] <- dmi
    }
    else{
      message("Error reading file.")
    }
  }

  if ("mei" %in% index) {

    message("Reading MEI data from http://www.esrl.noaa.gov/psd/enso/mei/table.html")
    # Starts at 1950
    # Read in current year - 1950 + 1 lines
#     mei <- dplyr::tbl_df(read.table("http://www.esrl.noaa.gov/psd/enso/mei/table.html",
#                                     skip = 12, nrows = lubridate::year(Sys.Date()) - 1950 + 1,
#                                     header = TRUE, fill = TRUE))

    mei_names <- read.table("http://www.esrl.noaa.gov/psd/enso/mei/table.html",
                            skip = 12, nrows = 1, header = FALSE)

    mei <- suppressWarnings(dplyr::tbl_df(data.table::fread("http://www.esrl.noaa.gov/psd/enso/mei/table.html", header = FALSE,
                                         skip = 12, showProgress = FALSE, data.table = FALSE)))

    names(mei) <- as.matrix(mei_names[1, ])

    if (nrow(mei) > 0) {
      mei <- tidyr::gather(mei, bimonth, mei, -YEAR)

      mei <- mei %>%
        dplyr::rename(year_of = YEAR, value = mei) %>%
        dplyr::mutate(date_of = substr(mei$bimonth, start = 4, stop = 6),
                      date_of = lubridate::ymd(paste(year_of, date_of, "01", sep = "-")),
                      index = "mei") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["mei"]] <- mei
    }
    else{
      message("Error reading file.")
    }
  }

  if ("oni" %in% index) {

    message("Reading ONI data from http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
    oni <- dplyr::tbl_df(read.table("http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", header = TRUE))

    if (nrow(oni) > 0) {
      oni$SEAS <- plyr::mapvalues(oni$SEAS,
                                  from = c("AMJ", "ASO", "DJF", "FMA",
                                           "JAS", "JFM", "JJA", "MAM",
                                           "MJJ", "NDJ", "OND", "SON"),
                                  to = c("5", "9", "1", "3",
                                         "8", "2", "7", "4",
                                         "6", "12", "11", "10"))

      oni <- oni %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(YR, SEAS, "16", sep = "-")),
                      value = ANOM,
                      index = "oni") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["oni"]] <- oni
    }
    else{
      message("Error reading file.")
    }
  }

  if ("soi" %in% index) {

    message("Reading SOI data from ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html")
    # Starts in 1876
#     soi <- dplyr::tbl_df(read.delim("ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html",
#                                     skip = 12, fill = TRUE,
#                                     nrows = lubridate::year(Sys.Date()) - 1876 + 1))

    soi <- dplyr::tbl_df(read.table("https://climatedataguide.ucar.edu/sites/default/files/SOI.signal.txt"))
    names(soi) <- c("Year", month.abb)

    if (nrow(soi) > 0) {
      soi <- suppressWarnings(tidyr::gather(soi, month_of, soi, -Year))

      soi <- soi %>%
        dplyr::filter(soi != "-99.9") %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(Year,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      value = as.numeric(soi),
                      index = "soi") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["soi"]] <- soi
    }
    else{
      message("Error reading file.")
    }
  }

  if ("pdo" %in% index) {

    message("Reading PDO data from http://jisao.washington.edu/pdo/PDO.latest")

    # Starts 1900
#     pdo <- dplyr::tbl_df(read.table("http://jisao.washington.edu/pdo/PDO.latest",
#                                     skip = 29, header = TRUE, fill = TRUE,
#                                     nrows = lubridate::year(Sys.Date()) - 1900 + 1))

    pdo_names <- read.table("http://jisao.washington.edu/pdo/PDO.latest",
                            skip = 29, nrows = 1, header = FALSE)

    pdo <- suppressWarnings(dplyr::tbl_df(data.table::fread("http://jisao.washington.edu/pdo/PDO.latest", header = FALSE,
                                                            showProgress = FALSE, data.table = FALSE,
                                                            skip = 29)))

    names(pdo) <- as.matrix(pdo_names[1, ])

    if (nrow(pdo) > 0) {
      pdo$YEAR <- stringr::str_replace_all(pdo$YEAR, stringr::fixed("*"), "")
      pdo <- tidyr::gather(pdo, month_of, value, -YEAR)
      pdo <- pdo %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(YEAR,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      value = as.numeric(value),
                      index = "pdo") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["pdo"]] <- pdo
    }
    else{
      message("Error reading file.")
    }
  }

  if ("amo" %in% index) {

    message("Reading AMO data from http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data")

    amo_yrs <- read.table("http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data",
                          nrows = 1)
    # Starts 1948
    amo <- dplyr::tbl_df(read.table("http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data",
                                    skip = 1, fill = TRUE, nrows = amo_yrs$V2 - amo_yrs$V1 + 1))

    if (nrow(amo) > 0) {
      names(amo)[2:13] <- month.abb
      amo <- tidyr::gather(amo, month_of, value, -V1)
      amo <- amo %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(V1,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      value = as.numeric(value),
                      index = "amo") %>%
        dplyr::filter(value != -99.990) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["amo"]] <- amo
    }
    else{
      message("Error reading file.")
    }
  }

  if ("nao" %in% index) {

    message("Reading NAO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table")

    nao <- dplyr::tbl_df(read.table("http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
                                    fill = TRUE))

    if (nrow(nao) > 0) {
      names(nao)[2:13] <- month.abb
      nao <- tidyr::gather(nao, month_of, value, -V1)
      nao <- nao %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(V1,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      value = as.numeric(value),
                      index = "nao") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["nao"]] <- nao
    }
    else{
      message("Error reading file.")
    }
  }

  if ("sam" %in% index) {

    message("Reading SAM data from http://www.nerc-bas.ac.uk/public/icd/gjma/newsam.1957.2007.txt")

    sam <- read.table("http://www.nerc-bas.ac.uk/public/icd/gjma/newsam.1957.2007.txt",
                      header = TRUE, fill = TRUE)

    if (nrow(sam) > 0) {
      sam$year_of <- row.names(sam)
      sam <- tbl_df(sam)
      names(sam)[1:12] <- month.abb
      sam <- tidyr::gather(sam, month_of, value, -year_of)

      sam <- sam %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(year_of,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      index = "sam") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["sam"]] <- sam
    }
    else{
      message("Error reading file.")
    }
  }

  if ("ao" %in% index) {

    message("Reading AO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table")

    ao <- read.table("http://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table",
                     header = TRUE, fill = TRUE)

    if (nrow(ao) > 0) {
      ao$year_of <- row.names(ao)
      ao <- tbl_df(ao)
      names(ao)[1:12] <- month.abb
      ao <- tidyr::gather(ao, month_of, value, -year_of)

      ao <- ao %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(year_of,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      index = "ao") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["ao"]] <- ao
    }
    else{
      message("Error reading file.")
    }
  }

  if (any(c("nino1.2", "nino3", "nino4", "nino3.4") %in% index)) {
    message("Reading ONI data from http://www.cpc.ncep.noaa.gov/data/indices/ersst4.nino.mth.81-10.ascii")
    ersst <- dplyr::tbl_df(read.table("http://www.cpc.ncep.noaa.gov/data/indices/ersst4.nino.mth.81-10.ascii", header = TRUE))

    if (nrow(ersst) > 0) {
      names(ersst)[3:10] <- c("nino1.2_raw", "nino1.2_anom", "nino3_raw", "nino3_anom",
                              "nino4_raw", "nino4_anom", "nino3.4_raw", "nino3.4_anom")

      ersst <- ersst %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(YR, MON, "16", sep = "-"))) %>%
        dplyr::select(-YR, -MON) %>%
        tidyr::gather(index, value, -date_of) %>%
        tidyr::separate(index, into = c("index", "measurement"), sep = "_") %>%
        dplyr::filter(measurement != "raw") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      if ("nino1.2" %in% index) {
        res[["nino1.2"]] <- filter(ersst, index == "nino1.2")
      }
      if ("nino3" %in% index) {
        res[["nino3"]] <- filter(ersst, index == "nino3")
      }
      if ("nino4" %in% index) {
        res[["nino4"]] <- filter(ersst, index == "nino4")
      }
      if ("nino3.4" %in% index) {
        res[["nino3.4"]] <- filter(ersst, index == "nino3.4")
      }

    }
    else{
      message("Error reading file.")
    }
  }


  if (length(res) < length(index)) {
    known_indices <- c("dmi", "mei", "oni", "soi", "pdo", "amo", "nao", "sam", "ao")
    errors <- index[which(index %ni% known_indices)]
    message(paste("Unknown indices:" , paste(errors, collapse = ", "), sep = " "))
  }

  if (length(res) == 0) {
    res = NULL
  }

  return(res)

}
