[Back to Respository](https://github.com/camposfa/ClimGrid)

Utility functions for climate data
----------------------------------

`ClimGrid` includes functions for pulling the most recent data for several large-scale climate oscillation indices from various online data repositories. Supported climate indices include:

-   Niño 1+2 Monthly ERSSTv4 ("nino1.2")
-   Niño 3 Monthly ERSSTv4 (""nino3")
-   Niño 4 Monthly ERSSTv4 ("nino4")
-   Niño 3.4 Monthly ERSSTv4 ("nino3.4")
-   Southern Oscillation Index ("soi")
-   Oceanic Niño Index ("oni"), same as 3-month running average in Niño 3.4
-   Multivariate ENSO Index ("mei")
-   Dipole Mode index ("dmi")
-   Pacific Decadal Oscillation ("pdo")
-   Atlantic Multidecadal Oscillation ("amo")
-   North Atlantic Oscillation ("nao")
-   Southern Annular Mode ("sam")

The function `load_climate_index` returns a named list of monthly climate index data. Each element of the list is an object of class `dplyr::tbl_df`.

``` r
  indices <- load_climate_index(c("nao", "mei"))
```

    ## Reading MEI data from http://www.esrl.noaa.gov/psd/enso/mei/table.html
    ## Reading NAO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table

``` r
  indices
```

    ## $mei
    ## Source: local data frame [789 x 3]
    ## 
    ##       date_of  value index
    ##        (time)  (dbl) (chr)
    ## 1  1950-01-01 -1.027   mei
    ## 2  1950-02-01 -1.149   mei
    ## 3  1950-03-01 -1.298   mei
    ## 4  1950-04-01 -1.081   mei
    ## 5  1950-05-01 -1.427   mei
    ## 6  1950-06-01 -1.391   mei
    ## 7  1950-07-01 -1.309   mei
    ## 8  1950-08-01 -1.057   mei
    ## 9  1950-09-01 -0.627   mei
    ## 10 1950-10-01 -0.402   mei
    ## ..        ...    ...   ...
    ## 
    ## $nao
    ## Source: local data frame [789 x 3]
    ## 
    ##       date_of value index
    ##        (time) (dbl) (chr)
    ## 1  1950-01-16  0.92   nao
    ## 2  1950-02-16  0.40   nao
    ## 3  1950-03-16 -0.36   nao
    ## 4  1950-04-16  0.73   nao
    ## 5  1950-05-16 -0.59   nao
    ## 6  1950-06-16 -0.06   nao
    ## 7  1950-07-16 -1.26   nao
    ## 8  1950-08-16 -0.05   nao
    ## 9  1950-09-16  0.25   nao
    ## 10 1950-10-16  0.85   nao
    ## ..        ...   ...   ...

Squash all the list elements together to a single `tbl_df` with `dplyr::bind_rows`:

``` r
  indices_df <- dplyr::bind_rows(indices)
  summary(indices_df)
```

    ##     date_of                        value             index          
    ##  Min.   :1950-01-01 00:00:00   Min.   :-3.18000   Length:1578       
    ##  1st Qu.:1966-06-04 18:00:00   1st Qu.:-0.67000   Class :character  
    ##  Median :1982-11-08 12:00:00   Median : 0.01450   Mode  :character  
    ##  Mean   :1982-11-08 01:06:36   Mean   : 0.01378                     
    ##  3rd Qu.:1999-04-12 06:00:00   3rd Qu.: 0.68925                     
    ##  Max.   :2015-09-16 00:00:00   Max.   : 3.04900
