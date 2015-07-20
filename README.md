ClimGrid
========

`ClimGrid` is an R package for obtaining and analyzing gridded climate data and global climate oscillation indices.

Additional functionality and documentation will be added soon.

Preparation
-----------

To use this package, you first need to install `devtools` with:

``` r
    install.packages("devtools")
```

Then, you can install the latest development version of `ClimGrid` from github:

``` r
  library(devtools)
  devtools::install_github("camposfa/ClimGrid")
```

After you have installed the package once, you can simply load it in the future using:

``` r
  library(ClimGrid)
```

This package makes heavy use of the data manipulation packages [stringr](http://cran.r-project.org/package=stringr), [lubridate](http://cran.r-project.org/package=lubridate), [tidyr](http://cran.r-project.org/package=tidyr), and [dplyr](http://cran.r-project.org/package=dplyr). If not already installed, `ClimGrid` will install these packages automatically.

Vignettes
---------

[Obtaining and analyzing climate oscillation index data](Climate.md)
