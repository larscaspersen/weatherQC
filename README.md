
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weatherQC

<!-- badges: start -->
<!-- badges: end -->

weatherQC allows standardized quality control checks on historical daily
weather observations including temperature (minimum, maximum, mean) and
precipitation (daily sum).

## Installation

You can install the development version of weatherQC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("larscaspersen/weatherQC")
```

## How to use the package

weatherQC allows the user to perform standardized quality checks on the
daily weather data. The goal is to fish out as many not-trustworthy
observation and to keep the invasiveness of the operation as low as
possible. The functions `weather_qc_durre()` and `weather_qc_costa()`
are the work horses of the package. Both contain sets of plausibility
checks for the weather data including basic tests (for example does the
observation exceed existing temperature and rainfall records?), temporal
plausibility tests (for example erradic day-to-day changes) to spatial
plausibility tests (does the observation make sense in the context of
neighbouring weather station observations?).

``` r
## how to use the function
weatherQC::weather_qc_costa(weather =  target_weather, 
                 weather_coords = c(target_info$Longitude, target_info$Latitude),
                 variable = "Tmin", 
                 aux_list = neighbour_weather,
                 aux_info = neighbour_info)

weatherQC::weather_qc_durre(weather =  target_weather, 
                 weather_coords = c(target_info$Longitude, target_info$Latitude),
                 aux_list = neighbour_weather,
                 aux_info = neighbour_info)
```

Explain the different objects in more detail

``` r
library(weatherQC)
target_info
#>        id             Name Longitude Latitude Start_date   End_date
#> 1 cimis_7 Firebaugh/Telles  -120.591 36.85125 1982-09-22 2022-06-13
```

``` r
head(neighbour_info)
#>         id                                  Name Longitude Latitude Start_date
#> 1  cimis_7                      Firebaugh/Telles -120.5910 36.85125 1982-09-22
#> 2 cimis_15                             Stratford -119.8514 36.15814 1982-10-29
#> 3 cimis_39                               Parlier -119.5041 36.59748 1983-05-23
#> 4 cimis_80                          Fresno State -119.7423 36.82083 1988-10-03
#> 5 MADERA.C                                Madera -120.0333 36.95000 1951-01-01
#> 6 FRESNO.C Fresno Yosemite International Airport -119.7167 36.76667 1951-01-01
#>     End_date
#> 1 2022-06-13
#> 2 2022-06-13
#> 3 2022-06-13
#> 4 2022-06-13
#> 5 2021-09-21
#> 6 2021-09-21
```

``` r
head(target_weather)
#>    Weather_Station Year Month Day Tmax Tmin Precip QC_Tmax QC_Tmin QC_Precip
#> 1 Firebaugh/Telles 1990     1   1   NA   NA     NA    <NA>    <NA>      <NA>
#> 2 Firebaugh/Telles 1990     1   2 11.2  1.6     NA       *       *         C
#> 3 Firebaugh/Telles 1990     1   3 10.3 -2.2     NA       *       *         C
#> 4 Firebaugh/Telles 1990     1   4 12.4 -2.9     NA       *       *         C
#> 5 Firebaugh/Telles 1990     1   5 12.9 -2.5     NA       *       *         C
#> 6 Firebaugh/Telles 1990     1   6 14.2 -2.5     NA       *       *         C
#>   Tmean QC_Tmean
#> 1    NA     <NA>
#> 2   6.9        *
#> 3   3.5        *
#> 4   3.5        *
#> 5   4.0        *
#> 6   4.7        *
```

``` r
head(neighbour_weather[[1]])
#>    Weather_Station Year Month Day Tmax Tmin Precip QC_Tmax QC_Tmin QC_Precip
#> 1 Firebaugh/Telles 1990     1   1   NA   NA     NA    <NA>    <NA>      <NA>
#> 2 Firebaugh/Telles 1990     1   2 11.2  1.6     NA       *       *         C
#> 3 Firebaugh/Telles 1990     1   3 10.3 -2.2     NA       *       *         C
#> 4 Firebaugh/Telles 1990     1   4 12.4 -2.9     NA       *       *         C
#> 5 Firebaugh/Telles 1990     1   5 12.9 -2.5     NA       *       *         C
#> 6 Firebaugh/Telles 1990     1   6 14.2 -2.5     NA       *       *         C
#>   Tmean QC_Tmean
#> 1    NA     <NA>
#> 2   6.9        *
#> 3   3.5        *
#> 4   3.5        *
#> 5   4.0        *
#> 6   4.7        *
```

show output of the function

## details on weather_qc_durre and weather_qc_costa

show the functions in more details
