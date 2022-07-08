
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
are the work horses of the package. The functions are built after the
described weather quality control schemes by Costa et al. (2021) and
Durre et al. (2010). Both contain sets of plausibility checks for the
weather data including basic tests (for example does the observation
exceed existing temperature and rainfall records?), temporal
plausibility tests (for example erratic day-to-day changes) to spatial
plausibility tests (does the observation make sense in the context of
neighbouring weather station observations?).

The function call is similar in both cases: the targeted weather needs
to be specified, as well as the coordinates of the target weather
station. Furthermore the functions need neighbouring weather station
observations for spatial consistency test as well as the information on
the neighbouring stations coordinates. In case of `weather_qc_costa` the
tested weather variable also needs to be specified. `weather_qc_durre`
automatically applies the tests on Tmin, Tmax and Precip columns.

The data of the target weather needs to be supplied via a named list of
data.frames or tibbles. The individual data.frames need to contain the
columns `Year`, `Month`, `Day`, `Tmin`, `Tmax`, `Tmean` and
`Precipitation`. An example can be seen in the data.frame supplied wiht
the package:

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

As you can see, having more columns than the previous mentioned ones is
fine. Furthermore the Here are two examples how to call the function.

``` r
## how to use the function
weatherQC::weather_qc_costa(weather_list = weather_list)

weatherQC::weather_qc_durre(weather_list = weather_list)
```

In both cases the function return the same list supplied via the
`weather_list` argument with six extra columns, two for each of the
tested variables Tmin, Tmax and Precip. These will be explained at the
example of the column for minimum temperature Tmin. The original column
of Tmin is altered by the function, if the test lead to a flagging of
the observation it is replaced with NA. In case of `weather_qc_durre`
one flag is enough for the removal of the observation, which test lead
to it is indicated in `flag_Tmin`. `Tmin_org` contains the original,
unaltered observations. In case of `weather_QC_durre` at least two tests
need to flag an observation before it gets replaced by NA. The comments
in `flag_Tmin` also look a bit different. The listed numbers correspond
to the five tests, which are (1) fixed limit test, (2) variable limit
test, (3) temporal consistency test , (4) consistency among variables
(which only works for Tmin and Tmax) and (5) spatial consistency test.
In cases of only one listed number, the observation was not removed.

``` r
#weather data needs to be supplied in a named list
weather_list <- list(target_weather)
names(weather_list) <- target_info$id

#example output
qc_outcome <- weather_qc_costa(weather_list = weather_list, 
                               mute = T)
#> Warning in weather_qc_costa(weather_list = weather_list, mute = T): Because arguments aux_info and aux_list were not provided, the spatial
#>             consistency tests are skipped

qc_outcome[[1]][750:760, c("Date", "Tmin", "Tmax", "Tmin_org", "Tmax_org", "flag_Tmin", "flag_Tmax")]
#> # A tibble: 11 x 7
#>    Date        Tmin  Tmax Tmin_org Tmax_org flag_Tmin flag_Tmax
#>    <date>     <dbl> <dbl>    <dbl>    <dbl> <chr>     <chr>    
#>  1 1992-01-20  NA    NA       NA       NA   ""        ""       
#>  2 1992-01-21  NA    NA       NA       NA   ""        ""       
#>  3 1992-01-22  NA    NA      -53.4     47.4 "2, 3, 4" "2, 3, 4"
#>  4 1992-01-23   2.1   3.9      2.1      3.9 ""        "2"      
#>  5 1992-01-24   1.7  NA        1.7      3.8 "4"       "2, 4"   
#>  6 1992-01-25   1.8  12.2      1.8     12.2 ""        ""       
#>  7 1992-01-26   5.6  11.7      5.6     11.7 "4"       "4"      
#>  8 1992-01-27   3.5  12.1      3.5     12.1 ""        ""       
#>  9 1992-01-28   4.1  17.5      4.1     17.5 ""        ""       
#> 10 1992-01-29   2.3  14        2.3     14   ""        ""       
#> 11 1992-01-30   1.7   9.6      1.7      9.6 ""        ""
```

The function keeps the user updated on which test it is currently
working, but the function can also be muted with the `mute = TRUE`
argument. The spatial consistency tests involve a lot of computation are
currently rather slow. If the user wishes to skip the spatial
consistency tests, then this can be done with the
`skip_spatial_test = TRUE` argument. In case the spatial consistency
tests should be carried out, the `skip_spatial_test` should be set to
FALSE and the arguments `weather_info`, `aux_list` and `aux_list` need
to be supplied as well. `weather_info` and `aux_info` should be both
data.frames with as many rows as elements in `weather_list` and
`aux_list`. Furthermore, the columns `id` with a unique identifier for
each weather station and weather station coordinates in decimal format
should be present in the columns `Longitude` and `Latitude`. An example
for the aux_info can be seen here:

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

The aux_list should be structured like in weather_list, a list of named
data.frames (same names as `id` column in aux_info) with the columns
Year, Month, Day, Tmin, Tmax and Precip.

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

## Help and further ressources

For more details on the individual tests within `weather_qc_costa` and
`weather_qc_durre` please use the function help pages which can be found
via `?weather_qc_durre` or `help(weather_qc_durre)`. There will be also
a further vignette which explains the inner workings of
`weather_qc_costa` and `weather_qc_durre` in more detail.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-costa_gap_2021" class="csl-entry">

Costa, Rafaela Lisboa, Heliofábio Barros Gomes, David Duarte Cavalcante
Pinto, Rodrigo Lins da Rocha Júnior, Fabrício Daniel dos Santos Silva,
Helber Barros Gomes, Maria Cristina Lemos da Silva, and Dirceu Luís
Herdies. 2021. “Gap Filling and Quality Control Applied to
Meteorological Variables Measured in the Northeast Region of Brazil.”
*Atmosphere* 12 (10): 1278. <https://doi.org/10.3390/atmos12101278>.

</div>

<div id="ref-durre_comprehensive_2010" class="csl-entry">

Durre, Imke, Matthew J. Menne, Byron E. Gleason, Tamara G. Houston, and
Russell S. Vose. 2010. “Comprehensive Automated Quality Assurance of
Daily Surface Observations.” *Journal of Applied Meteorology and
Climatology* 49 (8): 1615–33. <https://doi.org/10.1175/2010JAMC2375.1>.

</div>

</div>
