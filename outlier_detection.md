---

title: "Outlier detection functions"
author: "Lars Caspersen"
date: '2022-06-13'
output:   
  html_document:
    keep_md: yes
---



## Principle

Screening for outlier in weather data is important. Erroneous weather data can be introduced to weather data sets for example via to malfunctioning of sensors or in the form of "false zeros", where a zero is intended to represent actually a missing data point instead an actual measurement. Often agencies collecting and providing data for the public exercise outlier detection and either mark suspicious weather readings with quality flags or remove them from the data set. Often there are automated checks based on deviations from confidence intervals (for example in CIMIS). I need to make more literature review! 

For my master thesis I originally planned to do some chill analysis for California (since the last proper one is almost ten years ago). I noticed that especially the CIMIS weather data set is full of outliers. Some are easy to spot, but others are more subtle.


![](outlier_detection_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

I realized that some cleaning is necissary before doing further analysis (like feeding the data to weather generators or calculating chill portions). Since manual outlier detection requires time and skill (both of which I do not possess), so automated outlier detection routines are necessary. There are some general data quality paxckages specialised mainly for medical data [Marino 2022] but few are specialized for weather data. The patching and homogenisation package 'Climatol' offers a simple outlier detection where the weather reading is compared to a model output. Based on a (user-defined) threshold daily measurements with a residual (|observation - model|) exceeding the threshold were identified and removed. The reddprec package is specialised on quality control and missing data imputation of precipitation data. A detailed set of weatehr outlier detection algorithm is described by Durre 2010, who also validated the algorithms performance by manually checking the labelled outliers. Instead of using a single criterion a set of different plausibility checks were carried out. The plausibility checks range from basic integrity tests (for example duplicated records in different months) to more detailed tests like spatial integrity checks. These algorithms were also used for the Global Historical Climatology Network (GHCN-) Daily and are allegedly also available as a FORTRAN-code, but I can't find in on the web. A less exhaustive set of outlier tests were proposed by Costa 2021 for a dataset of Brazilian weather stations. Both proposed weather quality algorithms of Costa 2021 and Durre 2010 were here reconstructed in R as close as possible from the descriptions in the manuscripts and tested for a network of weather stations in California including daily observations of minimum and maxium temperature (Tmin, Tmax) and precipitaiton (Precip). A total of 33 weather stations were used having records from 1990 to 2021. Additionally, 92  auxilliary weather stations from the UCIPM-database were used for spatial plausibility test. 

<div class="figure" style="text-align: center">
<img src="figures/map_california_qc.png" alt="Map of target stations (from CIMIS database) and auxiliary weather stations for spatial plausibility checks (from UCIPM database)" width="50%" />
<p class="caption">Map of target stations (from CIMIS database) and auxiliary weather stations for spatial plausibility checks (from UCIPM database)</p>
</div>


## Weather quality tests by Costa 2021

Workin on a dataset of daily precipitation and temperature measurement in north-eastern Brazil, the authors proposed a set of 6 plausibility checks to detect suspicious data. The tests included checks on the meta data, fixed limit test, variable limit tests, consistency among variables, temporal consistency and spatial consistency tests. The main idea is, that a measurement needs to be flagged by at least two of these tests in order to be removed from the dataset. This should reduced the amount of false positive rates. 
The concept of the Costa 2021 quality control tests has been written as an R function called `weather_qc_costa()`. It takes `weather`, `weather_coords`, `var`, `aux_list`, `aux_info`, `level` and `country` as mandatory inputs and returns a boolean vector of the same length as days rows in weather as an output, indicating suspicious data with TRUE and data save to keep with FALSE.
`weather` is a dataframe, organized with the variables in the columns and daily measurenent in the rows. The quality control checks of Costa 2021 were described with a dataset containing minimum and maximum temperature, daily precipitation, relative humididty, atmospheric pressure, windspeed and insulation. However, in its current state it was written to satisfy the needs of the other functions of chillR, which only work with daily extreme temperatures and to a lesser extend with precipitation. Some of the tests require additionally the daily average temperature, so it should be supplied alongside to daily minimum and maximum temperature.Aditionally, the dataframe needs to contain columns called Day, Month and Year. An example for weather can be seen in the following.


```r
head(weather)
```

```
##   Weather_Station Year Month Day Tmax Tmin Precip QC_Tmax QC_Tmin QC_Precip
## 1     Five Points 1990     1   1   NA   NA     NA    <NA>    <NA>      <NA>
## 2     Five Points 1990     1   2 11.8  3.9      0       *       *         *
## 3     Five Points 1990     1   3 11.6 -1.3      0       *       *         *
## 4     Five Points 1990     1   4 13.3 -4.7      0       *       Y         *
## 5     Five Points 1990     1   5 13.2 -3.6      0       *       *         *
## 6     Five Points 1990     1   6 12.6 -2.7      0       *       *         *
##   Tmean QC_Tmean       Date
## 1    NA     <NA> 1990-01-01
## 2   7.5        * 1990-01-02
## 3   4.4        * 1990-01-03
## 4   3.2        * 1990-01-04
## 5   3.8        * 1990-01-05
## 6   4.0        * 1990-01-06
```

`weather_coords` is a numeric vector of length two, which contains the Longitude and Latitude of the target weather station. 

`var` is the variable the quality control function should target. It needs to be the same name as the column name in `weather`.

`aux_list` is a named list containing dataframes of additional weather stations. The data.frames should organized the same way as in `weather`. Furthermore, the list elements should be named.

`aux_info` is a dataframe which should contain the id names of the auxiliary weather stations. These ids should be also used for the naming of the elements contained by `aux_list`. Furthermore, it should contain columns called 'Longitude' and 'Latitude' which contain the coordinates of the auxiliary weather stations. The coordinates are needed for distance calculation to the target weather station. 

The last mandatory arguments are `level` and `country` which need to be both characters. They are needed for the location-specific lookup of measurement records for the variable. Sofar `level` can be either `world` or `USA`. `country` is then used to further narrow down the lookup. In case of `level = 'world'` it needs to be the the name of a country or in case of `level == 'USA'` it needs to be the name of the state the weather stations are located. It can be that for certain countries and variables no records are available. In these cases the records need to be supplied manually via the `records` argument, which is set as NULL in default.


In the following the priciples of the plausibility checks used in the function `weather_qc_costa()` are explained. For more details please refer to the original paper: XYX.

### Metadata test

In this version of the quality control function, the metadata accompanying the weather data was not analyzed. Originally, the test checks if the weather station specific identifier is the same throuhout the weather data.

### Fixed limit test

This test checks if the specified variable is outside of the range of the highest and lowest measurements for the specified subregion. The records can be either supplied manually via `records` or they can be looped up by the function. In case of user-defined records, it needs to contain the lower and upper bound. In case of daily precipitation the lower bound is zero. In case the user does not supply the records manually, a function downloads the weather records from resources in the internet. In case of `level = 'world'` the function scraps data from the website 'https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_extreme_temperatures' which may not be the most professional looking source but I couldn't find anything else which contained most of the countries. If there are ideas for a more trusted source of temperature records, please let me know. Also, in case of variables which are not temperature, records need to be supplied manually. In case of `level = 'USA'` the function loads a csv-file accessible via 'https://www.ncdc.noaa.gov/extremes/scec/records.csv'. Sometimes the server seems to be down, in these cases records need to be supplied manually.


```r
head(get_temp_records(region = 'world'))
```

```
## # A tibble: 6 x 4
##   Country       Tmin  Tmax Precip
##   <chr>        <dbl> <dbl> <lgl> 
## 1 Algeria      -12.8  51.3 NA    
## 2 Botswana     -15    44   NA    
## 3 Burkina Faso   5    47.2 NA    
## 4 Chad          NA    47.6 NA    
## 5 Ghana         NA    43.3 NA    
## 6 Niger         NA    48.2 NA
```

As can be seen, not all countries have a complete set of record temperature and currently there is no data for precipitation. An example for the state-specific data of the United States can be seen next.



```r
head(get_temp_records(region = 'USA'))
```

```
## # A tibble: 6 x 4
##   Country     Tmin  Tmax Precip
##   <chr>      <dbl> <dbl>  <dbl>
## 1 Alabama    -32.8  44.4   826.
## 2 Alaska     -62.2  37.8   382.
## 3 Arizona    -40    53.3   290.
## 4 Arkansas   -33.9  48.9   357.
## 5 California -42.8  56.7   656.
## 6 Colorado   -51.7  46.1   301.
```
The test simply compares if the target is either lower or higher than the region-specific record. An example for Californian minimum temperature was already shown in the beginning of the document.

### Variable limits test

The variable limit test is based on the percentiles of the variable for each month. The test simply flags for each month observaitons higher than the 99% percentile or lower than the 1% percentile. Additionally to the monthly percentiles, the same is done for the whole year percentiles.

![](outlier_detection_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


### Temporal consistency test

This test investigates unexpected jumps and drops in the target variable. Again, the flaggin is based on percentiles. The absolute day-to-day difference of the target variable is calculated. If the difference to the following day exceeds the 99.5% percentile, then this observation receives a flag. The figure below shows the minum temperature for January 1990 at the 'Five Points' weather station. The observation at Jan-10 looks suspicious, as there is a large jump. followed by a strong dip. Using the absolute day-to-day difference and comparing it the 99.5% percentile (10.8°C) it can be seen, that the outlier is successfully detected. However, the following day receives a flag, too. But since the test requires at leas two positive tests, it is unlikely that the falsely flagged second observation at Jan-11 gets marked as suspicious data.

![](outlier_detection_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Wouldnt this test applied to precipitation penelize strong precipitation events? These events probably also get a flag by the variable limits test.

### Variable consistency test

These tests mainly focus on temperature variables. They include simple tests, such as that daily minimum temperature should not exceed daily maximum temperature, or that average temperature should be lower than maximum but higher than minimum temperature of the same day.
Additionally it compares the reported average temperature to the mean of daily minumum and maximum temperature. Absolute residuals of reported to computed daily mean temperautre exceeding the 99% percentile also get flagged. In these cases both minimum and maximum temperature get flagged.

![](outlier_detection_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### Spatial consistency test

The authors mentioned that a spatial consistency test was carried out, but they did not explain any details to it. So for this kind of test, spatial regression of temperature and spatial corrobation test for precipitation were taken instead, which is explained in more detail by Durre 2010 and in the lower section dedicated to the Durre 2010 weather quality test. in general the outcome of the spatial consistency test also depend on the quality of the auxiliary weather stations. 


### Run quality control after Costa 2021


```r
#define variables
id <- weather_info$id[1]
weather <- weather_list[[id]]
target_coord <- c(weather_info$Longitude[weather_info$id == id],
                  weather_info$Latitude[weather_info$id == id])

#run costa 2021 quality control
test_result <- weather_qc_costa(weather = weather, weather_coords = target_coord, variable = 'Tmin',aux_list = aux_data, aux_info = aux_info, level = 'USA', country = 'California')

#check the output
head(test_result)
```


```
##   fixed_limit variable_limit temporal_consistent consistent_variables
## 1       FALSE          FALSE               FALSE                FALSE
## 2       FALSE          FALSE               FALSE                FALSE
## 3       FALSE          FALSE               FALSE                FALSE
## 4       FALSE           TRUE               FALSE                FALSE
## 5       FALSE          FALSE               FALSE                FALSE
## 6       FALSE          FALSE               FALSE                FALSE
##   spatial_consistent outlier
## 1              FALSE   FALSE
## 2              FALSE   FALSE
## 3              FALSE   FALSE
## 4              FALSE   FALSE
## 5              FALSE   FALSE
## 6              FALSE   FALSE
```
As you can see the output is a tibble with six columns. The first five relate to the previous mentioned plausibility checks and the respective outcomes for `variable = 'Tmin'`. The last column called `outlier` indicates that at least two tests showed a positive result. This column could be then used to either manually screen the identified suspicious data or to replace the values by `NA` values if convinced that they are indeed erronous data. 

One of the major drawbacks of this test approach is, that the erronous data can deteriote subsequent tests, especially the spatial consistency test.


### Check flagged Tmin data

For each daily observation of the variables a column indicating potential problems with the data was downloaded. The details of the flag codes can be read in Echem & Temesgen (no date) and statistical tests for the data in Eching and Snysder (no date). In short there are flags indicating `R` data for outside the range of historical range (x > 99.8% percentile), `S` consistency problems of Tmin and Tmax or problems with the sensor and `Y` data moderately outside historical range (x > 96%). Further flags include `M` for missing data, `P` for pending quality test. While not explained in the manual, there can be also the quality flag `*` or no quality flag at all. I assume that in these cases there are no problems with the observation. A summary of the flags for `Tmin` for the weather station 'Five Points' can be seen below


```r
table(weather$QC_Tmin)
```

```
## 
##         *    M    P    R    S    Y 
## 2440 8087  316    3   21   55  365
```

Focussing on the flags `S` (=erronous sensor or inconsistency among variables) and `Y` (=strong historical outliers) we can see that the costa 2021 quality detects less than half of the flagged data. 


```r
#add test result to weather data frame
weather <- cbind(weather, test_result)

#cases of flag and outlier detected
sum(weather$QC_Tmin %in% c('R', 'S') & weather$outlier == T) / sum(weather$QC_Tmin %in% c('R', 'S'))
```

```
## [1] 0.4605263
```

The detected cases were also pretty obvious


```r
weather %>%
  filter(QC_Tmin %in% c('R', 'S') & outlier == T) %>%
  select(Date, Tmax, Tmin, QC_Tmin, outlier)%>%
  head(n = 10)
```

```
##          Date   Tmax   Tmin QC_Tmin outlier
## 1  1990-03-27   27.6  -35.9       R    TRUE
## 2  1990-06-26   79.7 -134.0       S    TRUE
## 3  1990-07-01   85.5   51.4       R    TRUE
## 4  1990-07-02   85.5   84.9       R    TRUE
## 5  1990-07-03 2342.0 -134.0       S    TRUE
## 6  1990-07-05   38.7  -48.8       R    TRUE
## 7  1990-12-14   85.7   36.1       R    TRUE
## 8  1990-12-15   85.7   38.7       R    TRUE
## 9  1990-12-22  588.5   -8.0       R    TRUE
## 10 1990-12-23    8.7  -53.7       S    TRUE
```


Lets look at the cases which received a flag of `R` or `S` but remained undetected by the test. 


```r
weather %>%
  filter(QC_Tmin %in% c('R', 'S') & outlier == F) %>%
  select(Date, Tmax, Tmin, QC_Tmin, outlier)%>%
  head(n = 10)
```

```
##          Date Tmax Tmin QC_Tmin outlier
## 1  1990-02-20  1.0  1.6       S   FALSE
## 2  1996-08-13 42.2   NA       S   FALSE
## 3  1998-09-06 35.8 23.8       R   FALSE
## 4  2000-12-09   NA   NA       S   FALSE
## 5  2000-12-10   NA   NA       S   FALSE
## 6  2000-12-11   NA   NA       S   FALSE
## 7  2000-12-12   NA   NA       S   FALSE
## 8  2000-12-13   NA   NA       S   FALSE
## 9  2000-12-14   NA   NA       S   FALSE
## 10 2000-12-15   NA   NA       S   FALSE
```


 In many cases the data was already marked by an `NA`. 
 

```r
sum(weather$QC_Tmin %in% c('R', 'S') & is.na(weather$Tmin))
```

```
## [1] 31
```


This means the quality test didn't have a chance to detect 31 out of the 76 cases. So the rate of correctly detecting flags R and S by the Costa 2021 quality test is actually


```r
sum(weather$QC_Tmin %in% c('R', 'S') & is.na(weather$Tmin) ==F & weather$outlier == T) / sum(weather$QC_Tmin %in% c('R', 'S') & is.na(weather$Tmin) ==F)
```

```
## [1] 0.7777778
```

So rouhly 78% of the flags were detected. Not bad bad also not great. Lets have a look at the cases which slipped through the test.


```r
weather %>%
  filter(QC_Tmin %in% c('R', 'S') & outlier == F & is.na(Tmin) == F) %>%
  select(Date, Tmax, Tmin, QC_Tmin, outlier)%>%
  head(n = 10)
```

```
##          Date Tmax  Tmin QC_Tmin outlier
## 1  1990-02-20  1.0   1.6       S   FALSE
## 2  1998-09-06 35.8  23.8       R   FALSE
## 3  2009-03-10 14.9  -4.4       R   FALSE
## 4  2010-01-02  0.3 -32.9       S   FALSE
## 5  2010-01-03 11.9 -24.6       S   FALSE
## 6  2010-06-28 40.9  24.3       R   FALSE
## 7  2010-08-28 26.3   6.7       R   FALSE
## 8  2010-10-02 34.9  20.7       R   FALSE
## 9  2020-08-17 39.8  23.9       R   FALSE
## 10 2020-08-18 38.8  25.8       R   FALSE
```

There is at least one case in which the Tmax was smaler than Tmin. It probably remained undetected because of the rule that at least two tests need to detect an outlier. This is a major drawback of this approach, because it is safe to assume that this observation is fishy. 

Another aspect are the cases which were marked as outlier by the test, but did not get any R or S flag.


```r
weather %>%
  filter(!(QC_Tmin %in% c('R', 'S')) & outlier == T) %>%
  select(Date, Tmax, Tmin, QC_Tmin, outlier)%>%
  head(n = 10)
```

```
##          Date Tmax Tmin QC_Tmin outlier
## 1  1990-06-25 48.6 24.4       *    TRUE
## 2  1990-07-31 40.5 23.5       *    TRUE
## 3  1990-09-14 42.8  1.1       Y    TRUE
## 4  1990-12-13 85.7  3.7       *    TRUE
## 5  1990-12-26 44.6 23.5       *    TRUE
## 6  1991-05-26 29.1  6.1       *    TRUE
## 7  1991-10-09 34.4 19.5       *    TRUE
## 8  1992-01-05 32.7  9.6       *    TRUE
## 9  1992-01-09 70.4 28.1       Y    TRUE
## 10 1992-01-10 63.1 20.3       *    TRUE
```
 
 
![](outlier_detection_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

The circled point of Tmin was labelled by the Costa 2021 quality control function as an outlier, while the almost equally low observations of the two days prior remained in the end unflagged. All three points were flagged by the variable limits test. However, the circle point was also flagged by the temporal consistency test due to the sudden jump in temperature the day after and by the consistency among variables test, because the calculated mean temperature was far off the reported mean temperature (this might have been caused by the equally suspicious observaiton of Tmax at that day). 

In total the weather quality test by Costa 2021 flagged 95 cases of Tmin (0.8%). It seems the majority of flagged data include unreasonably high or low readings, but there were also several cases of 'false zeros'


```r
weather %>%
  ggplot(aes(x = Date, y = Tmin)) + 
  geom_point() + coord_cartesian(ylim = c(-20,40)) + 
  geom_point(data = weather[weather$outlier,], aes (x = Date, y = Tmin), col = 'red')
```

![](outlier_detection_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


check which test lead how often to a flagged outlier
sum


```r
data.frame(test = c(rep('fixed_limt', 2),
                    rep('variable_limit',2),
                    rep('temporal_consistent', 2),
                    rep('consistent_variable', 2),
                    rep('spatial_consistent', 2)), 
           outlier = rep(c(T,F), 5), 
           cases = c(sum(weather$outlier & weather$fixed_limit),
                     sum(weather$outlier == F & weather$fixed_limit),
                     sum(weather$outlier & weather$variable_limit),
                     sum(weather$outlier == F & weather$variable_limit),
                     sum(weather$outlier & weather$temporal_consistent),
                     sum(weather$outlier == F & weather$temporal_consistent),
                     sum(weather$outlier & weather$consistent_variables),
                     sum(weather$outlier == F & weather$consistent_variables),
                     sum(weather$outlier & weather$spatial_consistent),
                     sum(weather$outlier == F & weather$spatial_consistent))) %>%
  ggplot(aes(x = test, y= cases, fill = outlier)) + 
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](outlier_detection_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

It can be seen that the consistency among variables and the variable limits test lead to the most instances of outlier flags, however, they both have a low specificity as the share of stand-alone positive test results is high. Positive fixed limit test results always lead to outlier flags, the is very specific but not very sensitive as the majority of outlier flags came from other tests. Spatial consistency and temporal consistency lie somewhere in the middle between specificity and sensitivity. In both cases true test results also lead in the majority of cases also to flagging of outlier.



## Weather quality control after Durre 2010

As we could see the testing philosophy of Costa 2021 was to use the union of very sensitive test and more specific ones to flag suspicious data. The testing philosophy of Durre 2010 quality control schemes is different. The qc-scheme uses more tests, the testing limits are however very wide and the testing order plays an important role. Each positive test leads automaically to an outlier flag and the removal of the observation. The testing ranges are wide to reduce the false positive rate. The first tests are very broad and become more and more specific with the progress of the qc-scheme. At first basic integrity tests are carried out to detect duplicated months or 'flase zeros'. Then come the outlier checks for instance for climatological outliers (similar to the variable limits test). However, the tests rely on hard threshold instead of percentiles.  The comes the temporal consistency tests like the spike and dips test (similar to the temporal consistency test of Costa 2021). Next come spatial consistency test which include linear regressions and corrobation tests. Finally megaconsistency test look for remaining incosnsistency of the data. The original testing framework of Durre 2010 included tests for snow coverage, but these were skipped in the here presented R-functions. In the following the test will be explained, for more details please refer to Durre 2010.

### Basic integrity checks

The basic integrity tests include tests which try to detect severe problems with the data. For example the  function `perform_naught_check()` investigates repetitions of false zeros in temperature data. It checks if minimum and maximum daily temperature both are either 0°C or -17.8°C (which are 0°F). In such a case both observations get removed. The naught check did yield positive results for any of the 33 target weather stations.

The second function called `get_duplicated_values()` performs several checks to detect duplicated values. For precipitation data it checks if whole years are duplicated, given a year contains at least three rain events. For temperature and precipitation data the function also checks if either months of the same year are duplicated or if same months of different years contain exactly the same data (in case of precipitation there needs to be at least three rain events for a month to be included in the test). Furthermore, the test checks for months containing at least 10 cases in which minimum and maximum temperature are equal. In case one of the test finds instances of a duplicated month/year, the whole observation of that detected period gets flagged. 

Somehow the function still returns months of Precipitation which have only zeros. Check why.



```r
map(weather_list,~ get_duplicated_values(weather = .x, var = 'Precip') & is.na(.x[,'Precip']) == F) %>%
  map(sum)
```

```
## $cimis_2
## [1] 0
## 
## $cimis_5
## [1] 0
## 
## $cimis_6
## [1] 0
## 
## $cimis_7
## [1] 0
## 
## $cimis_12
## [1] 0
## 
## $cimis_13
## [1] 0
## 
## $cimis_15
## [1] 0
## 
## $cimis_35
## [1] 0
## 
## $cimis_39
## [1] 0
## 
## $cimis_41
## [1] 0
## 
## $cimis_43
## [1] 0
## 
## $cimis_44
## [1] 0
## 
## $cimis_47
## [1] 0
## 
## $cimis_52
## [1] 0
## 
## $cimis_54
## [1] 0
## 
## $cimis_56
## [1] 0
## 
## $cimis_57
## [1] 0
## 
## $cimis_62
## [1] 0
## 
## $cimis_64
## [1] 0
## 
## $cimis_68
## [1] 0
## 
## $cimis_70
## [1] 0
## 
## $cimis_71
## [1] 0
## 
## $cimis_75
## [1] 0
## 
## $cimis_77
## [1] 0
## 
## $cimis_78
## [1] 0
## 
## $cimis_80
## [1] 0
## 
## $cimis_84
## [1] 0
## 
## $cimis_86
## [1] 0
## 
## $cimis_87
## [1] 0
## 
## $cimis_88
## [1] 0
## 
## $cimis_90
## [1] 0
## 
## $cimis_91
## [1] 0
## 
## $cimis_92
## [1] 0
```

```r
which(get_duplicated_values(weather = weather_list[[1]], var = 'Precip') & is.na(weather_list[[1]]$Precip) == F)
```

```
## integer(0)
```

```r
weather[10744:10773, 'Precip']
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```


