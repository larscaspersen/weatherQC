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

Working on a data set of daily precipitation and temperature measurement in north-eastern Brazil, the authors proposed a set of 6 plausibility checks to detect suspicious data. The tests included checks on the meta data, fixed limit test, variable limit tests, consistency among variables, temporal consistency and spatial consistency tests. The main idea is, that a measurement needs to be flagged by at least two of these tests in order to be removed from the data set. This should reduced the amount of false positive rates. 
The concept of the Costa 2021 quality control tests has been written as an R function called `weather_qc_costa()`. It takes `weather`, `weather_coords`, `var`, `aux_list`, `aux_info`, `level` and `country` as mandatory inputs and returns a boolean vector of the same length as days rows in weather as an output, indicating suspicious data with TRUE and data save to keep with FALSE.
`weather` is a dataframe, organized with the variables in the columns and daily measurement in the rows. The quality control checks of Costa 2021 were described with a data set containing minimum and maximum temperature, daily precipitation, relative humidity, atmospheric pressure, wind speed and insulation. However, in its current state it was written to satisfy the needs of the other functions of chillR, which only work with daily extreme temperatures and to a lesser extend with precipitation. Some of the tests require additionally the daily average temperature, so it should be supplied alongside to daily minimum and maximum temperature. Additionally, the dataframe needs to contain columns called Day, Month and Year. An example for weather can be seen in the following.


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
weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                        format = '%Y-%m-%d')

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

Most of the tests require the weather data.frames to contain a column called Date and doy (day of the year). Before running the test, we make sure that these columns are present.


```r
weather_list <- map(weather_list, function(x){
  x %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep = '-'), format = '%Y-%m-%d'),
           doy = lubridate::yday(Date))
})
```



### Basic integrity checks

The basic integrity tests include tests which try to detect severe problems with the data. For example the  function `perform_naught_check()` investigates repetitions of false zeros in temperature data. It checks if minimum and maximum daily temperature both are either 0°C or -17.8°C (which are 0°F). In such a case both observations get removed. The naught check did yield positive results for any of the 33 target weather stations.

The second function called `get_duplicated_values()` performs several checks to detect duplicated values. For precipitation data it checks if whole years are duplicated, given a year contains at least three rain events. For temperature and precipitation data the function also checks if either months of the same year are duplicated or if same months of different years contain exactly the same data (in case of precipitation there needs to be at least three rain events for a month to be included in the test). Furthermore, the test checks for months containing at least 10 cases in which minimum and maximum temperature are equal. In case one of the test finds instances of a duplicated month/year, the whole observation of that detected period gets flagged. The `get_duplicated_values()` function does not return any flags for non_NA observations when applied to Tmin, Tmax or Precip. However, it returns flags for some months which have no observations at all. This behavior should be changed, because the flags are in these cases meaningless and potentially misleading.

Next comes the record exceedance test. It is implemented as described in the Costa 2021 quality control algorithm. The function `fixed_limit_test()` allows for the retrieval of country-specific temperature records and in the case of US-States also precipitation records and then flags observations outside the record range. This deviates from the description of Durre 2010, who used global temperature and precipitation records for the test.


Next comes the identical value streak test, which only applies to minimum and maximum temperature. It is carried out using the `get_streaks()` function. It tests if 20 or more subsequent observations of a variable have the exactly the same value. Missing values are skipped when evaluating for streaks. In such a case all values belonging to the streak are flagged. No cases of identical streak were detected for the CIMIS weather station data set.


```r
map(weather_list,~ get_streaks(weather = .x, variable = 'Tmax')) %>%
  map_dbl(sum)
```

```
##  cimis_2  cimis_5  cimis_6  cimis_7 cimis_12 cimis_13 cimis_15 cimis_35 
##        0        0        0        0        0        0        0        0 
## cimis_39 cimis_41 cimis_43 cimis_44 cimis_47 cimis_52 cimis_54 cimis_56 
##        0        0        0        0        0        0        0        0 
## cimis_57 cimis_62 cimis_64 cimis_68 cimis_70 cimis_71 cimis_75 cimis_77 
##        0        0        0        0        0        0        0        0 
## cimis_78 cimis_80 cimis_84 cimis_86 cimis_87 cimis_88 cimis_90 cimis_91 
##        0        0        0        0        0        0        0        0 
## cimis_92 
##        0
```

```r
map(weather_list,~ get_streaks(weather = .x, variable = 'Tmin')) %>%
  map_dbl(sum)
```

```
##  cimis_2  cimis_5  cimis_6  cimis_7 cimis_12 cimis_13 cimis_15 cimis_35 
##        0        0        0        0        0        0        0        0 
## cimis_39 cimis_41 cimis_43 cimis_44 cimis_47 cimis_52 cimis_54 cimis_56 
##        0        0        0        0        0        0        0        0 
## cimis_57 cimis_62 cimis_64 cimis_68 cimis_70 cimis_71 cimis_75 cimis_77 
##        0        0        0        0        0        0        0        0 
## cimis_78 cimis_80 cimis_84 cimis_86 cimis_87 cimis_88 cimis_90 cimis_91 
##        0        0        0        0        0        0        0        0 
## cimis_92 
##        0
```

A similar test is also available for precipitation, called `frequent_value_check()`. Before the test is run, the percentiles for each day of the year is calculated using the function `get_each_day_precipitation_percentile()`. Precipitation percentiles for each day of the year is calculated using a 29-day window centered at the day of interest. All non-zero precipitation observations thorughout the observation period lying in that observation-window are used to calculate the percentiles.
The `frequent_value_check()` test ignores missing observation or zero-preciptiation observations. For the remaining data it checks for a 10 day window, if five or more identical precipitation obserations can be found. Given the frequency of the repeated value, it is then checked if the repeated value exceeds a certain climatological precipitation percentile for that day of the year, which was calculated before. The more often the suspected values is repeated, the lower the testing threshold is. For 9 - 10 identical repeated values the threshold is the 30% percentiles, for 8 repeated values the 50% percentile, for 7 the 70% percentile and for 5-6 repeated values the 90% percentile. 


```r
  #calculate percentiles for each weather df, store in list
  prec_percentile_list <- map(weather_list, get_each_day_precipitation_percentile)

  #carry out test
 map2(weather_list, prec_percentile_list, function(x,y){
   frequent_value_check(weather = x, percentile_df = y )}) %>%
   map_dbl(sum)
```

```
##  cimis_2  cimis_5  cimis_6  cimis_7 cimis_12 cimis_13 cimis_15 cimis_35 
##        0       97       17      109       70        0        0        0 
## cimis_39 cimis_41 cimis_43 cimis_44 cimis_47 cimis_52 cimis_54 cimis_56 
##       57       38       14       14       26        0       44        9 
## cimis_57 cimis_62 cimis_64 cimis_68 cimis_70 cimis_71 cimis_75 cimis_77 
##        9       36       32       32        0      117        0       28 
## cimis_78 cimis_80 cimis_84 cimis_86 cimis_87 cimis_88 cimis_90 cimis_91 
##       60       88        0       68        0        0        0       50 
## cimis_92 
##        0
```
It seems there are several positive cases for several stations. Let's have a closer look at the test results of the second weatherstation.


```r
 #check for second stations which ones were marked
 weather_list[[2]] %>%
   mutate(flag =  frequent_value_check(weather = ., percentile_df =  prec_percentile_list[[2]]),
          Date = as.Date(paste(Year, Month, Day, sep = '-'), format = '%Y-%m-%d')) %>%
   filter(flag == T) %>%
   select(Date, Tmin, Tmax, Precip, QC_Precip) %>%
   head()
```

```
##         Date Tmin Tmax Precip QC_Precip
## 1 1995-12-26  0.1 15.4    0.3         R
## 2 1995-12-29  3.4 14.9    0.3         R
## 3 1996-01-02  3.5 15.4    0.3         R
## 4 1996-01-03  2.9 22.4    0.3         R
## 5 1996-01-07  1.9 17.2    0.3         R
## 6 1996-01-08  3.3 10.5    0.3         R
```

It is suspicious that there is so often the exact same observation of precipitation for these days, Furthermore, the quality flag indicates that these observations are also far outside the ususal ones for these days. So this can be probably a error in the measurement.


### Outlier checks

In the next section the daily observations are compared to the longterm observation for the same variable. 

At first the so-called gap-test is carried out using the `perform_gap_check()` function. For each month of the year, the observations are tested independently. The test evaluates the ordered observations for gaps larger than 10°C in case of temperature and 300mm for precipitation. In case of temperature the search for gaps start at the median and goes to the tails of the distribution. In case of a gap, each observation to the tail side of the distribution is flagged. The search for gaps in precipitation starts at the first non-zero observation and includes only the upper tail of the distribution.


```r
 #check for second stations which ones were marked
 map(weather_list, ~perform_gap_check(weather = .x, variable = 'Tmin')) %>%
  map_dbl(sum)
```

```
##  cimis_2  cimis_5  cimis_6  cimis_7 cimis_12 cimis_13 cimis_15 cimis_35 
##       27        1        1        1        0        0        3        6 
## cimis_39 cimis_41 cimis_43 cimis_44 cimis_47 cimis_52 cimis_54 cimis_56 
##        0        3        1        4        1       22        7        2 
## cimis_57 cimis_62 cimis_64 cimis_68 cimis_70 cimis_71 cimis_75 cimis_77 
##        0       17        2       12       55       46        4       20 
## cimis_78 cimis_80 cimis_84 cimis_86 cimis_87 cimis_88 cimis_90 cimis_91 
##       24       15       27       19       15       18        1       23 
## cimis_92 
##        0
```

As can be seen the function detects for several weather stations outliers. Let's have a look at the flagged values.


```r
 #check for second stations which ones were marked
perform_gap_check(weather = weather_list[[1]], variable = 'Tmin') %>%
  weather_list[[1]][.,] %>%
  select(Date, Tmin, Tmax, QC_Tmin)
```

```
##            Date    Tmin   Tmax QC_Tmin
## 86   1990-03-27   -35.9   27.6       R
## 177  1990-06-26  -134.0   79.7       S
## 182  1990-07-01    51.4   85.5       R
## 183  1990-07-02    84.9   85.5       R
## 184  1990-07-03  -134.0 2342.0       S
## 186  1990-07-05   -48.8   38.7       R
## 348  1990-12-14    36.1   85.7       R
## 349  1990-12-15    38.7   85.7       R
## 357  1990-12-23   -53.7    8.7       S
## 359  1990-12-25    33.1   51.3       R
## 360  1990-12-26    23.5   44.6       *
## 361  1990-12-27  -118.0 2139.0       S
## 732  1992-01-02 -6999.0   81.4       S
## 733  1992-01-03 -6999.0   80.2       S
## 734  1992-01-04 -6999.0   81.8       S
## 736  1992-01-06 -6999.0   80.8       S
## 745  1992-01-15    40.7   66.2       R
## 746  1992-01-16    40.1   67.0       R
## 781  1992-02-20 -6999.0   17.6       S
## 7177 2009-08-25   -42.2   35.3       S
## 7295 2009-12-21   -34.4   10.5       S
## 7307 2010-01-02   -32.9    0.3       S
## 7308 2010-01-03   -24.6   11.9       S
## 7309 2010-01-04   -19.1    3.5       S
## 7313 2010-01-08   -23.3    6.5       S
## 7314 2010-01-09   -39.7   -1.5       S
## 7316 2010-01-11   -39.8    9.7       S
```

It seems that all of the flagged data are also rightly flagged. Observations detected by a flag are (temporary) removed from the weather data.frame, so that they do not deteriorate the subsequent tests. This is done by the helper function `clear_flagged_data()`, which removes the observation in the original column and adds a note in the quality control flag which test is responsible for the removal. In the end the `durre_quality_control` function returns the list of weather data.frames with two additional, columns for each tested variable: one witht the original observations called `org_Tmin` in the example of minimum temperature and one further column indicating which test flagged the observation called `flag_Tmin` in the example of Tmin. An example how the function is called can be seen in the following. This is usually done inside the `Durre_quality_control()` function.


```r
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_gap_check(weather = x, 
                                                 variable = 'Tmin'), 
                       test_name = 'gap_check')
  })
```


The next test called `perform_climate_outlier_check()` evaluates, as the name already indicates, climatological outlier. The routines for temperature and precipitation data are different. For temperature data at first the long term mean and standard deviation for each day of the year is calculated. This is done using a 15-day window centered at the day of interest and using each observation in that time window throughout all observation years. There need to be at least 100 observation for the long term mean and standard deviation, otherwise the function will return only NA. In the next step the standardized residuals for each day to to long term mean of that day of the year are calculated. Normalization involves subtracting the long term mean from the observation and dividing by the standard deviation. Finally if the absolute value of standardized residuals is larger than 6°C, the value is flagged as outlier. 
In case of precipitation the 95% percentile is used instead of long term mean and standard deviation. It is calculated in the same manner as in the frequent values test. A 29-day window centered at the day of interested is used, missing values and zero-precipitation observations were ignored. There need to be at least 20 valid observation in the time-window for the percentile calculation. In case of above zero temperature, precipitation values larger than 9 times the 95% percentile for the day of interest are flagged. In freezing conditions the threshold is lowered to 5 times the 95% percentile.


Use the final results instead of the function call to check how many days are flagged.


### Temporal consistency

The next tests investigate the temporal integrity of the weather data. The iterative temperature consistency test called `quickker_iterat_consistency()` checks if minimum, maximum and mean temperature are line with another. This involves that minim temperature should not be larger than mean and maximum temperature at the same day, but also at the current day compared to the following day. There are in total 7 seven plausibility checks done for each day, for more details please refer to the Appendix A of Durre 2010. For each variable at each day the amount of positive tests (called violations) are summarized. The observations having the most violations are removed. Then test is run again until no more violations are detected. The iterative nature of the test should prevent excessive flagging and prevent that valid observations are 'dragged down' by faulty neighbours. For full potential of the test, also mean temperature observations are required. 

test results

Next comes the spike/dip test, which is run using `do_spike_dip_test()`. The test detects rapid day-to-day changes. If the absolute difference of a observation at day 0 to day -1 is larger than 25°C followed by an absolute change in the same variable to day +1 by also 25°C the observaiton at day 0 is flagged. This means that one spike or dip is not enough, there needs to be both.

Lagged temperature range test. Never fully understood. Still needs to be explained.

test results

There were further precipitation consistency tests described by Durre 2010, however these involved the consistency between precipitation and snow data. Because snow data is not considered in this implementation of the quality control scheme, they were discarded.

### Spatial consistency test

The next section is about the spatial consistency of the observations. The first test in this section is a regression check of temperature observation called `spatial_consistency_test()`. At first only neighbouring stations within a 75km radius around the target station were considered. Using a three day window centered on the day of interest, a weighted mean of the pairwise regression of target and neighbour values is calculated. Target neighbour regressions needed to have correlation coefficient of 0.8 or larger in order to be considered. At least three, but never more than seven neighbouriung stations were used for the regression. Mean value of the regression is calculated based on the index of agreement. Regressions are done for each year / month independently. In order to be flagged, the residual and the standardized residual must exceed a threshold. (>= 4 and 8, respectively).

Results

The spatial regression has relatively high quality demands on the target - neighbour station data quality. In cases were the requirements were not met, the second test can fill a gap. The corrobation test of temperature `perform_temperature_corrobation_check()` and precipitation `precipitation_spatial_corrobation_test()` tests if the smallest difference in target - neighbour observation exceeds a certain threshold. For the lowest absolute difference calculation a three day window centered on the day of interested is used for the neighbouring station. That means if there are seven neighbpour stations having for each of the three day window a valid observaiton, the target observation of day 0 is compared to 21 neighbouring observations. The three-day time window should account for different measurement protocols. If the smallest absolute difference is larger or equal to 10°C, then the target observation is flagged. In case of precipitation, the absolute difference accompanied by the difference in climatological percentiles. Climatological percentiles are calculated following the same protocoll described for the climatological outliers. The difference is, that this time the percentile of the values of interest are caclulated and not compared to a fixed threshold. The smalles absolute difference in climatololoigical percentiles is then used to determine a testing threshold for the smalles absolute difference in precipitation. For more details please refer to the Appendix C of Durre 2010. 

test results

### Megaconsistency test

In the end of the quality control scheme some final consistency test, labelled by Durre 2010 as megaconsistency tests are carried out. These involve again mostly plausibility on snow data (for example only snow in months were snowfall can be expected, or that snow occured in months were the lowest Tmin was larger or equal than 7°C). In case of temperature one test is carried out, called `temperature_mega_consistency_check()`. The test checks that the current Tmax is not smaller than the lowest Tmin of the month and that the current Tmin is not larger than the highest Tmax of the month. This is necissary, because the consistency tests demanding that Tmin is smaller than Tmax only work if there are both observations available for a day. For days with a 'missin partner', erroneous observation could slipe through the test and are intended to be catched with the megaconsistency test.

test results

