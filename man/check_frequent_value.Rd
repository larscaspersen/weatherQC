% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_frequent_value.R
\name{check_frequent_value}
\alias{check_frequent_value}
\title{Check for duplicated values in precipitation}
\usage{
check_frequent_value(weather, percentile_df, min_non_zero_days = 20)
}
\arguments{
\item{weather}{data.frame containing a daily time series data set. 
It should have columns c("Year", "Month", "Day")}

\item{percentile_df}{data.frame containing the climatological precipitation
percentiles for the corresponding day of the year}

\item{min_non_zero_days}{minimum amount of non-zero precipitation observation need 
to be present for ecdf calculation, otherwise NA returned}
}
\value{
logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
meaning that the tested variable exceeded the limits of the test and is flagged
as suspicious. Data which was dropped in the process of the analysis receives
automatically a FALSE
}
\description{
The function screens for duplicated precipitation values within a sliding
10 day window.
}
\details{
This is the corresponding version of \code{\link{get_streaks}} for precipitation.
The function drops all NA and zero-precipitation observation from the test and
uses the remaining data. This means a sequence like c(10, 0, 5, NA, 0, 5) will 
be evaluates as c(10, 5, 5) in the test. Within a sliding 10-day window frequent
values are screened for. If the frequently duplicated precipitation value is
repeated 5 or more times within that time-window it will be compared to a 
climatological precipitation percentile for the first day of the time window.
The more repetitions found, the lower the compared precipitation percentile will be.
If the frequently repeated value exceeds the percentile, each of the repeated
precipitation values will be flagged.

For 5 or 6 repetitions the 90\% percentile will be used, for 7 the 70\% percentile,
for 8 repetitions the 50\% percentile and for 9 or 10 repetitions the 30\% percentile.

Climatological precipitation percentiles are calculated for each day of the year,
using a 29-day window centered at the day of interest. All precipitation observation
throughout the recorded years lying within that time window will be included in
the percentile calculation. Missing values and zero-precipitation observations
will be discarded. By default there need to be at least 20 non-zero observation
for the percentile calculation, otherwise there will be NA instead of percentiles.

This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
please refer to section 3 "Basic integrity tests".
}
\examples{
percentile_df <- get_each_day_precipitation_percentile(weather = target_weather)
check_frequent_value(weather = target_weather, percentile_df = percentile_df)
}
\references{
\insertAllCited{}
}
\author{
Lars Caspersen, \email{lars.caspersen@uni-bonn.de}
}
