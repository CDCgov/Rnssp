# Introduction to the Rnssp package

## Introduction

The goal of `Rnssp` package is to facilitate the access to the
Electronic Surveillance System for the Early Notification of
Community-based Epidemics (`ESSENCE`) via a secure and simplified
interface. In addition, `Rnssp` provides methods and functions that
streamline the data pull by abstracting the complexity of the R codes
from the users.

In this vignette, we explained how to create a user profile, and provide
various examples to how to use it to pull data from NSSP-ESSENCE using
the following ESSENCE APIs:

1.  Time series data table,
2.  Time series png image,
3.  Table builder results,
4.  Data details (line level),
5.  Summary stats
6.  Alert list detection table
7.  Time series data table with stratified, historical alerts (from
    ESSENCE2)

## Creating an NSSP user profile

We start by loading the `Rnssp` package.

``` r
# Loading useful R packages...
library(Rnssp)
```

The next step is to create a user profile with NSSP-ESSENCE user
credentials (username and password). The created user profile is an
object of class `Credentials` class which has been designed with the R6
system. R6 integrates classical Object Oriented Programming concepts
into R. Here, we use the
[`create_profile()`](https://cdcgov.github.io/Rnssp/reference/create_profile.md)
function to create a user profile.

``` r
# Creating an ESSENCE user profile
myProfile <- create_profile()

# save profile object to file for future use
# save(myProfile, file = "myProfile.rda") # saveRDS(myProfile, "myProfile.rds")
# Load profile object
# load("myProfile.rda") # myProfile <- readRDS("myProfile.rds")
```

The above code needs to be executed only once. Upon execution, it
prompts the user to provide his username and password.

Alternatively, the `myProfile` object can be saved on file as an
`.RData` (`.rda`) or `.rds` file and loaded to automate Rmarkdown
reports generation.

Be assured that your password is highly protected and completely
insulated from being accessible in plain text. In fact, both password
and username are fully encrypted, even the encrypted texts are not
available for display as shown (below) when the `myProfile` object is
inspected.

``` r
# Inspecting the myProfile object
myProfile
#> <NSSPCredentials>
#>   Inherits from: <NSSPAuth>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     get_api_data: function (url, fromCSV = FALSE, ...) 
#>     get_api_graph: function (url, file_ext = ".png") 
#>     get_api_response: function (url) 
#>     initialize: function (username = NULL, password = NULL) 
#>   Private:
#>     ..__: NSSPContainer, R6
#>     ..password: NSSPContainer, R6
#>     ..username: NSSPContainer, R6
```

``` r
# Alternatively, you may look at the structure of the myProfile object
str(myProfile) # or `structure(myProfile)`
#> Classes 'NSSPCredentials', 'NSSPAuth', 'R6' <NSSPCredentials>
#>   Inherits from: <NSSPAuth>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     get_api_data: function (url, fromCSV = FALSE, ...) 
#>     get_api_graph: function (url, file_ext = ".png") 
#>     get_api_response: function (url) 
#>     initialize: function (username = NULL, password = NULL) 
#>   Private:
#>     ..__: NSSPContainer, R6
#>     ..password: NSSPContainer, R6
#>     ..username: NSSPContainer, R6
```

The created `myProfile` object comes with the `$get_api_response()`,
`$get_api_data()`, `$get_api_graph()` and `$get_api_tsgraph()` methods
with various parameters to pull ESSENCE data. Alternatively, the
[`get_api_response()`](https://cdcgov.github.io/Rnssp/reference/get_api_response.md),
[`get_api_data()`](https://cdcgov.github.io/Rnssp/reference/get_api_data.md),
and
[`get_api_graph()`](https://cdcgov.github.io/Rnssp/reference/get_api_graph.md)
functions serve as wrappers to their respective methods.

For a full documentation on all the methods available, execute the
[`help(Credentials)`](https://cdcgov.github.io/Rnssp/reference/Credentials.md)
or
[`?Credentials`](https://cdcgov.github.io/Rnssp/reference/Credentials.md)
command in the R console. Similarly, execute `?get_api_response()`,
`?get_api_data()`, or `?get_api_graph()` for a full documentation on
each function.

In the following sections, we show how to pull data from NSSP-ESSENCE
using the seven APIs listed above.

## Time Series Data Table

``` r
# URL from ESSENCE JSON Time Series Data Table API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=9Feb2021&medicalGrouping=injury&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=11Nov2020&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TimeSeries"

# pull time series data
api_data_ts <- get_api_data(url) # or api_data_ts <- myProfile$get_api_data(url)

class(api_data_ts)
#> [1] "list"
names(api_data_ts)
#> [1] "title"          "timeSeriesData"
# glimpse(api_data_ts$timeSeriesData)
```

Alternatively, the example below with the
[`get_api_response()`](https://cdcgov.github.io/Rnssp/reference/get_api_response.md)
function achieves the same outcome as previously (*we recommend using
this for debugging purposes*):

``` r
# URL from ESSENCE JSON Time Series Data Table API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=9Feb2021&medicalGrouping=injury&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=11Nov2020&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TimeSeries"

# pull time series data
api_response <- get_api_response(url) 

api_response_content <- httr::content(api_response, as = "text")
api_data_ts2 <- jsonlite::fromJSON(api_response_content)

class(api_data_ts2)
names(api_data_ts2$timeSeriesData)

# glimpse(api_data_ts2$timeSeriesData)
```

Although, we present the alternate code above, the following sections
will only demonstrate the direct approach using the
[`get_api_data()`](https://cdcgov.github.io/Rnssp/reference/get_api_data.md)
function The alternate code can nevertheless be easily adapted to the
other types of ESSENCE API data pulls.

## Time Series Graph from ESSENCE

The example below shows how to retrieve the Time Series Graph from
NSSP-ESSENCE and insert it in the Rmarkdown report.

``` r
# URL from ESSENCE Time Series Graph from ESSENCE API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries/graph?endDate=9Feb2021&medicalGrouping=injury&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=11Nov2020&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TimeSeries&graphTitle=National%20-%20Injury%20Syndrome%20Daily%20Counts&xAxisLabel=Date&yAxisLabel=Count"

# Data pull from ESSENCE
api_data_graph <- get_api_graph(url)

# Inspecting api_data_graph object
class(api_data_graph)
#> [1] "list"
names(api_data_graph)
#> [1] "api_response" "graph"

# Inserting graph in the report
knitr::include_graphics(api_data_graph$graph)
```

## Table Builder Results

The CSV option of the Table Builder Results API pulls in data in the
tabular format seen on NSSP-ESSENCE. The JSON option on the other hand,
pulls in the data in a long, pivoted format. In the following
subsections, we demonstrate how to pull the Table Builder results data
with both options.

### CSV option

``` r
# URL from CSV ESSENCE Table Builder results API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate=31Dec2020&ccddCategory=cdc%20opioid%20overdose%20v3&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=nodetectordetector&startDate=1Oct2020&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&ageNCHS=unknown&timeResolution=monthly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=geographyhospitaldhhsregion&columnField=ageNCHS"

# Data Pull from ESSENCE
api_data_tb_csv <- get_api_data(url, fromCSV = TRUE)

# Inspecting api_data_graph object
class(api_data_tb_csv) 
#> [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

# glimpse(api_data_tb_csv)
```

### JSON option

``` r
# URL from JSON ESSENCE Table Builder results API
url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/tableBuilder?endDate=31Dec2020&ccddCategory=cdc%20opioid%20overdose%20v3&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=nodetectordetector&startDate=1Oct2020&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&ageNCHS=unknown&timeResolution=monthly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=geographyhospitaldhhsregion&columnField=ageNCHS"

# Data Pull from ESSENCE
api_data_tb_json <- get_api_data(url)

class(api_data_tb_json)
#> [1] "data.frame"
names(api_data_tb_json)
#> [1] "timeResolution"              "geographyhospitaldhhsregion"
#> [3] "ageNCHS"                     "count"

# glimpse(api_data_tb_json)
```

## Data Details (line level)

Similarly to the Table builder Results API, the Data Details (line
level) provides CSV and JSON data outputs.

### CSV option

``` r
# URL from CSV ESSENCE Data Details (line level) API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?medicalGrouping=injury&geography=region%20i&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TimeSeries&startDate=31Jan2021&endDate=31Jan2021"

# Data Pull from ESSENCE
api_data_dd_csv <- get_api_data(url, fromCSV = TRUE)

class(api_data_dd_csv)  
#> [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

# glimpse(api_data_dd_csv)
```

### JSON option

``` r
# URL from JSON ESSENCE Data Details (line level) API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Jan2021&medicalGrouping=injury&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=31Jan2021&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=DataDetails"

# Data Pull from ESSENCE
api_data_dd_json <- get_api_data(url)

class(api_data_dd_json)
#> [1] "list"
names(api_data_dd_json)
#> [1] "dataDetails"

# glimpse(api_data_dd_json$dataDetails)
```

## Summary Stats

``` r
# URL from JSON ESSENCE Summary Stats API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/summaryData?endDate=31Jan2021&medicalGrouping=injury&geography=region%20i&percentParam=noPercent&geographySystem=hospitaldhhsregion&datasource=va_hosp&detector=probrepswitch&startDate=29Jan2021&timeResolution=daily&medicalGroupingSystem=essencesyndromes&userId=455&aqtTarget=TimeSeries"

# Data Pull from ESSENCE
api_data_ss <- get_api_data(url)

class(api_data_ss)
#> [1] "list"
names(api_data_ss)
#> [1] "summaryData"

# glimpse(api_data_ss$summaryData)
```

## Alert List Detection Table

Since the Alert List API provides programmatic access to the Alert List
table on the ESSENCE user interface by patient region or by hospital
regions, we provide two use cases of data pull in the following
subsections:

### Alert List Detection Table by Patient Region

``` r
# URL from JSON ESSENCE Alert List Detection Table API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/regionSyndromeAlerts?end_date=31Jan2021&start_date=29Jan2021"

# Data Pull from ESSENCE
api_data_alr <- get_api_data(url)

class(api_data_alr)
#> [1] "list"
names(api_data_alr)
#> [1] "regionSyndromeAlerts"

# glimpse(api_data_alr$regionSyndromeAlerts)
```

### Alert List Detection Table by Hospital Region

``` r
# URL from JSON ESSENCE Alert List Detection Table API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?end_date=31Jan2021&start_date=29Jan2021"

# Data Pull from ESSENCE
api_data_alh <- get_api_data(url)

class(api_data_alh)
#> [1] "list"
names(api_data_alh)
#> [1] "hospitalSyndromeAlerts"

# glimpse(api_data_alh$hospitalSyndromeAlerts)
```

## Time series data table with stratified, historical alerts (from ESSENCE2)

This functionality as of November 03, 2026 is available from ESSENCE2.
Therefore, if your ESSENCE 2 credentials are different from the one you
define for ESSENCE above, you will have to recreate another profile
object for ESSENCE 2 and use it to run the code below. In this example,
it is assumed that the same user profile has been used for both ESSENCE
and ESSENCE 2.

``` r
# URL from JSON ESSENCE Time series data table with stratified, historical alerts API
url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=9Feb2021&ccddCategory=cdc%20pneumonia%20ccdd%20v1&ccddCategory=cdc%20coronavirus-dd%20v1&ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&percentParam=ccddCategory&geographySystem=hospitaldhhsregion&datasource=va_hospdreg&detector=probrepswitch&startDate=11Nov2020&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TimeSeries&stratVal=ccddCategory&multiStratVal=geography&graphOnly=true&numSeries=3&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=ccddCategory&multiStratVal=geography&graphOnly=true&numSeries=3&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false"

# Data Pull from ESSENCE
api_data_tssh <- get_api_data(url)

class(api_data_tssh)
#> [1] "list"
names(api_data_tssh)
#> [1] "timeSeriesData"

# glimpse(api_data_tssh$timeSeriesData)
```
