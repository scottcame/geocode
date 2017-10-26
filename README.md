### Overview

The `geocode` package provides a facade to three freely-available but limited geocoding services:  US Census, Nominatim (Open Street Map), Google, and ArcGIS's GeocodeServer.

Each of these geocoding services is useful in its own right, but the goal of this package is to provide a seamless interface to all of them, and to use them in combination.  The Nominatim and Google services
have usage limits, but allow for richer and more varied queries than the US Census geocoder.  A common use case is to try the US Census geocoder first, and if it doesn't succeed, then try the others.  This
package implements this use case. It executes Census, Nominatim, and Google geocoders, in that order, by default. The user can specify the specific geocoders to try, and the order, as an optional
parameter to the `geocode` function.

This package does not attempt to prevent the user from exceeding the rate/usage limits for ArcGIS, [Google](https://developers.google.com/maps/documentation/geocoding/usage-limits) and 
[Nominatim](https://operations.osmfoundation.org/policies/nominatim/).  The `geocodeNominatim()` function takes an optional parameter, `nominatimServiceURL`, that directs the client to an alternative
instance of Nominatim (e.g., one running locally or on a private network). Setting up Nominatim can be a bit of a chore, but I have made it a bit easier with [Docker](https://www.docker.com/) 
[images](https://github.com/scottcame/docker/tree/master/nominatim).


### Installation

This package is not currently available on CRAN.  Install directly from github:

``` r
# install.packages("devtools")
devtools::install_github("scottcame/geocode")
```

### Usage

There are two usage modes available through the package:

* [Non-batch (address by address)](#non-batch-usage)
* [Census Batch](#census-batch-usage)

#### Non-Batch Usage

Default usage, attempts Census first, then Nominatim, then Google:

```
> library(geocode)
> stadiums <- c(safeco='1250 1st Avenue South, Seattle, WA 98134', progressive='2401 Ontario St, Cleveland, OH 44115', yankee='Yankee Stadium')
> geocode(stadiums)
# A tibble: 3 x 11
     Number     Street      City    State   Zip Latitude  Longitude                             InputAddress Approximate    Source SourceIndex
      <chr>      <chr>     <chr>    <chr> <chr>    <dbl>      <dbl>                                    <chr>       <lgl>     <chr>       <int>
1 1100-1298  1ST AVE S   SEATTLE       WA 98134 47.59090 -122.33419 1250 1st Avenue South, Seattle, WA 98134       FALSE    Census           1
2 2401-2599 ONTARIO ST CLEVELAND       OH 44115 41.49500  -81.68713     2401 Ontario St, Cleveland, OH 44115       FALSE    Census           2
3         1       <NA>       NYC New York 10451 40.82958  -73.92652                           Yankee Stadium       FALSE Nominatim           3
>
```

Or just go straight to Google:

```
> geocode(stadiums, geocoders='Google')
# A tibble: 3 x 11
  Number           Street      City State   Zip Latitude  Longitude Source Approximate                             InputAddress SourceIndex
   <chr>            <chr>     <chr> <chr> <chr>    <dbl>      <dbl>  <chr>       <lgl>                                    <chr>       <int>
1   1250 1st Avenue South   Seattle    WA 98134 47.59163 -122.33326 Google       FALSE 1250 1st Avenue South, Seattle, WA 98134           1
2   2401       Ontario St Cleveland    OH 44115 41.49570  -81.68527 Google       FALSE     2401 Ontario St, Cleveland, OH 44115           2
3      1       E 161st St     Bronx    NY 10451 40.82964  -73.92617 Google       FALSE                           Yankee Stadium           3
>
```

An unresolvable address:

```
> geocode('A non-existent address, Chicago, IL')
# A tibble: 0 x 0
>
```

Centroid of a jurisdiction (state, county, etc.):

```
> geocode('Kennebec County, ME')
# A tibble: 1 x 11
  Number Street  City State   Zip Latitude Longitude        InputAddress Approximate    Source SourceIndex
   <chr>  <chr> <chr> <chr> <chr>    <dbl>     <dbl>               <chr>       <lgl>     <chr>       <int>
1   <NA>   <NA>  <NA> Maine  <NA> 44.41846 -69.82507 Kennebec County, ME        TRUE Nominatim           1
>
```

Handling of nulls in input and invalid addresses (note `SourceIndex` column):

```
> geocode(c('1250 1st Avenue South, Seattle, WA 98134', NA_character_, 'A non-existent address, Chicago, IL', 'Yankee Stadium'))
# A tibble: 2 x 11
     Number    Street    City    State   Zip Latitude  Longitude                             InputAddress Approximate    Source SourceIndex
      <chr>     <chr>   <chr>    <chr> <chr>    <dbl>      <dbl>                                    <chr>       <lgl>     <chr>       <int>
1 1100-1298 1ST AVE S SEATTLE       WA 98134 47.59090 -122.33419 1250 1st Avenue South, Seattle, WA 98134       FALSE    Census           1
2         1      <NA>     NYC New York 10451 40.82958  -73.92652                           Yankee Stadium       FALSE Nominatim           4
>
```

Handling of duplicates in the input vector:

```
> geocode(rep('1250 1st Avenue South, Seattle, WA 98134', 2))
# A tibble: 2 x 11
     Number    Street    City State   Zip Latitude Longitude                             InputAddress Approximate Source SourceIndex
      <chr>     <chr>   <chr> <chr> <chr>    <dbl>     <dbl>                                    <chr>       <lgl>  <chr>       <int>
1 1100-1298 1ST AVE S SEATTLE    WA 98134  47.5909 -122.3342 1250 1st Avenue South, Seattle, WA 98134       FALSE Census           1
2 1100-1298 1ST AVE S SEATTLE    WA 98134  47.5909 -122.3342 1250 1st Avenue South, Seattle, WA 98134       FALSE Census           2
>
```
#### Census Batch Usage

The package provides an interface to the US Census Bureau's [batch geocoding service](https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form).
It automatically partitions the input data into chunks of the appropriate size (currently 999 records) and submits them via HTTP POST.

Example:

```
> geocodeCensusBatch(
+     c('520 4th Ave E', '4550 Wyoga Lake Rd'),
+     c('Olympia', 'Cuyahoga Falls'),
+     c('WA', 'OH'),
+     c('98501', NA_character_))
Processing batch 1 of 1
Batch had 2 successful geocodes
# A tibble: 2 x 11
  SourceIndex                            InputAddress Match Status                                     Address  Longitude Latitude TIGERLineID  Side      Source Approximate
        <int>                                   <chr> <chr>  <chr>                                       <chr>      <dbl>    <dbl>       <int> <chr>       <chr>       <lgl>
1           2 4550 Wyoga Lake Rd, Cuyahoga Falls, OH, Match  Exact 4550 WYOGA LAKE RD, CUYAHOGA FLS, OH, 44224  -81.49443 41.19390    61393964     L CensusBatch       FALSE
2           1       520 4th Ave E, Olympia, WA, 98501 Match  Exact           520 4TH AVE E, OLYMPIA, WA, 98501 -122.89642 47.04527   180944221     L CensusBatch       FALSE
>
```


