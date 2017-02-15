Overview
--------

The `geocode` package provides a facade to three freely-available but limited geocoding services:  US Census, Nominatim (Open Street Map), and Google (via the `ggmap` R package).

Each of these geocoding services is useful in its own right, but the goal of this package is to provide a seamless interface to all of them, and to use them in combination.  The Nominatim and Google services
have usage limits, but allow for richer and more varied queries than the US Census geocoder.  A common use case is to try the US Census geocoder first, and if it doesn't succeed, then try the others.  This
package implements this use case.  It also supports persistent caching of results (in a serialized data frame) so repeated requests for geocoding the same address do not need to hit the services online.

Installation
------------

This package is not currently available on CRAN.  Install directly from github:

``` r
# install.packages("devtools")
devtools::install_github("scottcame/geocode")
```

Usage
-----

Default usage, attempts Census first, then Nominatim, then Google:

```
> library(geocode)
> stadiums <- c(safeco='1250 1st Avenue South, Seattle, WA 98134', progressive='2401 Ontario St, Cleveland, OH 44115', yankee='Yankee Stadium')
> geocode(stadiums)
     Number           Street      City      State   Zip Latitude  Longitude                             InputAddress    Source
1 2401-2599       ONTARIO ST CLEVELAND         OH 44115 41.49500  -81.68713     2401 Ontario St, Cleveland, OH 44115    Census
2      1250 1st Avenue South   Seattle Washington 98134 47.59185 -122.33396 1250 1st Avenue South, Seattle, WA 98134 Nominatim
3         1             <NA>       NYC   New York 10451 40.82958  -73.92652                           Yankee Stadium Nominatim
>```

Or just go straight to Google:

```
> geocode(stadiums, geocoders='Google')
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=1250%201st%20Avenue%20South,%20Seattle,%20WA%2098134&sensor=false
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=2401%20Ontario%20St,%20Cleveland,%20OH%2044115&sensor=false
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Yankee%20Stadium&sensor=false
  Number            Street      City      State   Zip Latitude  Longitude                             InputAddress Source
1   1250  1st Avenue South   Seattle Washington 98134 47.59134 -122.33201 1250 1st Avenue South, Seattle, WA 98134 Google
2   2401    Ontario Street Cleveland       Ohio 44115 41.49570  -81.68527     2401 Ontario St, Cleveland, OH 44115 Google
3      1 East 161st Street      <NA>   New York 10451 40.82964  -73.92617                           Yankee Stadium Google
>```

An unresolvable address:

```
> geocode('A non-existent address, Chicago, IL')
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=A%20non-existent%20address,%20Chicago,%20IL&sensor=false
  Number Street City State  Zip Latitude Longitude                        InputAddress Source
1   <NA>   <NA> <NA>  <NA> <NA>       NA        NA A non-existent address, Chicago, IL Google
Warning message:
geocode failed with status ZERO_RESULTS, location = "A non-existent address, Chicago, IL" 
>```

Centroid of a jurisdiction (state, county, etc.):

```
> geocode('Kennebec County, ME')
  Number Street City State  Zip Latitude Longitude        InputAddress    Source
1   <NA>   <NA> <NA> Maine <NA> 44.41846 -69.82507 Kennebec County, ME Nominatim
>```