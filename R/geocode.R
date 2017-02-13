#' Geocode addresses using a hierarchy of geocoders, where later geocoders fill in gaps that earlier ones couldn't handle
#' @param geocoders the geocoders to try, in order
#' @param addresses the list of addresses to geocode
#' @param cache the name of a file to use to cache results (to avoid repeat geocoding of the same addresses), NULL value means don't cache
#' @import dplyr
#' @export
geocode <- function(addresses, geocoders=c('Census', 'Nominatim', 'Google'), cache=NULL) {

  dfs <- list()

  if (!is.null(cache)) {
    if (file.exists(cache)) {
      df <- readRDS(cache)
      addresses <- setdiff(addresses, df$InputAddress)
      dfs[['cache']] <- df
    }
  }

  if (length(addresses) > 0) {
    gaps <- NULL
    for (gc in geocoders) {

      f <- paste0('geocode', gc)
      args <- list()
      if (is.null(gaps)) {
        args$addresses <- addresses
      } else {
        args$addresses <- gaps$InputAddress
      }
      df <- do.call(f, args)
      gaps <- df %>% filter(is.na(Latitude) | is.na(Longitude))
      df <- df %>% filter(!is.na(Latitude) & !is.na(Longitude))
      dfs[[gc]] <- df

      if (nrow(gaps) == 0) {
        break
      }

    }
    if (!is.null(gaps)) {
      dfs[['gaps']] <- gaps
    }
  }
  ret <- bind_rows(dfs)
  if (nrow(ret) == 0) {
    ret <- NULL
  }
  if (!is.null(cache) & !is.null(ret)) {
    saveRDS(ret, cache)
  }
  ret
}

#' Geocode addresses using the Census Geocoder
#' @param addresses a vector of address strings
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#' @export
geocodeCensus <- function(addresses) {
  geocodeViaREST(addresses, 'https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?format=json&benchmark=Public_AR_Current&address=', 0, function(content, inputAddress) {
    ret <- NULL
    if (length(content$exceptions) == 0 & length(content$result$addressMatches) > 0) {
      match <- content$result$addressMatches[[1]]
      matchComponents <- match$addressComponents
      df <- data.frame(
        stringsAsFactors = FALSE,
        Number = paste0(matchComponents$fromAddress, '-', matchComponents$toAddress),
        Street = gsub(x=trimws(paste0(matchComponents$preQualifier, ' ', matchComponents$preDirection, ' ', matchComponents$preType, ' ',
                                      matchComponents$streetName,
                                      ' ', matchComponents$suffixType, ' ', matchComponents$suffixQualifier, ' ', matchComponents$suffixDirection)),
                      pattern='[ ]+', replacement=' '),
        City = matchComponents$city,
        State = matchComponents$state,
        Zip = matchComponents$zip,
        Latitude=as.numeric(match$coordinates$y),
        Longitude=as.numeric(match$coordinates$x),
        InputAddress=inputAddress
      )
      ret <- df
    } else {
      df <- data.frame(
        stringsAsFactors = FALSE,
        Number = as.character(NA),
        Street = as.character(NA),
        City = as.character(NA),
        State = as.character(NA),
        Zip = as.character(NA),
        Latitude=as.numeric(NA),
        Longitude=as.numeric(NA),
        InputAddress=inputAddress
      )
      ret <- df
    }
    ret$Source='Census'
    ret
  })
}

#' Geocode addreses using the Nominatim service provided by Open Street Map
#' @param addresses a vector of address strings
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#' @export
geocodeNominatim <- function(addresses) {
  geocodeViaREST(addresses, 'http://nominatim.openstreetmap.org?addressdetails=1&format=json&q=', 1.5, function(content, inputAddress) {
    ret <- NULL
    if (length(content) > 0) {
      j <- content[[1]]
      ret <- data.frame(
        stringsAsFactors = FALSE,
        Number = ifelse(is.null(j$address$house_number), as.character(NA), j$address$house_number),
        Street = ifelse(is.null(j$address$road), as.character(NA), j$address$road),
        City = ifelse(is.null(j$address$city), as.character(NA), j$address$city),
        State = ifelse(is.null(j$address$state), as.character(NA), j$address$state),
        Zip = ifelse(is.null(j$address$postcode), as.character(NA), j$address$postcode),
        Latitude=as.numeric(j$lat),
        Longitude=as.numeric(j$lon),
        InputAddress=inputAddress
      )
    } else {
      ret <- data.frame(
        stringsAsFactors = FALSE,
        Number = as.character(NA),
        Street = as.character(NA),
        City = as.character(NA),
        State = as.character(NA),
        Zip = as.character(NA),
        Latitude=as.numeric(NA),
        Longitude=as.numeric(NA),
        InputAddress=inputAddress
      )
    }
    ret$Source='Nominatim'
    ret
  })
}

#' @importFrom utils URLencode
geocodeViaREST <- function(addresses, baseURL, sleep=0, contentToDataFrameFunction) {

  ret <- NULL

  dfs <- list()
  i <- 1

  for (address in addresses) {
    if (i > 1) {
      Sys.sleep(sleep)
    }
    url <- paste0(baseURL, URLencode(address))
    response <- GET(url)
    j <- content(response)
    args <- list()
    args$content <- j
    args$inputAddress <- address
    dfs[[i]] <- do.call(contentToDataFrameFunction, args)
    i <- i + 1
  }

  if (length(dfs) > 0) {
    ret <- bind_rows(dfs)
  }

  ret

}

#' Geocode addresses using the Google API, via the ggmap package
#' @param addresses a vector of address strings
#' @import dplyr
#' @export
geocodeGoogle <- function(addresses) {
  ret <- NULL
  dfs <- list()
  i <- 1

  addNACol <- function(df, col) {
    if (!(col %in% colnames(df))) {
      df[[col]] <- as.character(NA)
    }
    df
  }

  for (address in addresses) {

    df <- ggmap::geocode(address, output='more') %>% filter(!is.na(lon) & !is.na(lat))

    if (nrow(df) > 0) {
      df <- df %>%
        addNACol('locality') %>%
        addNACol('administrative_area_level_1') %>%
        addNACol('postal_code') %>%
        addNACol('street_number') %>%
        addNACol('route') %>%
        rename(Latitude=lat, Longitude=lon, City=locality, State=administrative_area_level_1, Zip=postal_code, Number=street_number, Street=route) %>%
        select(Number, Street, City, State, Zip, Latitude, Longitude) %>%
        mutate_each('as.character', -Latitude, -Longitude) %>%
        mutate(InputAddress=address)
    } else {
      df <- data.frame(
        stringsAsFactors = FALSE,
        Number = as.character(NA),
        Street = as.character(NA),
        City = as.character(NA),
        State = as.character(NA),
        Zip = as.character(NA),
        Latitude=as.numeric(NA),
        Longitude=as.numeric(NA),
        InputAddress=address
      )
    }

    dfs[[i]] <- df
    i <- i + 1
  }
  if (length(dfs) > 0) {
    ret <- bind_rows(dfs)
    ret$Source='Google'
  }
  ret
}
