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
        Number = j$address$house_number,
        Street = j$address$road,
        City = j$address$city,
        State = j$address$state,
        Zip = j$address$postcode,
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
  for (address in addresses) {
    df <- ggmap::geocode(address, output='more') %>% filter(!is.na(lon) & !is.na(lat))
    if (nrow(df) > 0) {
      df <- df %>%
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
  }
  ret
}
