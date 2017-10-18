#' Geocode addresses using a hierarchy of geocoders, where later geocoders fill in gaps that earlier ones couldn't handle
#' @param geocoders the geocoders to try, in order
#' @param addresses the list of addresses to geocode
#' @param ... additional named arguments passed to specific geocoder functions
#' @import dplyr
#' @import tibble
#' @return a data frame with the results of geocoding, with the SourceIndex column providing the index into the input address vector
#' @export
geocode <- function(addresses, geocoders=c('Census', 'Nominatim', 'Google'), ...) {

  ret <- tibble()

  addresses <- trimws(addresses)

  uniqueAddresses <- unique(addresses) %>% na.omit() %>% as.vector()
  indices <- seq(length(uniqueAddresses))
  names(indices) <- uniqueAddresses

  missings <- uniqueAddresses

  for (geocoder in geocoders) {
    if (length(uniqueAddresses) > 0) {
      f <- paste0('geocode', geocoder)
      args <- list()
      args$addresses <- missings
      args <- c(args, list(...))
      ret <- bind_rows(ret, do.call(f, args))
      successes <- character()
      if (nrow(ret)) {
        successes <- ret$InputAddress
      }
      missings <- setdiff(uniqueAddresses, successes)
    }
  }

  if (nrow(ret)) {

    ret <- select(ret, -SourceIndex) %>%
      right_join(tibble(InputAddress=addresses) %>% mutate(SourceIndex=row_number()), by='InputAddress') %>%
      filter(!is.na(Source))

  }

  ret

}

#' Geocode addresses using the Census Geocoder
#' @param addresses a vector of address strings
#' @param ... unused
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import tibble
#' @importFrom purrr map2_df
#' @return a data frame with the results of geocoding, with the SourceIndex column providing the index into the input address vector
#' @export
geocodeCensus <- function(addresses, ...) {

  baseURL <- 'https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?format=json&benchmark=Public_AR_Current&address='

  indices <- seq(length(addresses))

  sel <- !grepl(x=trimws(addresses), pattern='^[0-9]+$')
  indices <- indices[sel]
  addresses <- addresses[sel]

  map2_df(addresses, indices, function(address, index) {

    url <- paste0(baseURL, URLencode(address))
    content <- fromJSON(url, simplifyDataFrame=FALSE)

    ret <- NULL

    if (!is.null(content)) {

      result <- content$result

      if (!is.null(result)) {

        addressMatches <- result$addressMatches

        if (length(addressMatches) > 0) {

          matchComponents <- addressMatches[[1]]$addressComponents
          coords <- addressMatches[[1]]$coordinates

          ret <- tibble(
            Number = paste0(matchComponents$fromAddress, '-', matchComponents$toAddress),
            Street = gsub(x=trimws(paste0(matchComponents$preQualifier, ' ',
                                          matchComponents$preDirection, ' ',
                                          matchComponents$preType, ' ',
                                          matchComponents$streetName, ' ',
                                          matchComponents$suffixType, ' ',
                                          matchComponents$suffixQualifier, ' ',
                                          matchComponents$suffixDirection)),
                          pattern='[ ]+', replacement=' '),
            City = matchComponents$city,
            State = matchComponents$state,
            Zip = matchComponents$zip,
            Latitude=as.numeric(coords$y),
            Longitude=as.numeric(coords$x),
            InputAddress=address,
            Approximate=(FALSE),
            Source='Census',
            SourceIndex=index
          )

        }

      }

      Sys.sleep(1.5 * (index < max(indices)))

    }

    ret

  })

}

#' Geocode addreses using the Nominatim service provided by Open Street Map
#' @param addresses a vector of address strings
#' @param nominatimServiceURL the base address of the nominatim service to use; include everything that precedes the ?
#' @param ... unused
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import tibble
#' @importFrom purrr map2_df
#' @return a data frame with the results of geocoding, with the SourceIndex column providing the index into the input address vector
#' @export
geocodeNominatim <- function(addresses, nominatimServiceURL='http://nominatim.openstreetmap.org', ...) {

  baseURL <- paste0(nominatimServiceURL, '?addressdetails=1&format=json&q=')

  indices <- seq(length(addresses))

  sel <- !grepl(x=trimws(addresses), pattern='^[0-9]+$')
  indices <- indices[sel]
  addresses <- addresses[sel]

  map2_df(addresses, indices, function(address, index) {

    url <- paste0(baseURL, URLencode(address))
    content <- fromJSON(url, simplifyDataFrame=FALSE)

    ret <- NULL

    if (length(content) > 0) {

      j <- content[[1]]

      ret <- tibble(
        Number = ifelse(is.null(j$address$house_number), NA_character_, j$address$house_number),
        Street = ifelse(is.null(j$address$road), NA_character_, j$address$road),
        City = ifelse(is.null(j$address$city), NA_character_, j$address$city),
        State = ifelse(is.null(j$address$state), NA_character_, j$address$state),
        Zip = ifelse(is.null(j$address$postcode), NA_character_, j$address$postcode),
        Latitude=as.numeric(j$lat),
        Longitude=as.numeric(j$lon),
        InputAddress=address,
        Approximate=(is.null(j$address$house_number)),
        Source='Nominatim',
        SourceIndex=index
      )

      Sys.sleep(1.5 * (index < max(indices)))

    }

    ret

  })

}

#' Geocode addreses using the Nominatim service provided by Open Street Map
#' @param addresses a vector of address strings
#' @param ... unused
#' @importFrom httr GET content
#' @importFrom purrr map_df map2_df
#' @import tibble
#' @import dplyr
#' @return a data frame with the results of geocoding, with the SourceIndex column providing the index into the input address vector
#' @export
geocodeGoogle <- function(addresses, ...) {

  baseURL <- 'https://maps.googleapis.com/maps/api/geocode/json?address='

  indices <- seq(length(addresses))

  sel <- !grepl(x=trimws(addresses), pattern='^[0-9]+$')
  indices <- indices[sel]
  addresses <- addresses[sel]

  map2_df(addresses, indices, function(address, index) {

    url <- paste0(baseURL, URLencode(address))
    content <- fromJSON(url, simplifyDataFrame=FALSE)

    ret <- NULL

    if (length(content) > 1) {

      results <- content$results

      if (length(results) > 0) {

        results <- results[[1]]

        ac <- results$address_components
        location <- results$geometry$location

        if (!is.null(ac)) {

          addNAField <- function(df_, fieldName) {
            if (!nrow(df_ %>% filter(field==fieldName))) {
              df_ <- bind_rows(df_, tibble(field=fieldName, value=NA_character_))
            }
            df_
          }

          ret <- ac %>% map_df(function(ll) {tibble(field=ll$types, value=ll$short_name)}) %>%
            filter(!(field %in% c('political')))

          if (nrow(filter(ret, field %in% c('administrative_area_level_1', 'postal_code')))) {

            ret <- ret %>%
              addNAField('street_number') %>%
              addNAField('route') %>%
              addNAField('locality') %>%
              addNAField('sublocality') %>%
              addNAField('administrative_area_level_1') %>%
              addNAField('postal_code') %>%
              spread(key=field, value=value) %>%
              mutate(locality=ifelse(is.na(locality), sublocality, locality)) %>%
              select(Number=street_number, Street=route, City=locality, State=administrative_area_level_1, Zip=postal_code) %>%
              mutate(Latitude=location$lat, Longitude=location$lng, Source='Google',
                     Approximate=is.na(Number), InputAddress=address, SourceIndex=index)

          }

        }

      }

      Sys.sleep(1.5 * (index < max(indices)))

    }

    ret

  })

}
