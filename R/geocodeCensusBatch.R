# Copyright 2017 Scott Came Consulting LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Geocodes a batch of addresses via the US Census Bureau's batch geocoder
#' @param street the street portion of the address (e.g., 123 Main St.)
#' @param city the city portion of the address
#' @param state the state portion of the address
#' @param zip the zip (postal) code portion of the address
#' @param sourceIndex a vector of identifiers to identify each address and correlate with the response
#' @param quietly whether to suppress diagnostic/progress messages
#' @importFrom readr write_csv read_csv read_lines
#' @importFrom purrr pmap_df
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @import dplyr
#' @importFrom httr POST content upload_file
#' @export
geocodeCensusBatch <- function(street, city=NA_character_, state=NA_character_, zip=NA_character_, sourceIndex=NULL, quietly=FALSE) {

  maxLength <- max(c(length(street), length(city), length(state), length(zip)))

  if (length(street)==1) {
    street <- rep(street, maxLength)
  }

  if (length(city)==1) {
    city <- rep(city, maxLength)
  }

  if (length(state)==1) {
    state <- rep(state, maxLength)
  }

  if (length(zip)==1) {
    zip <- rep(zip, maxLength)
  }

  if (is.null(sourceIndex)) {
    sourceIndex <- seq(street)
  }

  if (length(unique(c(
    length(street),
    length(city),
    length(state),
    length(zip),
    length(sourceIndex)
  ))) > 1) {
    stop('All parameters must be of the same length.')
  }

  if ((length(sourceIndex) != length(unique(sourceIndex))) | class(sourceIndex) != 'integer') {
    stop('Source index parameter must contain a unique integer ID for each address')
  }

  infile <- tempfile(fileext='.csv')
  outfile <- tempfile(fileext='.csv')

  #writeLines(paste0('infile=', infile))
  #writeLines(paste0('outfile=', outfile))

  args <- list()
  args$street <- split(street, ceiling(seq_along(street)/999))
  args$city <- split(city, ceiling(seq_along(city)/999))
  args$state <- split(state, ceiling(seq_along(state)/999))
  args$zip <- split(zip, ceiling(seq_along(zip)/999))
  args$sourceIndex <- split(sourceIndex, ceiling(seq_along(sourceIndex)/999))

  batches <- length(args$street)
  args$batch <- seq(batches)

  ret <- pmap_df(args, function(sourceIndex, street, city, state, zip, batch) {

    if (!quietly) {
      writeLines(paste0('Processing batch ', batch, ' of ', batches))
    }

    tibble(sourceIndex, street, city, state, zip) %>%
      write_csv(infile, col_names=FALSE, na='')

    response <- POST('https://geocoding.geo.census.gov/geocoder/locations/addressbatch',
                     body=list(benchmark='4', addressFile=upload_file(infile, "text/csv")),
                     encode='multipart')

    responseText <- suppressMessages(content(response, as='text'))

    lines <- responseText %>% read_lines()
    lines <- lines[!grepl(x=lines, pattern='No_Match"|"Tie"$')]

    #outfile <- tempfile(fileext='.csv')
    #writeLines(paste0('outfile=', outfile))
    writeLines(lines, outfile)

    ret <- suppressMessages(
      read_csv(outfile,
               col_names=c('SourceIndex', 'InputAddress', 'Match', 'Status', 'Address', 'Coordinates', 'TIGERLineID', 'Side'),
               col_types='icccccic'))

    writeLines(paste0('Batch had ', nrow(ret), ' successful geocodes'))

    ret

  })

  if (nrow(ret)) {
    ret <- ret %>%
      separate(Coordinates, c('Longitude', 'Latitude'), sep=',', convert=TRUE) %>%
      mutate(Source='CensusBatch', Approximate=Status!='Exact')
  }

  ret

}
