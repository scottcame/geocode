#' @importFrom readr write_csv read_csv read_lines
#' @importFrom purrr pmap_df
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @import dplyr
#' @importFrom httr POST content upload_file
#' @export
geocodeCensusBatch <- function(street, city, state, zip=NA_character_) {

  if (length(unique(c(
    length(street),
    length(city),
    length(state),
    length(zip)
  ))) > 1) {
    stop('All parameters must be of the same length.')
  }

  infile <- tempfile(fileext='.csv')
  outfile <- tempfile(fileext='.csv')

  args <- list()
  args$street <- split(street, ceiling(seq_along(street)/999))
  args$city <- split(city, ceiling(seq_along(city)/999))
  args$state <- split(state, ceiling(seq_along(state)/999))
  args$zip <- split(zip, ceiling(seq_along(zip)/999))

  ret <- pmap_df(args, function(street, city, state, zip) {

    tibble(street,city,state,zip) %>% mutate(id=row_number()) %>%
      select(id,street,city,state,zip) %>%
      write_csv(infile, col_names=FALSE, na='')

    response <- POST('https://geocoding.geo.census.gov/geocoder/locations/addressbatch',
                     body=list(benchmark='4', addressFile=upload_file(infile, "text/csv")),
                     encode='multipart')

    lines <- suppressMessages(content(response, as='text')) %>% read_lines()
    lines <- lines[!grepl(x=lines, pattern='No_Match"$')]

    writeLines(lines, outfile)

    ret <- suppressMessages(
      read_csv(outfile,
               col_names=c('SourceIndex', 'InputAddress', 'Match', 'Status', 'Address', 'Coordinates', 'TIGERLineID', 'Side'),
               col_types='icccccic'))

    ret

  })

  if (nrow(ret)) {
    ret <- ret %>%
      separate(Coordinates, c('Longitude', 'Latitude'), sep=',', convert=TRUE) %>%
      mutate(Source='CensusBatch', Approximate=Status!='Exact')
  }

  ret

}
