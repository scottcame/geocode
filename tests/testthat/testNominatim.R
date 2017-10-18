library(geocode)

context('Nominatim geocoding')

test_that('Normal addresses', {

  result <- geocodeNominatim('520 4th Ave E, Olympia, WA 98501')

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, '4th Avenue East')
  expect_equal(row$City, 'Olympia')
  expect_equal(row$State, 'Washington')
  expect_equal(row$Zip, '98502')
  expect_equal(row$Latitude, 47.04602, tolerance=.0001)
  expect_equal(row$Longitude, -122.8679, tolerance=.0001)
  expect_equal(row$Source, 'Nominatim')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA 98501')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeNominatim(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224'))

  expect_equal(nrow(result), 2L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]
  expect_equal(row$Number, '1250') # this area has been mapped at building level by OSM
  expect_equal(row$Street, '1st Avenue South')
  expect_equal(row$SourceIndex, 1L)

  row <- result[2,]
  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, 'Wyoga Lake Road')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Non-existent addresses', {

  result <- geocodeNominatim('520 4th Ave E, Olympia, TX 12112')
  expect_equal(nrow(result), 0L)

  result <- geocodeNominatim(c('520 4th Ave E, Olympia, TX 12221', '520 4th Ave E, Olympia, WA 98501'))
  row <- result[1,]
  expect_equal(nrow(result), 1L)
  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, '4th Avenue East')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Non-address locations', {

  result <- geocodeNominatim('Florida')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, NA_character_)
  expect_equal(row$State, 'Florida')
  expect_equal(row$Zip, NA_character_)
  expect_equal(row$Latitude, 27.75677, tolerance=.0001)
  expect_equal(row$Longitude, -81.46398, tolerance=.0001)
  expect_equal(row$Source, 'Nominatim')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Florida')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeNominatim('Boise, Idaho')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, 'Boise')
  expect_equal(row$State, 'Idaho')
  expect_equal(row$Zip, '83702')
  expect_equal(row$Latitude, 43.61656, tolerance=.0001)
  expect_equal(row$Longitude, -116.2008, tolerance=.0001)
  expect_equal(row$Source, 'Nominatim')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Boise, Idaho')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeNominatim('Gateway Arch, St. Louis, Missouri')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, '11')
  expect_equal(row$Street, "North 4th Street")
  expect_equal(row$City, 'St. Louis')
  expect_equal(row$State, 'Missouri')
  expect_equal(row$Zip, '63102')
  expect_equal(row$Latitude, 38.62461, tolerance=.0001)
  expect_equal(row$Longitude, -90.18498, tolerance=.0001)
  expect_equal(row$Source, 'Nominatim')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, 'Gateway Arch, St. Louis, Missouri')
  expect_equal(row$SourceIndex, 1L)

})

test_that('Empty input', {
  expect_equal(geocodeNominatim(character()), tibble())
  expect_equal(geocodeNominatim(NULL), tibble())
})
