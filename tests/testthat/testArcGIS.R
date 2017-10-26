library(geocode)
library(tibble)

context('ArcGIS geocoding')

test_that('Normal addresses', {

  result <- geocodeArcGIS('520 4th Ave E, Olympia, WA 98501')

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]

  expect_equal(row$Number, '520')
  expect_equal(row$Street, '4th Ave E')
  expect_equal(row$City, 'Olympia')
  expect_equal(row$State, 'Washington')
  expect_equal(row$Zip, '98501')
  expect_equal(row$Latitude, 47.04533, tolerance=.0001)
  expect_equal(row$Longitude, -122.8957, tolerance=.0001)
  expect_equal(row$Source, 'ArcGIS')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA 98501')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeArcGIS(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224'))

  expect_equal(nrow(result), 2L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]
  expect_equal(row$Number, '1250')
  expect_equal(row$SourceIndex, 1L)

  row <- result[2,]
  expect_equal(row$Number, '4550')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Non-address locations', {

  result <- geocodeArcGIS('Florida')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, NA_character_)
  expect_equal(row$State, 'Florida')
  expect_equal(row$Zip, NA_character_)
  expect_equal(row$Latitude, 28.56604, tolerance=.0001)
  expect_equal(row$Longitude, -81.68865, tolerance=.0001)
  expect_equal(row$Source, 'ArcGIS')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Florida')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeArcGIS('Boise, Idaho')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, 'Boise')
  expect_equal(row$State, 'Idaho')
  expect_equal(row$Zip, NA_character_)
  expect_equal(row$Latitude, 43.60761, tolerance=.0001)
  expect_equal(row$Longitude, -116.1934, tolerance=.0001)
  expect_equal(row$Source, 'ArcGIS')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Boise, Idaho')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeArcGIS('Gateway Arch, St. Louis, Missouri')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, '200')
  expect_equal(row$Street, 'Washington Ave')
  expect_equal(row$City, 'Saint Louis')
  expect_equal(row$State, 'Missouri')
  expect_equal(row$Zip, '63102')
  expect_equal(row$Latitude, 38.6247, tolerance=.0001)
  expect_equal(row$Longitude, -90.18432, tolerance=.0001)
  expect_equal(row$Source, 'ArcGIS')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, 'Gateway Arch, St. Louis, Missouri')
  expect_equal(row$SourceIndex, 1L)

})

test_that('Approximate', {

  result <- geocodeArcGIS('Seattle, WA')
  expect_equal(nrow(result), 1L)
  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Approximate, TRUE)

  result <- geocodeArcGIS('1st Avenue South, Seattle, WA')
  expect_equal(nrow(result), 1L)
  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Approximate, TRUE)

  result <- geocodeArcGIS('1250 1st Avenue South, Seattle, WA')
  expect_equal(nrow(result), 1L)
  row <- result[1,]

  expect_equal(row$Number, '1250')
  expect_equal(row$Approximate, FALSE)

})

test_that('Empty input', {
  expect_equal(geocodeArcGIS(character()), tibble())
  expect_equal(geocodeArcGIS(NULL), tibble())
})
