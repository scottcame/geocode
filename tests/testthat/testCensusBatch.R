library(geocode)
library(tibble)
library(dplyr, quietly=TRUE)

context('Census Batch geocoding')

test_that('Single address', {

  result <- geocodeCensusBatch('520 4th Ave E', 'Olympia', 'WA', '98501')
  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Address, '520 4TH AVE E, OLYMPIA, WA, 98501')
  expect_equal(row$Latitude, 47.04527, tolerance=.0001)
  expect_equal(row$Longitude, -122.8964, tolerance=.0001)
  expect_equal(row$Source, 'CensusBatch')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA, 98501')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeCensusBatch('520 4th Ave E', 'Olympia', 'WA')
  expect_equal(nrow(result), 1L)

  row <- result[1,]
  expect_equal(row$Address, '520 4TH AVE E, OLYMPIA, WA, 98501')
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA,')

})

test_that('Multiple addresses', {

  result <- geocodeCensusBatch(
    c('520 4th Ave E', '4550 Wyoga Lake Rd'),
    c('Olympia', 'Cuyahoga Falls'),
    c('WA', 'OH'),
    c('98501', NA_character_))
  expect_equal(nrow(result), 2L)

  row <- result %>% filter(SourceIndex==1L)
  expect_equal(row$Latitude, 47.04527, tolerance=.0001)
  expect_equal(row$Longitude, -122.8964, tolerance=.0001)
  expect_equal(row$Source, 'CensusBatch')

  row <- result %>% filter(SourceIndex==2L)
  expect_equal(row$Latitude, 41.19390, tolerance=.0001)
  expect_equal(row$Longitude, -81.49443, tolerance=.0001)
  expect_equal(row$Source, 'CensusBatch')

})

test_that('Bad address', {

  result <- geocodeCensusBatch('X', 'Y', 'WA', '98501')
  expect_equal(nrow(result), 0)

  result <- geocodeCensusBatch(
    c('520 4th Ave E', 'X'),
    c('Olympia', 'Y'),
    c('WA', 'OH'),
    c('98501', NA_character_))

  expect_equal(nrow(result), 1L)
  expect_equal(nrow(result %>% filter(SourceIndex==1L)), 1L)

})
