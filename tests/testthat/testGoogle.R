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

library(geocode)
library(tibble)

context('Google geocoding')

test_that('Normal addresses', {

  result <- geocodeGoogle('520 4th Ave E, Olympia, WA 98501')

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]

  expect_equal(row$Number, '520')
  expect_equal(row$Street, '4th Ave E')
  expect_equal(row$City, 'Olympia')
  expect_equal(row$State, 'WA')
  expect_equal(row$Zip, '98501')
  expect_equal(row$Latitude, 47.0459, tolerance=.0001)
  expect_equal(row$Longitude, -122.8957, tolerance=.0001)
  expect_equal(row$Source, 'Google')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA 98501')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeGoogle(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224'))

  expect_equal(nrow(result), 2L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]
  expect_equal(row$Number, '1250')
  expect_equal(row$SourceIndex, 1L)

  row <- result[2,]
  expect_equal(row$Number, '4550')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Non-existent addresses', {

  result <- geocodeGoogle('123 g, lorem 12365 ipsus')
  expect_equal(nrow(result), 0)

  result <- geocodeGoogle(c('123 g, lorem 12365 ipsus', '520 4th Ave E, Olympia, WA 98501'))
  expect_equal(nrow(result), 1)

  row <- result[1,]
  expect_equal(row$Number, '520')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Sublocality', {
  result <- geocodeGoogle('Yankee Stadium')
  expect_equal(nrow(result), 1L)
  row <- result[1,]
  expect_equal(row$City, 'Bronx')
})

test_that('Non-address locations', {

  result <- geocodeGoogle('Florida')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, NA_character_)
  expect_equal(row$State, 'FL')
  expect_equal(row$Zip, NA_character_)
  expect_equal(row$Latitude, 27.66483, tolerance=.0001)
  expect_equal(row$Longitude, -81.51575, tolerance=.0001)
  expect_equal(row$Source, 'Google')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Florida')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeGoogle('Boise, Idaho')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, 'Boise')
  expect_equal(row$State, 'ID')
  expect_equal(row$Zip, NA_character_)
  expect_equal(row$Latitude, 43.61871, tolerance=.0001)
  expect_equal(row$Longitude, -116.2146, tolerance=.0001)
  expect_equal(row$Source, 'Google')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Boise, Idaho')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeGoogle('Gateway Arch, St. Louis, Missouri')

  expect_equal(nrow(result), 1L)

  row <- result[1,]

  expect_equal(row$Number, NA_character_)
  expect_equal(row$Street, NA_character_)
  expect_equal(row$City, 'St. Louis')
  expect_equal(row$State, 'MO')
  expect_equal(row$Zip, '63102')
  expect_equal(row$Latitude, 38.62469, tolerance=.0001)
  expect_equal(row$Longitude, -90.18478, tolerance=.0001)
  expect_equal(row$Source, 'Google')
  expect_equal(row$Approximate, TRUE)
  expect_equal(row$InputAddress, 'Gateway Arch, St. Louis, Missouri')
  expect_equal(row$SourceIndex, 1L)

})

test_that('Empty input', {
  expect_equal(geocodeGoogle(character()), tibble())
  expect_equal(geocodeGoogle(NULL), tibble())
})
