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

context('Census geocoding')

test_that('Normal addresses', {

  result <- geocodeCensus('520 4th Ave E, Olympia, WA 98501')

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]

  expect_equal(row$Number, '500-598')
  expect_equal(row$Street, '4TH AVE E')
  expect_equal(row$City, 'OLYMPIA')
  expect_equal(row$State, 'WA')
  expect_equal(row$Zip, '98501')
  expect_equal(row$Latitude, 47.04527, tolerance=.0001)
  expect_equal(row$Longitude, -122.8964, tolerance=.0001)
  expect_equal(row$Source, 'Census')
  expect_equal(row$Approximate, FALSE)
  expect_equal(row$InputAddress, '520 4th Ave E, Olympia, WA 98501')
  expect_equal(row$SourceIndex, 1L)

  result <- geocodeCensus(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224'))

  expect_equal(nrow(result), 2L)
  expect_equal(ncol(result), 11L)

  row <- result[1,]
  expect_equal(row$Number, '1100-1298')
  expect_equal(row$SourceIndex, 1L)

  row <- result[2,]
  expect_equal(row$Number, '4500-4672')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Non-existent addresses', {

  result <- geocodeCensus('520 4th Ave E, Olympia, TX 12112')
  expect_equal(nrow(result), 0L)

  result <- geocodeCensus(c('520 4th Ave E, Olympia, TX 12221', '520 4th Ave E, Olympia, WA 98501'))
  row <- result[1,]
  expect_equal(nrow(result), 1L)
  expect_equal(row$Number, '500-598')
  expect_equal(row$SourceIndex, 2L)

})

test_that('Empty input', {
  expect_equal(geocodeCensus(character()), tibble())
  expect_equal(geocodeCensus(NULL), tibble())
})
