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

context('Combined geocoding')

test_that('Normal addresses', {

  result <- geocode(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224',
                      NA_character_, 'ispum 5x %% loquitor', 'Utah'))

  expect_equal(nrow(result), 3)

  row <- result[1, ]
  expect_equal(row$Number, '1100-1298')
  expect_equal(row$Source, 'Census')
  expect_equal(row$SourceIndex, 1L)

  row <- result[2, ]
  expect_equal(row$Number, '4500-4672')
  expect_equal(row$Source, 'Census')
  expect_equal(row$SourceIndex, 2L)

  row <- result[3, ]
  expect_equal(row$Number, NA_character_)
  expect_equal(row$State, 'Utah')
  expect_equal(row$Source, 'Nominatim')
  expect_equal(row$SourceIndex, 5L)

})
