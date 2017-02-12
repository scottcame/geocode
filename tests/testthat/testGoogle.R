library(geocode)

context('Google (ggmap) geocoding')

test_that('Normal addresses', {

  expect_equal(geocodeGoogle('520 4th Ave E, Olympia, WA 98501'),
               data.frame(stringsAsFactors=FALSE,
                          Number='520',
                          Street='4th Avenue East',
                          City='Olympia',
                          State='Washington',
                          Zip='98501',
                          Latitude=47.0459,
                          Longitude=-122.8957,
                          InputAddress='520 4th Ave E, Olympia, WA 98501'), tolerance=.0001)

  expect_equal(geocodeGoogle(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1250', '4550'),
                          Street=c('1st Avenue South','Wyoga Lake Road'),
                          City=c('Seattle', 'Cuyahoga Falls'),
                          State=c('Washington', 'Ohio'),
                          Zip=c('98134','44224'),
                          Latitude=c(47.59134, 41.19192),
                          Longitude=c(-122.33201, -81.49618),
                          InputAddress=c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224')), tolerance=.0001)

})

test_that('Non-existent addresses', {
  expect_equal(geocodeCensus('520 4th Ave E, Olympia, TX 12112'),
               data.frame(
                 stringsAsFactors = FALSE,
                 Number = as.character(NA),
                 Street = as.character(NA),
                 City = as.character(NA),
                 State = as.character(NA),
                 Zip = as.character(NA),
                 Latitude=as.numeric(NA),
                 Longitude=as.numeric(NA),
                 InputAddress='520 4th Ave E, Olympia, TX 12112'))
  expect_equal(geocodeGoogle(c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 11122')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('520', NA),
                          Street=c('4th Avenue East', NA),
                          City=c('Olympia', NA),
                          State=c('Washington', NA),
                          Zip=c('98501', NA),
                          Latitude=c(47.0459, NA),
                          Longitude=c(-122.8957, NA),
                          InputAddress=c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 11122')), tolerance=.0001)
})

test_that('Empty input', {
  expect_null(geocodeGoogle(character()))
  expect_null(geocodeGoogle(NULL))
})
