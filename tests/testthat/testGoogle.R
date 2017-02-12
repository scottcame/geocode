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
                          Longitude=-122.8957), tolerance=.0001)

  expect_equal(geocodeGoogle(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1250', '4550'),
                          Street=c('1st Avenue South','Wyoga Lake Road'),
                          City=c('Seattle', 'Cuyahoga Falls'),
                          State=c('Washington', 'Ohio'),
                          Zip=c('98134','44224'),
                          Latitude=c(47.59134, 41.19192),
                          Longitude=c(-122.33201, -81.49618)), tolerance=.0001)

})

test_that('Non-existent addresses', {
  expect_null(geocodeGoogle('520 4th Ave E, Olympia, TX 11122'))
  expect_equal(geocodeGoogle(c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 11122')),
               data.frame(stringsAsFactors=FALSE,
                          Number='520',
                          Street='4th Avenue East',
                          City='Olympia',
                          State='Washington',
                          Zip='98501',
                          Latitude=47.0459,
                          Longitude=-122.8957), tolerance=.0001)
})

test_that('Empty input', {
  expect_null(geocodeGoogle(character()))
  expect_null(geocodeGoogle(NULL))
})
