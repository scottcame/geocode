library(geocode)

context('Nominatim geocoding')

test_that('Normal addresses', {

  expect_equal(geocodeNominatim('520 4th Ave E, Olympia, WA 98501'),
                   data.frame(stringsAsFactors=FALSE,
                              Number='520',
                              Street='4th Avenue East',
                              City='Olympia',
                              State='Washington',
                              Zip='98501',
                              Latitude=47.0453482244898,
                              Longitude=-122.896375877551,
                              InputAddress='520 4th Ave E, Olympia, WA 98501',
                              Source='Nominatim'), tolerance=.000001)

  expect_equal(geocodeNominatim(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224')),
                   data.frame(stringsAsFactors=FALSE,
                              Number=c('1250', '4550'),
                              Street=c('1st Avenue South','Wyoga Lake Road'),
                              City=c('Seattle', 'Cuyahoga Falls'),
                              State=c('Washington', 'Ohio'),
                              Zip=c('98134','44224'),
                              Latitude=c(47.59185, 41.19384),
                              Longitude=c(-122.33396, -81.49453),
                              InputAddress=c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224'),
                              Source='Nominatim'), tolerance=.000001)

})

test_that('Non-existent addresses', {
  expect_equal(geocodeNominatim('520 4th Ave E, Olympia, TX 12112'),
               data.frame(
                 stringsAsFactors = FALSE,
                 Number = as.character(NA),
                 Street = as.character(NA),
                 City = as.character(NA),
                 State = as.character(NA),
                 Zip = as.character(NA),
                 Latitude=as.numeric(NA),
                 Longitude=as.numeric(NA),
                 InputAddress='520 4th Ave E, Olympia, TX 12112',
                 Source='Nominatim'))
  expect_equal(geocodeNominatim(c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 12221')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('520', NA),
                          Street=c('4th Avenue East', NA),
                          City=c('Olympia', NA),
                          State=c('Washington', NA),
                          Zip=c('98501', NA),
                          Latitude=c(47.0453482244898, NA),
                          Longitude=c(-122.896375877551, NA),
                          InputAddress=c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 12221'),
                          Source=rep('Nominatim', 2)), tolerance=.000001)

})

test_that('Non-address locations', {
  expect_equal(geocodeNominatim('Florida'),
               data.frame(
                 stringsAsFactors = FALSE,
                 Number = as.character(NA),
                 Street = as.character(NA),
                 City = as.character(NA),
                 State = 'Florida',
                 Zip = as.character(NA),
                 Latitude=27.75677,
                 Longitude=-81.46398,
                 InputAddress='Florida',
                 Source='Nominatim'), tolerance=.0001)
  expect_equal(geocodeNominatim('Boise, Idaho'),
               data.frame(
                 stringsAsFactors = FALSE,
                 Number = as.character(NA),
                 Street = as.character(NA),
                 City = 'Boise',
                 State = 'Idaho',
                 Zip = as.character(NA),
                 Latitude=43.61656,
                 Longitude=-116.2008,
                 InputAddress='Boise, Idaho',
                 Source='Nominatim'), tolerance=.0001)
  expect_equal(geocodeNominatim('Gateway Arch, St. Louis, Missouri'),
               data.frame(
                 stringsAsFactors = FALSE,
                 Number = as.character(NA),
                 Street = as.character(NA),
                 City = 'St. Louis',
                 State = 'Missouri',
                 Zip = '63101',
                 Latitude=38.62461,
                 Longitude=-90.18498,
                 InputAddress='Gateway Arch, St. Louis, Missouri',
                 Source='Nominatim'), tolerance=.0001)
})

test_that('Empty input', {
  expect_null(geocodeNominatim(character()))
  expect_null(geocodeNominatim(NULL))
})
