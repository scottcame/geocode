library(geocode)

context('Combined geocoding')

test_that('Normal addresses', {

  expect_equal(geocode(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1100-1298', '4500-4672', NA),
                          Street=c('1ST AVE S','WYOGA LAKE RD', NA),
                          City=c('SEATTLE', 'CUYAHOGA FALLS', NA),
                          State=c('WA', 'OH', 'Utah'),
                          Zip=c('98134','44224', NA),
                          Latitude=c(47.5909, 41.1939, 39.42252),
                          Longitude=c(-122.33419, -81.49443, -111.71436),
                          InputAddress=c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah'),
                          Source=c(rep('Census',2), 'Nominatim')),
               tolerance=.000001)
})

test_that('Caching', {

  f <- tempfile()

  expect_false(file.exists(f))

  expect_equal(geocode(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah'), cache=f),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1100-1298', '4500-4672', NA),
                          Street=c('1ST AVE S','WYOGA LAKE RD', NA),
                          City=c('SEATTLE', 'CUYAHOGA FALLS', NA),
                          State=c('WA', 'OH', 'Utah'),
                          Zip=c('98134','44224', NA),
                          Latitude=c(47.5909, 41.1939, 39.42252),
                          Longitude=c(-122.33419, -81.49443, -111.71436),
                          InputAddress=c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah'),
                          Source=c(rep('Census',2), 'Nominatim')),
               tolerance=.000001)

  expect_true(file.exists(f))

  expect_equal(geocode(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah'), cache=f),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1100-1298', '4500-4672', NA),
                          Street=c('1ST AVE S','WYOGA LAKE RD', NA),
                          City=c('SEATTLE', 'CUYAHOGA FALLS', NA),
                          State=c('WA', 'OH', 'Utah'),
                          Zip=c('98134','44224', NA),
                          Latitude=c(47.5909, 41.1939, 39.42252),
                          Longitude=c(-122.33419, -81.49443, -111.71436),
                          InputAddress=c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224', 'Utah'),
                          Source=c(rep('Census',2), 'Nominatim')),
               tolerance=.000001)

})
