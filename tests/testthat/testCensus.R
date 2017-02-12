library(geocode)

context('Census geocoding')

test_that('Normal addresses', {

  expect_equal(geocodeCensus('520 4th Ave E, Olympia, WA 98501'),
               data.frame(stringsAsFactors=FALSE,
                          Number='500-598',
                          Street='4TH AVE E',
                          City='OLYMPIA',
                          State='WA',
                          Zip='98501',
                          Latitude=47.04527,
                          Longitude=-122.8964), tolerance=.000001)

  expect_equal(geocodeCensus(c('1250 1st Avenue South, Seattle, WA 98134', '4550 Wyoga Lake Rd, Cuyahoga Falls, OH 44224')),
               data.frame(stringsAsFactors=FALSE,
                          Number=c('1100-1298', '4500-4672'),
                          Street=c('1ST AVE S','WYOGA LAKE RD'),
                          City=c('SEATTLE', 'CUYAHOGA FALLS'),
                          State=c('WA', 'OH'),
                          Zip=c('98134','44224'),
                          Latitude=c(47.5909, 41.1939),
                          Longitude=c(-122.33419, -81.49443)), tolerance=.000001)

})

test_that('Non-existent addresses', {
  expect_null(geocodeCensus('520 4th Ave E, Olympia, TX 12112'))
  expect_equal(geocodeCensus(c('520 4th Ave E, Olympia, WA 98501', '520 4th Ave E, Olympia, TX 12221')),
               data.frame(stringsAsFactors=FALSE,
                          Number='500-598',
                          Street='4TH AVE E',
                          City='OLYMPIA',
                          State='WA',
                          Zip='98501',
                          Latitude=47.04527,
                          Longitude=-122.8964), tolerance=.000001)
})

test_that('Empty input', {
  expect_null(geocodeCensus(character()))
  expect_null(geocodeCensus(NULL))
})
