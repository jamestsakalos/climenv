library(testthat)

test_that("elev() fails gracefully", {

  expect_error(
    expect_warning(
      elev(),
      "Error in elev() : argument location is missing, with no default"
    ))

  flip_lat_long <- sf::st_polygon(
    list(cbind(lat = c(-61, -49, -61, -61), lng = c(161, 161, 154, 161))))

  expect_error(
    expect_warning(
      elev(location = flip_lat_long),
      "Error in elev(location = flip_lat_long) :
  bounding box of location has potentially an invalid value range"
    ))

  flip_lat_long <- sf::st_polygon(
    list(cbind(long = c(161, 161, 154, 161),
               lat = c(-61, -49, -61, -61)))
  )

  expect_error(
    expect_warning(
      elev(location = flip_lat_long),
      "Error: check that the location has been projected (epsg: 4326)"
    ))
})
