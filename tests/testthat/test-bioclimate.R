test_that("bioclimate() fails gracefully", {
  expect_error(bioclimate(matrix(1:11, 11), matrix(1:11)),
               "12 months")
  expect_error(bioclimate(matrix(1:24, 12), matrix(1:12)),
               "should have the same dimensions")
})

test_that("bioclimate() returns expected values", {
  data(it_data)
  if (interactive()) {
    plot_h(it_data, c("MED", "NEM"), col = hcl.colors(2))
  }
  expect_equal(tolerance = 0.01,
    bioclimate(it_data$tavg_m["MED", 1:12], it_data$prec_m["MED", 1:12]),
    data.frame(abt = 15.6, tap = 625, per = 1.47,
               zone = "Tropical very dry forest", row.names = "MED")
  )
  expect_equal(
    bioclimate(it_data$tavg_m[, 1:12], it_data$prec_m[, 1:12])[
      c("NEM", "MED"), "zone"],
    c("Subtropical dry forest", "Tropical very dry forest")
  )
})
