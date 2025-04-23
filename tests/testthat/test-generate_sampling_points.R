
library(NicEuc)


test_that("generate_sampling_points returns expected structure and types", {
  data("synthetic_gps", package = "NicEuc")
  set.seed(42)

  result <- suppressWarnings(generate_sampling_points(
    gps_ref = synthetic_gps,
    samples_per_plot = 4,
    min_distance = 0.3,
    target_species = c("centella_asiatica", "fimbristylis_dichotoma"),
    export_csv = FALSE
  ))


  expect_type(result, "list")
  expect_named(result, c("samples", "plot", "map", "moran_test"))
  expect_s3_class(result$samples, "data.frame")
  expect_true(all(c("Ring", "Plot", "Species", "utm_x", "utm_y") %in% names(result$samples)))
  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$map, "leaflet")
  expect_type(result$moran_test, "list")
  expect_true(all(unique(result$samples$Plot) %in% names(result$moran_test)))
})
