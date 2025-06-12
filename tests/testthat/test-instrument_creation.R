test_that("Real instruments creation works", {
  test_data <- create_test_epc_data()
  
  # Test instrument creation function
  instruments <- create_real_instruments_from_data(test_data)
  
  expect_true(is.data.frame(instruments))
  expect_equal(nrow(instruments), 5)  # 5 countries
  expect_true("country" %in% names(instruments))
  expect_true("geo_isolation" %in% names(instruments))
  expect_true("internet_adoption_lag" %in% names(instruments))
  
  # Test that geo_isolation values are in reasonable range
  expect_true(all(instruments$geo_isolation >= 0))
  expect_true(all(instruments$geo_isolation <= 1))
  
  # Test that all countries have instruments
  expect_equal(length(unique(instruments$country)), 5)
})

test_that("Composite instruments creation works", {
  test_instruments <- create_test_instruments()  # This includes all required variables
  
  # Test composite creation
  enhanced_instruments <- create_composite_instruments(test_instruments)
  
  expect_true(is.data.frame(enhanced_instruments))
  expect_true("tech_composite" %in% names(enhanced_instruments))
  expect_true("migration_composite" %in% names(enhanced_instruments))
  expect_true("financial_composite" %in% names(enhanced_instruments))
  expect_true("multidim_composite" %in% names(enhanced_instruments))
  
  # Test that composite values are numeric
  expect_true(is.numeric(enhanced_instruments$tech_composite))
  expect_true(is.numeric(enhanced_instruments$migration_composite))
  expect_true(is.numeric(enhanced_instruments$financial_composite))
  expect_true(is.numeric(enhanced_instruments$multidim_composite))
})

test_that("Data merging works correctly", {
  test_data <- create_test_epc_data()
  test_instruments <- create_test_instruments()
  
  # Test merging
  merged_data <- merge_epc_with_created_instruments(test_data, test_instruments)
  
  expect_true(is.data.frame(merged_data))
  expect_true("geo_isolation" %in% names(merged_data))
  expect_true("CO2_per_capita" %in% names(merged_data))
  
  # Test that merge preserves data
  expect_equal(length(unique(merged_data$country)), 5)
  expect_true(nrow(merged_data) > 0)
})

test_that("Instrument strength calculation works", {
  # Use enhanced test data with variation
  sample_data <- create_enhanced_test_data()
  
  # Add more variation to avoid factor level issues
  sample_data <- sample_data %>%
    dplyr::mutate(
      # Ensure variation in composites
      tech_composite = tech_composite + rnorm(dplyr::n(), 0, 0.1),
      migration_composite = migration_composite + rnorm(dplyr::n(), 0, 0.1),
      financial_composite = financial_composite + rnorm(dplyr::n(), 0, 0.1),
      multidim_composite = multidim_composite + rnorm(dplyr::n(), 0, 0.1)
    )
  
  # Test strength calculation
  strength_results <- calculate_instrument_strength(sample_data)
  
  expect_true(is.data.frame(strength_results))
  expect_true("F_Statistic" %in% names(strength_results))
  expect_true("Strength" %in% names(strength_results))
  expect_true(all(strength_results$F_Statistic >= 0, na.rm = TRUE))
  expect_true(nrow(strength_results) > 0)
})
