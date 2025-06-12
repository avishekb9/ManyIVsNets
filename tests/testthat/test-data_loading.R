test_that("EPC data loading and cleaning works", {
  # Test with sample data
  test_data <- create_test_epc_data()
  
  # Test data structure
  expect_true(is.data.frame(test_data))
  expect_true("country" %in% names(test_data))
  expect_true("year" %in% names(test_data))
  expect_true("CO2_per_capita" %in% names(test_data))
  expect_true("UR" %in% names(test_data))
  expect_true("PCGDP" %in% names(test_data))
  
  # Test data dimensions
  expect_equal(nrow(test_data), 50)
  expect_equal(length(unique(test_data$country)), 5)
  
  # Test that all values are positive
  expect_true(all(test_data$CO2_per_capita > 0))
  expect_true(all(test_data$UR > 0))
  expect_true(all(test_data$PCGDP > 0))
})

test_that("Data cleaning creates log variables correctly", {
  test_data <- create_test_epc_data()
  
  # Simulate the cleaning process
  cleaned_data <- test_data %>%
    dplyr::filter(!is.na(CO2_per_capita) & !is.na(UR) & !is.na(PCGDP)) %>%
    dplyr::mutate(
      lnCO2 = log(pmax(CO2_per_capita, 0.01)),
      lnUR = log(pmax(UR, 0.01)),
      lnPCGDP = log(pmax(PCGDP, 1))
    )
  
  expect_true("lnCO2" %in% names(cleaned_data))
  expect_true("lnUR" %in% names(cleaned_data))
  expect_true("lnPCGDP" %in% names(cleaned_data))
  
  # Test that log values are finite
  expect_true(all(is.finite(cleaned_data$lnCO2)))
  expect_true(all(is.finite(cleaned_data$lnUR)))
  expect_true(all(is.finite(cleaned_data$lnPCGDP)))
})
