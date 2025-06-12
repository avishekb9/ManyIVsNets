test_that("econometric models work correctly", {
  # Load and transform sample data to expected format
  data <- ManyIVsNets::sample_epc_data
  
  # Transform to log format if needed by your functions
  data$lnCO2 <- log(data$CO2_per_capita)
  data$lnUR <- log(data$UR)
  data$lnURF <- log(data$URF)
  data$lnURM <- log(data$URM)
  data$lnPCGDP <- log(data$PCGDP)
  data$lnTrade <- log(data$Trade)
  data$lnRES <- log(data$RES)
  
  # Now test with the transformed data
  expect_true("lnCO2" %in% names(data))
  expect_true("lnUR" %in% names(data))
  
  # Test instrument creation
  expect_error({
    instruments <- create_real_instruments_from_data(data)
  }, NA)
  
  # Test data merging
  instruments <- create_real_instruments_from_data(data)
  expect_error({
    final_data <- merge_epc_with_created_instruments(data, instruments)
  }, NA)
})
