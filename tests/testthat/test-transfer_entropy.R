test_that("Transfer entropy analysis works", {
  # Create sample data with required variables
  sample_data <- data.frame(
    country = rep(c("USA", "Germany", "Japan"), each = 15),
    year = rep(2000:2014, 3),
    lnCO2 = rnorm(45, 2.5, 0.3),
    lnUR = rnorm(45, 1.5, 0.3),
    lnURF = rnorm(45, 1.6, 0.3),
    lnURM = rnorm(45, 1.4, 0.3),
    lnPCGDP = rnorm(45, 10.5, 0.3),
    lnTrade = rnorm(45, 4, 0.3),
    lnRES = rnorm(45, 2.5, 0.3),
    country_code = rep(c("USA", "DEU", "JPN"), each = 15)
  )
  
  # Test transfer entropy analysis
  te_results <- conduct_transfer_entropy_analysis(sample_data)
  
  expect_true(is.list(te_results))
  expect_true("te_matrix" %in% names(te_results))
  expect_true("te_network" %in% names(te_results))
  expect_true(is.matrix(te_results$te_matrix))
  expect_true(igraph::is_igraph(te_results$te_network))
  
  # Test matrix dimensions
  expect_equal(nrow(te_results$te_matrix), 7)  # 7 variables
  expect_equal(ncol(te_results$te_matrix), 7)
  
  # Test that diagonal is zero
  expect_true(all(diag(te_results$te_matrix) == 0))
})

test_that("TE-based instruments creation works", {
  # Create sample data
  sample_data <- data.frame(
    country = rep(c("USA", "Germany", "Japan"), each = 15),
    year = rep(2000:2014, 3),
    lnCO2 = rnorm(45, 2.5, 0.3),
    lnUR = rnorm(45, 1.5, 0.3),
    lnURF = rnorm(45, 1.6, 0.3),
    lnURM = rnorm(45, 1.4, 0.3),
    lnPCGDP = rnorm(45, 10.5, 0.3),
    lnTrade = rnorm(45, 4, 0.3),
    lnRES = rnorm(45, 2.5, 0.3),
    country_code = rep(c("USA", "DEU", "JPN"), each = 15),
    income_group = rep(c("High_Income", "High_Income", "High_Income"), each = 15),
    region_enhanced = rep(c("North_America", "Western_Europe", "East_Asia"), each = 15),
    time_trend = rep(0:14, 3)
  )
  
  # Create TE results first
  te_results <- conduct_transfer_entropy_analysis(sample_data)
  
  # Test TE-based instrument creation
  te_iv_results <- create_te_based_instruments(sample_data, te_results)
  
  expect_true(is.list(te_iv_results))
  expect_true("enhanced_data" %in% names(te_iv_results))
  expect_true("country_network" %in% names(te_iv_results))
  expect_true("te_isolation" %in% names(te_iv_results$enhanced_data))
  
  # Test that instruments are numeric
  expect_true(is.numeric(te_iv_results$enhanced_data$te_isolation))
  expect_true(all(is.finite(te_iv_results$enhanced_data$te_isolation)))
})
