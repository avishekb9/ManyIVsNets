test_that("Comprehensive models run without error", {
  # Use enhanced test data with all required variables
  sample_data <- create_enhanced_test_data()
  
  # Test model running
  models <- run_comprehensive_epc_models(sample_data)
  
  expect_true(is.list(models))
  expect_true(length(models) > 0)
  expect_true("OLS_Baseline" %in% names(models))
  
  # Test that OLS model works
  if(!is.null(models$OLS_Baseline)) {
    expect_s3_class(models$OLS_Baseline, "lm")
  }
})

test_that("IV diagnostics work", {
  # Create simple sample data
  sample_data <- create_enhanced_test_data()
  
  # Fit simple models
  ols_model <- lm(lnCO2 ~ lnUR + lnPCGDP + lnTrade + lnRES + factor(country) + factor(year), 
                  data = sample_data)
  
  models <- list(OLS_Baseline = ols_model)
  
  # Test diagnostics
  diagnostics <- run_comprehensive_iv_diagnostics(models)
  
  expect_true(is.list(diagnostics))
  expect_true("OLS_Baseline" %in% names(diagnostics))
  expect_true(diagnostics$OLS_Baseline$valid)
})

test_that("Results table creation works", {
  # Use enhanced test data
  sample_data <- create_enhanced_test_data()
  
  ols_model <- lm(lnCO2 ~ lnUR + lnPCGDP + lnTrade + lnRES + factor(country), 
                  data = sample_data)
  
  models <- list(OLS_Baseline = ols_model)
  diagnostics <- list(OLS_Baseline = list(valid = TRUE, n_obs = nrow(sample_data), r_squared = 0.5))
  
  # Test results table creation
  results_table <- create_comprehensive_results_table(models, diagnostics)
  
  expect_true(is.data.frame(results_table))
  expect_true("Model" %in% names(results_table))
  expect_true("UR_Coefficient" %in% names(results_table))
  expect_true(nrow(results_table) > 0)
})
