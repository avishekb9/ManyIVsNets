#' Run Comprehensive EPC Models
#'
#' @param data Enhanced EPC data with all instruments
#' @return List of fitted models
#' @export
run_comprehensive_epc_models <- function(data) {
  cat("Running comprehensive EPC models...\n")
  
  models <- list()
  
  # OLS Baseline
  tryCatch({
    models$OLS_Baseline <- lm(lnCO2 ~ lnUR + lnPCGDP + lnTrade + lnRES + 
                              factor(country) + factor(year), data = data)
    cat("✓ OLS Baseline completed\n")
  }, error = function(e) cat("✗ OLS Baseline failed:", e$message, "\n"))
  
  # IV Models with different instrument sets
  iv_specs <- list(
    IV_Geographic = c("geo_isolation"),
    IV_TE_Isolation = c("te_isolation"),
    IV_TE_Combined = c("te_isolation", "te_bridging"),
    IV_Tech_Composite = c("tech_composite"),
    IV_Migration_Composite = c("migration_composite"),
    IV_Financial_Composite = c("financial_composite"),
    IV_Alternative_SOTA = c("judge_historical_1", "spatial_lag_ur", "bartik_employment"),
    IV_Multidim_Composite = c("multidim_composite"),
    IV_Geographic_TE = c("geo_isolation", "te_isolation"),
    IV_Comprehensive_Real = c("geo_isolation", "tech_composite", "migration_composite", "financial_composite")
  )
  
  for(model_name in names(iv_specs)) {
    instruments <- iv_specs[[model_name]]
    
    tryCatch({
      # Create formula
      iv_formula <- as.formula(paste(
        "lnCO2 ~ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year) |",
        paste(instruments, collapse = " + "),
        "+ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year) | lnUR"
      ))
      
      models[[model_name]] <- AER::ivreg(iv_formula, data = data)
      cat("✓", model_name, "completed\n")
    }, error = function(e) {
      cat("✗", model_name, "failed:", e$message, "\n")
      models[[model_name]] <- NULL
    })
  }
  
  cat("✓ Comprehensive EPC models completed\n")
  return(models)
}

#' Calculate Instrument Strength
#'
#' @param data Enhanced EPC data
#' @return Data frame with instrument strength results
#' @export
calculate_instrument_strength <- function(data) {
  cat("Calculating instrument strength (F-statistics)...\n")
  
  # Define instrument sets
  instrument_sets <- list(
    Geographic_Single = c("geo_isolation"),
    Technology_Real = c("internet_adoption_lag", "mobile_infrastructure_1995"),
    Migration_Real = c("diaspora_network_strength", "english_language_advantage"),
    Geopolitical_Real = c("post_communist_transition", "nato_membership_early"),
    Financial_Real = c("financial_market_maturity", "banking_development_1990"),
    Natural_Risk_Real = c("seismic_risk_index", "island_isolation"),
    TE_Isolation = c("te_isolation"),
    Spatial_Lag_SOTA = c("spatial_lag_ur"),
    Bartik_SOTA = c("bartik_employment"),
    Migration_Network_SOTA = c("migration_network_ur"),
    Judge_Historical_SOTA = c("judge_historical_1"),
    Network_Clustering_SOTA = c("network_clustering_1"),
    Shift_Share_SOTA = c("shift_share_tech"),
    Gravity_Trade_SOTA = c("gravity_trade_1"),
    Tech_Composite = c("tech_composite"),
    Migration_Composite = c("migration_composite"),
    Geopolitical_Composite = c("geopolitical_composite"),
    Risk_Composite = c("risk_composite"),
    Financial_Composite = c("financial_composite"),
    Multidim_Composite = c("multidim_composite"),
    Geographic_TE = c("geo_isolation", "te_isolation"),
    Real_Geographic_Tech = c("geo_isolation", "tech_composite"),
    Alternative_SOTA_Combined = c("spatial_lag_ur", "bartik_employment", "judge_historical_1"),
    Comprehensive_Real = c("geo_isolation", "tech_composite", "migration_composite", "financial_composite")
  )
  
  strength_results <- data.frame()
  
  for(set_name in names(instrument_sets)) {
    instruments <- instrument_sets[[set_name]]
    
    tryCatch({
      # First stage regression
      first_stage_formula <- as.formula(paste(
        "lnUR ~", paste(instruments, collapse = " + "),
        "+ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year)"
      ))
      
      first_stage <- lm(first_stage_formula, data = data)
      
      # Calculate F-statistic for excluded instruments
      restricted_formula <- as.formula(paste(
        "lnUR ~ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year)"
      ))
      
      restricted_model <- lm(restricted_formula, data = data)
      
      # F-test
      f_test <- anova(restricted_model, first_stage)
      f_stat <- f_test$F[2]
      p_val <- f_test$`Pr(>F)`[2]
      
      # Strength classification
      strength <- dplyr::case_when(
        f_stat > 50 ~ "Very Strong",
        f_stat > 10 ~ "Strong",
        f_stat > 5 ~ "Moderate",
        TRUE ~ "Weak"
      )
      
      strength_results <- rbind(strength_results, data.frame(
        Instrument_Set = set_name,
        F_Statistic = f_stat,
        P_Value = p_val,
        R_Squared = summary(first_stage)$r.squared,
        N_Instruments = length(instruments),
        Strength = strength,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat("Error calculating strength for", set_name, ":", e$message, "\n")
    })
  }
  
  cat("✓ Instrument strength analysis completed\n")
  return(strength_results)
}
