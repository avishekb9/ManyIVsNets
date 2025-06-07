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
  
  # IV Models with different instrument sets - FIXED FORMULA CONSTRUCTION
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
      # Check if all instruments exist in data
      missing_instruments <- instruments[!instruments %in% names(data)]
      if(length(missing_instruments) > 0) {
        cat("✗", model_name, "failed: Missing instruments:", paste(missing_instruments, collapse = ", "), "\n")
        next
      }
      
      # Create proper IV formula - FIXED SYNTAX
      exog_vars <- c("lnPCGDP", "lnTrade", "lnRES", "factor(country)", "factor(year)")
      all_instruments <- c(instruments, exog_vars)
      
      # Correct AER::ivreg formula syntax: outcome ~ endogenous + exogenous | instruments + exogenous
      iv_formula <- as.formula(paste(
        "lnCO2 ~ lnUR +", paste(exog_vars, collapse = " + "),
        "|", paste(all_instruments, collapse = " + ")
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
  
  # Define instrument sets - ONLY USE VARIABLES THAT EXIST AND HAVE VARIATION
  instrument_sets <- list()
  
  # Check each instrument set before adding
  if("geo_isolation" %in% names(data) && length(unique(data$geo_isolation)) > 1) {
    instrument_sets$Geographic_Single <- c("geo_isolation")
  }
  
  if("tech_composite" %in% names(data) && length(unique(data$tech_composite)) > 1) {
    instrument_sets$Tech_Composite <- c("tech_composite")
  }
  
  if("migration_composite" %in% names(data) && length(unique(data$migration_composite)) > 1) {
    instrument_sets$Migration_Composite <- c("migration_composite")
  }
  
  if("financial_composite" %in% names(data) && length(unique(data$financial_composite)) > 1) {
    instrument_sets$Financial_Composite <- c("financial_composite")
  }
  
  if("multidim_composite" %in% names(data) && length(unique(data$multidim_composite)) > 1) {
    instrument_sets$Multidim_Composite <- c("multidim_composite")
  }
  
  if("te_isolation" %in% names(data) && length(unique(data$te_isolation)) > 1) {
    instrument_sets$TE_Isolation <- c("te_isolation")
  }
  
  # Only add spatial lag if we have enough observations (handle NA from lag)
  if("spatial_lag_ur" %in% names(data)) {
    complete_spatial <- data %>% dplyr::filter(!is.na(spatial_lag_ur))
    if(nrow(complete_spatial) > 10 && length(unique(complete_spatial$spatial_lag_ur)) > 1) {
      instrument_sets$Spatial_Lag_SOTA <- c("spatial_lag_ur")
    }
  }
  
  if("bartik_employment" %in% names(data) && length(unique(data$bartik_employment)) > 1) {
    instrument_sets$Bartik_SOTA <- c("bartik_employment")
  }
  
  if("judge_historical_1" %in% names(data) && length(unique(data$judge_historical_1)) > 1) {
    instrument_sets$Judge_Historical_SOTA <- c("judge_historical_1")
  }
  
  strength_results <- data.frame()
  
  for(set_name in names(instrument_sets)) {
    instruments <- instrument_sets[[set_name]]
    
    tryCatch({
      # Filter data to remove NAs for this specific instrument set
      analysis_data <- data %>%
        dplyr::filter(dplyr::if_all(dplyr::all_of(c("lnUR", "lnPCGDP", "lnTrade", "lnRES", "country", "year", instruments)), ~ !is.na(.)))
      
      if(nrow(analysis_data) < 10) {
        cat("Error calculating strength for", set_name, ": Insufficient data after removing NAs\n")
        next
      }
      
      # Check if we have enough countries for factor(country)
      if(length(unique(analysis_data$country)) < 2) {
        cat("Error calculating strength for", set_name, ": Need at least 2 countries\n")
        next
      }
      
      # First stage regression
      first_stage_formula <- as.formula(paste(
        "lnUR ~", paste(instruments, collapse = " + "),
        "+ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year)"
      ))
      
      first_stage <- lm(first_stage_formula, data = analysis_data)
      
      # Calculate F-statistic for excluded instruments
      restricted_formula <- as.formula(paste(
        "lnUR ~ lnPCGDP + lnTrade + lnRES + factor(country) + factor(year)"
      ))
      
      restricted_model <- lm(restricted_formula, data = analysis_data)
      
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


#' Run Comprehensive IV Diagnostics
#'
#' @param models List of fitted models
#' @return List of diagnostic results
#' @export
run_comprehensive_iv_diagnostics <- function(models) {
  cat("Running comprehensive IV diagnostics...\n")
  
  diagnostics <- list()
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    
    if(!is.null(model)) {
      tryCatch({
        if(grepl("IV_", model_name)) {
          # IV diagnostics
          model_summary <- summary(model, diagnostics = TRUE)
          
          # Extract diagnostics safely
          diag_names <- rownames(model_summary$diagnostics)
          
          sargan_test <- if("Sargan" %in% diag_names) {
            model_summary$diagnostics["Sargan", ]
          } else {
            c(statistic = NA, p.value = NA)
          }
          
          weak_test <- if("Weak instruments" %in% diag_names) {
            model_summary$diagnostics["Weak instruments", ]
          } else {
            c(statistic = NA, p.value = NA)
          }
          
          diagnostics[[model_name]] <- list(
            model_name = model_name,
            n_obs = nobs(model),
            r_squared = summary(model)$r.squared,
            sargan_statistic = sargan_test["statistic"],
            sargan_p_value = sargan_test["p-value"],
            weak_statistic = weak_test["statistic"],
            weak_p_value = weak_test["p-value"],
            valid = ifelse(is.na(sargan_test["p-value"]), TRUE, sargan_test["p-value"] > 0.05) && 
              summary(model)$r.squared > 0
          )
        } else {
          # OLS diagnostics
          diagnostics[[model_name]] <- list(
            model_name = model_name,
            n_obs = nobs(model),
            r_squared = summary(model)$r.squared,
            valid = TRUE
          )
        }
      }, error = function(e) {
        diagnostics[[model_name]] <- list(
          model_name = model_name,
          error = e$message,
          valid = FALSE
        )
      })
    }
  }
  
  cat("✓ Comprehensive IV diagnostics completed\n")
  return(diagnostics)
}

#' Create Comprehensive Results Table
#'
#' @param models List of fitted models
#' @param diagnostics List of diagnostic results
#' @return Data frame with comprehensive results
#' @export
create_comprehensive_results_table <- function(models, diagnostics) {
  results <- data.frame()
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    
    if(!is.null(model)) {
      tryCatch({
        # Get robust standard errors
        robust_vcov <- sandwich::vcovHC(model, type = "HC1")
        robust_summary <- lmtest::coeftest(model, vcov = robust_vcov)
        
        # Extract unemployment coefficient
        ur_coef <- robust_summary["lnUR", "Estimate"]
        ur_se <- robust_summary["lnUR", "Std. Error"]
        ur_pval <- robust_summary["lnUR", "Pr(>|t|)"]
        ur_tstat <- robust_summary["lnUR", "t value"]
        
        # Get diagnostics
        diag <- diagnostics[[model_name]]
        
        results <- rbind(results, data.frame(
          Model = model_name,
          Method = ifelse(grepl("IV_", model_name), "IV", "OLS"),
          UR_Coefficient = ur_coef,
          UR_SE = ur_se,
          UR_TStatistic = ur_tstat,
          UR_PValue = ur_pval,
          R_Squared = ifelse(is.null(diag$r_squared), NA, diag$r_squared),
          N_Obs = ifelse(is.null(diag$n_obs), NA, diag$n_obs),
          Sargan_PValue = ifelse(is.null(diag$sargan_p_value), NA, diag$sargan_p_value),
          Valid = ifelse(is.null(diag$valid), FALSE, diag$valid),
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        cat("Error processing", model_name, "\n")
      })
    }
  }
  
  return(results)
}
