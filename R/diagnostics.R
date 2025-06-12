#' Run Comprehensive IV Diagnostics
#'
#' @param models List of fitted models
#' @return List of diagnostic results
#' @export
run_comprehensive_iv_diagnostics <- function(models) {
  message("Running comprehensive IV diagnostics...")
  
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
  
  message("v Comprehensive IV diagnostics completed")
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
        warning("Error processing ", model_name)
      })
    }
  }
  
  return(results)
}
