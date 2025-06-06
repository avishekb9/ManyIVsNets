#' Safe Package Loading with Conflict Resolution
#'
#' @param packages Character vector of package names
#' @return List with loaded and failed packages
#' @export
safe_load_packages <- function(packages) {
  loaded <- character(0)
  failed <- character(0)
  
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org/")
        library(pkg, character.only = TRUE)
        loaded <- c(loaded, pkg)
        cat("✓ Installed & loaded:", pkg, "\n")
      }, error = function(e) {
        failed <- c(failed, pkg)
        cat("~ Package not available:", pkg, "\n")
      })
    } else {
      loaded <- c(loaded, pkg)
      cat("✓ Already loaded:", pkg, "\n")
    }
  }
  
  return(list(loaded = loaded, failed = failed))
}

#' Run Complete EPC Analysis Pipeline
#'
#' @param data_file Path to EPC data file (optional)
#' @param output_dir Directory for outputs
#' @return List with all analysis results
#' @export
run_complete_epc_analysis <- function(data_file = NULL, output_dir = "epc_analysis_results") {
  
  cat("=== RUNNING COMPLETE EPC ANALYSIS PIPELINE ===\n")
  
  # Load data
  if(is.null(data_file)) {
    # Use sample data if no file provided
    data("sample_epc_data", package = "ManyIVsNets")
    epc_data <- sample_epc_data
  } else {
    epc_data <- load_epc_data_corrected(data_file)
  }
  
  # Create instruments
  cat("\n1. Creating instruments...\n")
  instruments <- create_real_instruments_from_data(epc_data)
  instruments_complete <- create_composite_instruments(instruments)
  
  # Merge data
  cat("\n2. Merging data with instruments...\n")
  enhanced_data <- merge_epc_with_created_instruments(epc_data, instruments_complete)
  
  # Transfer entropy analysis
  cat("\n3. Conducting transfer entropy analysis...\n")
  te_results <- conduct_transfer_entropy_analysis(enhanced_data)
  
  # Create TE-based instruments
  cat("\n4. Creating TE-based instruments...\n")
  te_iv_results <- create_te_based_instruments(enhanced_data, te_results)
  enhanced_data_with_te <- te_iv_results$enhanced_data
  
  # Create alternative instruments
  cat("\n5. Creating alternative SOTA instruments...\n")
  final_data <- create_alternative_sota_instruments(enhanced_data_with_te)
  
  # Calculate instrument strength
  cat("\n6. Calculating instrument strength...\n")
  strength_results <- calculate_instrument_strength(final_data)
  
  # Run models
  cat("\n7. Running econometric models...\n")
  models <- run_comprehensive_epc_models(final_data)
  
  # Run diagnostics
  cat("\n8. Running diagnostics...\n")
  diagnostics <- run_comprehensive_iv_diagnostics(models)
  
  # Create visualizations
  cat("\n9. Creating visualizations...\n")
  plots <- create_comprehensive_network_plots(
    te_results, te_iv_results, final_data, strength_results, 
    file.path(output_dir, "plots")
  )
  
  # Export results
  cat("\n10. Exporting results...\n")
  export_comprehensive_results(
    models, diagnostics, strength_results, te_results,
    instruments_complete, te_iv_results$country_centralities,
    file.path(output_dir, "tables")
  )
  
  cat("\n✓ COMPLETE EPC ANALYSIS PIPELINE FINISHED\n")
  cat("Results saved to:", output_dir, "\n")
  
  return(list(
    data = final_data,
    instruments = instruments_complete,
    te_results = te_results,
    models = models,
    diagnostics = diagnostics,
    strength_results = strength_results,
    plots = plots
  ))
}
