
#' Run Complete EPC Analysis Pipeline
#'
#' @param data_file Path to EPC data file (optional)
#' @param output_dir Directory for outputs
#' @return List with all analysis results
#' @export
run_complete_epc_analysis <- function(data_file = NULL, output_dir = tempdir()) {
  
  message("=== RUNNING COMPLETE EPC ANALYSIS PIPELINE ===")
  
  # Load data - FIXED
  if(is.null(data_file)) {
    # Use sample data if no file provided - PROPER WAY
    epc_data <- ManyIVsNets::sample_epc_data

  } else {
    epc_data <- load_epc_data_corrected(data_file)
  }
  
  
  # Create instruments
  message("\n1. Creating instruments...")
  instruments <- create_real_instruments_from_data(epc_data)
  instruments_complete <- create_composite_instruments(instruments)
  
  # Merge data
  message("\n2. Merging data with instruments...")
  enhanced_data <- merge_epc_with_created_instruments(epc_data, instruments_complete)
  
  # Transfer entropy analysis
  message("\n3. Conducting transfer entropy analysis...")
  te_results <- conduct_transfer_entropy_analysis(enhanced_data)
  
  # Create TE-based instruments
  message("\n4. Creating TE-based instruments...")
  te_iv_results <- create_te_based_instruments(enhanced_data, te_results)
  enhanced_data_with_te <- te_iv_results$enhanced_data
  
  # Create alternative instruments
  message("\n5. Creating alternative SOTA instruments...")
  final_data <- create_alternative_sota_instruments(enhanced_data_with_te)
  
  # Calculate instrument strength
  message("\n6. Calculating instrument strength...")
  strength_results <- calculate_instrument_strength(final_data)
  
  # Run models
  message("\n7. Running econometric models...")
  models <- run_comprehensive_epc_models(final_data)
  
  # Run diagnostics
  message("\n8. Running diagnostics...")
  diagnostics <- run_comprehensive_iv_diagnostics(models)
  
  # Create visualizations
  message("\n9. Creating visualizations...")
  plots <- create_comprehensive_network_plots(
    te_results, te_iv_results, final_data, strength_results, 
    file.path(output_dir, "plots")
  )
  
  # Export results
  message("\n10. Exporting results...")
  export_comprehensive_results(
    models, diagnostics, strength_results, te_results,
    instruments_complete, te_iv_results$country_centralities,
    file.path(output_dir, "tables")
  )
  
  message("\nv COMPLETE EPC ANALYSIS PIPELINE FINISHED")
  message("Results saved to: ", output_dir)
  
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
