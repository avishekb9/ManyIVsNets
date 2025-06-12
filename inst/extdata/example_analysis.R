# Example analysis script for ManyIVsNets package
# This demonstrates the complete workflow

library(ManyIVsNets)

# Run complete EPC analysis pipeline
message("Running complete EPC analysis example...")

# Option 1: Use built-in sample data
results <- run_complete_epc_analysis(
  data_file = NULL,  # Uses sample_epc_data
  output_dir = tempdir()
)

# Option 2: Use your own data file
# results <- run_complete_epc_analysis(
#   data_file = "your_epc_data.csv",
#   output_dir = tempdir()
# )

# View key results
message("\n=== ANALYSIS SUMMARY ===")
message("Countries analyzed: ", length(unique(results$data$country)))
message("Time period: ", min(results$data$year), "-", max(results$data$year))
message("Total observations: ", nrow(results$data))

# Display instrument strength results
message("\n=== TOP 10 STRONGEST INSTRUMENTS ===")
top_instruments <- results$strength_results %>%
  arrange(desc(F_Statistic)) %>%
  head(10)
print(top_instruments)

# Display main EPC results
message("\n=== MAIN EPC RESULTS ===")
main_results <- results$models$OLS_Baseline
if(!is.null(main_results)) {
  coef_summary <- summary(main_results)$coefficients["lnUR", ]
  message("Unemployment coefficient: ", round(coef_summary[1], 4))
  message("Standard error: ", round(coef_summary[2], 4))
  message("T-statistic: ", round(coef_summary[3], 2))
  message("P-value: ", format(coef_summary[4], scientific = TRUE))
  message("Economic interpretation: 1% ↑ unemployment → ", 
      round(coef_summary[1] * 100, 3), "% ↓ CO2 emissions")
}

# Display network analysis results
message("\n=== NETWORK ANALYSIS RESULTS ===")
if(!is.null(results$te_results$te_network)) {
  message("Transfer entropy network density: ", 
      round(igraph::edge_density(results$te_results$te_network), 3))
  message("Number of causal links: ", 
      igraph::ecount(results$te_results$te_network))
}

message("\nAnalysis complete! Check the output directory for detailed results.")
