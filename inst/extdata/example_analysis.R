# Example analysis script for ManyIVsNets package
# This demonstrates the complete workflow

library(ManyIVsNets)

# Run complete EPC analysis pipeline
cat("Running complete EPC analysis example...\n")

# Option 1: Use built-in sample data
results <- run_complete_epc_analysis(
  data_file = NULL,  # Uses sample_epc_data
  output_dir = "example_epc_analysis"
)

# Option 2: Use your own data file
# results <- run_complete_epc_analysis(
#   data_file = "your_epc_data.csv",
#   output_dir = "your_analysis_results"
# )

# View key results
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Countries analyzed:", length(unique(results$data$country)), "\n")
cat("Time period:", min(results$data$year), "-", max(results$data$year), "\n")
cat("Total observations:", nrow(results$data), "\n")

# Display instrument strength results
cat("\n=== TOP 10 STRONGEST INSTRUMENTS ===\n")
top_instruments <- results$strength_results %>%
  arrange(desc(F_Statistic)) %>%
  head(10)
print(top_instruments)

# Display main EPC results
cat("\n=== MAIN EPC RESULTS ===\n")
main_results <- results$models$OLS_Baseline
if(!is.null(main_results)) {
  coef_summary <- summary(main_results)$coefficients["lnUR", ]
  cat("Unemployment coefficient:", round(coef_summary[1], 4), "\n")
  cat("Standard error:", round(coef_summary[2], 4), "\n")
  cat("T-statistic:", round(coef_summary[3], 2), "\n")
  cat("P-value:", format(coef_summary[4], scientific = TRUE), "\n")
  cat("Economic interpretation: 1% ↑ unemployment →", 
      round(coef_summary[1] * 100, 3), "% ↓ CO2 emissions\n")
}

# Display network analysis results
cat("\n=== NETWORK ANALYSIS RESULTS ===\n")
if(!is.null(results$te_results$te_network)) {
  cat("Transfer entropy network density:", 
      round(igraph::edge_density(results$te_results$te_network), 3), "\n")
  cat("Number of causal links:", 
      igraph::ecount(results$te_results$te_network), "\n")
}

cat("\nAnalysis complete! Check the output directory for detailed results.\n")
