test_that("Transfer entropy network visualization works", {
  # Create mock TE results
  te_matrix <- matrix(runif(49, 0, 0.1), nrow = 7, ncol = 7)
  diag(te_matrix) <- 0
  rownames(te_matrix) <- c("lnCO2", "lnUR", "lnURF", "lnURM", "lnPCGDP", "lnTrade", "lnRES")
  colnames(te_matrix) <- c("lnCO2", "lnUR", "lnURF", "lnURM", "lnPCGDP", "lnTrade", "lnRES")
  
  # Add some actual relationships
  te_matrix[1, 5] <- 0.05  
  te_matrix[2, 3] <- 0.04
  
  # Create network
  te_network <- igraph::graph_from_adjacency_matrix(te_matrix, mode = "directed", weighted = TRUE)
  
  # Add attributes
  igraph::V(te_network)$variable_type <- c("Environmental", "Employment", "Employment", 
                                           "Employment", "Economic", "Economic", "Energy")
  igraph::V(te_network)$centrality <- igraph::degree(te_network)
  
  te_results <- list(
    te_matrix = te_matrix,
    te_network = te_network,
    te_threshold = 0.03
  )
  
  # Test visualization function
  plot_result <- plot_transfer_entropy_network(te_results)
  
  expect_true(!is.null(plot_result) || igraph::vcount(te_network) == 0)
  if(!is.null(plot_result)) {
    expect_s3_class(plot_result, "ggplot")
  }
})

test_that("Country network visualization works", {
  # Create symmetric adjacency matrix - FIXED
  adj_matrix <- matrix(runif(25, 0, 1), nrow = 5, ncol = 5)
  adj_matrix <- (adj_matrix + t(adj_matrix)) / 2  # Make symmetric
  diag(adj_matrix) <- 0
  rownames(adj_matrix) <- c("USA", "Germany", "Japan", "UK", "France")
  colnames(adj_matrix) <- c("USA", "Germany", "Japan", "UK", "France")
  
  country_network <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
  
  # Add attributes
  igraph::V(country_network)$income_group <- rep("High_Income", 5)
  igraph::V(country_network)$country_code <- c("USA", "DEU", "JPN", "GBR", "FRA")
  
  # Test visualization
  plot_result <- plot_country_income_network(country_network)
  
  expect_true(!is.null(plot_result) || igraph::vcount(country_network) == 0)
  if(!is.null(plot_result)) {
    expect_s3_class(plot_result, "ggplot")
  }
})

test_that("Instrument strength visualization works", {
  # Create mock strength results
  strength_results <- data.frame(
    Instrument_Set = c("Geographic_Single", "Tech_Composite", "Migration_Composite"),
    F_Statistic = c(5.27, 188.47, 44.12),
    Strength = c("Moderate", "Very Strong", "Strong"),
    stringsAsFactors = FALSE
  )
  
  # Test visualization
  plot_result <- plot_instrument_strength_comparison(strength_results)
  
  expect_s3_class(plot_result, "ggplot")
  expect_true(!is.null(plot_result))
})

test_that("Comprehensive network plots creation works", {
  skip_on_cran()  # Skip this test on CRAN due to graphics device issues
  
  # Create minimal test data
  te_results <- list(
    te_network = igraph::make_empty_graph(0),
    te_matrix = matrix(0, nrow = 2, ncol = 2)
  )
  
  te_iv_results <- list(
    country_network = igraph::make_empty_graph(0)
  )
  
  test_data <- create_enhanced_test_data()  # Use enhanced data
  
  strength_results <- data.frame(
    Instrument_Set = "Test",
    F_Statistic = 10,
    Strength = "Strong"
  )
  
  # Test comprehensive plots creation with error handling
  expect_error(
    plots <- create_comprehensive_network_plots(te_results, te_iv_results, test_data, strength_results),
    NA  # Expect no error
  )
})
