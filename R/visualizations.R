#' Create Transfer Entropy Network Visualization
#'
#' @param te_results Transfer entropy analysis results
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_transfer_entropy_network <- function(te_results, output_dir = NULL) {
  if(igraph::vcount(te_results$te_network) == 0) {
    cat("No network to plot\n")
    return(NULL)
  }
  
  # Create the plot
  p <- ggraph::ggraph(te_results$te_network, layout = "stress") +
    ggraph::geom_edge_arc(ggplot2::aes(width = weight, alpha = weight),
                         arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm")),
                         start_cap = ggraph::circle(3, "mm"),
                         end_cap = ggraph::circle(3, "mm"),
                         color = "#2E86AB") +
    ggraph::geom_node_point(ggplot2::aes(color = variable_type, size = centrality)) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
    ggplot2::scale_color_viridis_d(name = "Variable Type") +
    ggplot2::scale_size_continuous(name = "Centrality", range = c(3, 8)) +
    ggplot2::scale_edge_width_continuous(name = "Transfer Entropy", range = c(0.5, 2)) +
    ggplot2::scale_edge_alpha_continuous(range = c(0.3, 0.8)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    ) +
    ggplot2::labs(
      title = "Transfer Entropy Network: EPC Variables Causal Relationships",
      subtitle = paste("Network Density:", round(igraph::edge_density(te_results$te_network), 3))
    )
  
  if(!is.null(output_dir)) {
    ggplot2::ggsave(
      filename = file.path(output_dir, "network_01_transfer_entropy_variables.png"),
      plot = p, width = 12, height = 8, dpi = 600, bg = "white"
    )
  }
  
  return(p)
}

#' Create Country Network Visualization by Income Classification
#'
#' @param country_network igraph network object
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_country_income_network <- function(country_network, output_dir = NULL) {
  if(igraph::vcount(country_network) == 0) {
    cat("No country network to plot\n")
    return(NULL)
  }
  
  # Create the plot
  p <- ggraph::ggraph(country_network, layout = "stress") +
    ggraph::geom_edge_link(alpha = 0.3, color = "#95A5A6") +
    ggraph::geom_node_point(ggplot2::aes(color = income_group, size = igraph::degree(country_network))) +
    ggraph::geom_node_text(ggplot2::aes(label = country_code), repel = TRUE, size = 2.5) +
    ggplot2::scale_color_manual(
      name = "Income Group",
      values = c("High_Income" = "#E74C3C", "Upper_Middle_Income" = "#F39C12",
                 "Lower_Middle_Income" = "#F1C40F", "Low_Income" = "#95A5A6")
    ) +
    ggplot2::scale_size_continuous(name = "Network Degree", range = c(2, 6)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    ) +
    ggplot2::labs(
      title = "Country Network by Income Classification",
      subtitle = paste("Network Density:", round(igraph::edge_density(country_network), 3))
    )
  
  if(!is.null(output_dir)) {
    ggplot2::ggsave(
      filename = file.path(output_dir, "network_02_countries_income_classification.png"),
      plot = p, width = 12, height = 8, dpi = 600, bg = "white"
    )
  }
  
  return(p)
}

#' Create Instrument Strength Comparison Visualization
#'
#' @param strength_results Data frame with instrument strength results
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_instrument_strength_comparison <- function(strength_results, output_dir = NULL) {
  p <- strength_results %>%
    dplyr::mutate(
      Instrument_Set = factor(Instrument_Set, levels = Instrument_Set[order(F_Statistic)]),
      Strength_Color = dplyr::case_when(
        Strength == "Very Strong" ~ "#27AE60",
        Strength == "Strong" ~ "#F39C12",
        Strength == "Moderate" ~ "#E67E22",
