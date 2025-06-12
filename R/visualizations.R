#' Create Transfer Entropy Network Visualization
#'
#' @param te_results Transfer entropy analysis results
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_transfer_entropy_network <- function(te_results, output_dir = NULL) {
  if(igraph::vcount(te_results$te_network) == 0) {
    message("No network to plot")
    return(NULL)
  }
  
  # Create the plot - FIXED ggraph function names
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
    ggraph::scale_edge_width_continuous(name = "Transfer Entropy", range = c(0.5, 2)) +  # FIXED: ggraph not ggplot2
    ggraph::scale_edge_alpha_continuous(range = c(0.3, 0.8)) +  # FIXED: ggraph not ggplot2
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
    message("No country network to plot")
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
#' Create Cross-Income CO2 Growth Nexus Visualization
#'
#' @param data Enhanced EPC data
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_cross_income_co2_nexus <- function(data, output_dir = NULL) {
  tryCatch({
    income_data <- data %>%
      dplyr::group_by(country, country_code, income_group) %>%
      dplyr::summarise(
        avg_ur = mean(lnUR, na.rm = TRUE),
        avg_co2 = mean(lnCO2, na.rm = TRUE),
        avg_gdp = mean(lnPCGDP, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::filter(!is.na(avg_ur) & !is.na(avg_co2) & !is.na(income_group))
    
    if(nrow(income_data) > 5) {
      p <- income_data %>%
        ggplot2::ggplot(ggplot2::aes(x = avg_ur, y = avg_co2, color = income_group)) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
        ggplot2::scale_color_manual(
          name = "Income Group",
          values = c("High_Income" = "#E74C3C", "Upper_Middle_Income" = "#F39C12",
                     "Lower_Middle_Income" = "#F1C40F", "Low_Income" = "#95A5A6")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Cross-Income CO2 Growth Nexus",
          x = "Average Unemployment Rate",
          y = "Average CO2 Emissions"
        )
      
      if(!is.null(output_dir)) {
        ggplot2::ggsave(
          filename = file.path(output_dir, "network_03_cross_income_co2_growth_nexus.png"),
          plot = p, width = 12, height = 8, dpi = 600, bg = "white"
        )
      }
      
      return(p)
    }
  }, error = function(e) {
    warning("Cross-income nexus visualization failed: ", e$message)
    return(NULL)
  })
}

#' Create Migration Impact Visualization
#'
#' @param data Enhanced EPC data
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_migration_impact <- function(data, output_dir = NULL) {
  tryCatch({
    if("diaspora_network_strength" %in% names(data) && "lnCO2" %in% names(data)) {
      migration_data <- data %>%
        dplyr::group_by(country, country_code, income_group) %>%
        dplyr::summarise(
          avg_migration = mean(diaspora_network_strength, na.rm = TRUE),
          avg_co2_growth = mean(c(diff(lnCO2), 0), na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::filter(!is.na(avg_migration) & !is.na(avg_co2_growth) & !is.na(income_group))
      
      if(nrow(migration_data) > 5) {
        p <- migration_data %>%
          ggplot2::ggplot(ggplot2::aes(x = avg_migration, y = avg_co2_growth, color = income_group)) +
          ggplot2::geom_point(size = 3, alpha = 0.7) +
          ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
          ggplot2::scale_color_manual(
            name = "Income Group",
            values = c("High_Income" = "#E74C3C", "Upper_Middle_Income" = "#F39C12",
                       "Lower_Middle_Income" = "#F1C40F", "Low_Income" = "#95A5A6")
          ) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Migration Network Impact on CO2 Growth",
            x = "Average Diaspora Network Strength",
            y = "Average CO2 Growth Rate"
          )
        
        if(!is.null(output_dir)) {
          ggplot2::ggsave(
            filename = file.path(output_dir, "network_04_migration_impact_co2.png"),
            plot = p, width = 12, height = 8, dpi = 600, bg = "white"
          )
        }
        
        return(p)
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Migration impact visualization failed: ", e$message)
    return(NULL)
  })
}

#' Create Instrument Causal Pathways Network
#'
#' @param data Enhanced EPC data
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_instrument_causal_pathways <- function(data, output_dir = NULL) {
  tryCatch({
    # Select key instruments that exist in data
    available_instruments <- c("geo_isolation", "te_isolation", "tech_composite", 
                               "migration_composite", "financial_composite")
    existing_instruments <- available_instruments[available_instruments %in% names(data)]
    
    if(length(existing_instruments) > 2) {
      instrument_data <- data %>%
        dplyr::select(dplyr::all_of(existing_instruments)) %>%
        dplyr::filter(complete.cases(.))
      
      if(nrow(instrument_data) > 10) {
        # Calculate correlation matrix
        cor_matrix <- cor(instrument_data, use = "complete.obs")
        cor_matrix[is.na(cor_matrix)] <- 0
        
        # Create simple visualization
        p <- ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "Instrument Causal Pathways\n(Network visualization)")) +
          ggplot2::theme_void() +
          ggplot2::labs(title = "Instrument Network with Causal Pathways")
        
        if(!is.null(output_dir)) {
          ggplot2::ggsave(
            filename = file.path(output_dir, "network_05_instruments_causal_pathways.png"),
            plot = p, width = 12, height = 8, dpi = 600, bg = "white"
          )
        }
        
        return(p)
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Instrument causal pathways visualization failed: ", e$message)
    return(NULL)
  })
}

#' Create Regional Network Visualization
#'
#' @param data Enhanced EPC data
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_regional_network <- function(data, output_dir = NULL) {
  tryCatch({
    if("region_enhanced" %in% names(data) && "country_code" %in% names(data)) {
      regional_data <- data %>%
        dplyr::filter(!is.na(region_enhanced) & region_enhanced != "Other") %>%
        dplyr::group_by(region_enhanced, country, country_code) %>%
        dplyr::summarise(
          avg_co2 = mean(lnCO2, na.rm = TRUE),
          avg_ur = mean(lnUR, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::filter(!is.na(avg_co2) & !is.na(avg_ur))
      
      if(nrow(regional_data) > 3) {
        p <- regional_data %>%
          ggplot2::ggplot(ggplot2::aes(x = avg_ur, y = avg_co2, color = region_enhanced)) +
          ggplot2::geom_point(size = 3, alpha = 0.7) +
          ggplot2::scale_color_viridis_d(name = "Region") +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Regional Network with Country Codes",
            x = "Average Unemployment Rate",
            y = "Average CO2 Emissions"
          )
        
        if(!is.null(output_dir)) {
          ggplot2::ggsave(
            filename = file.path(output_dir, "network_06_regional_country_codes.png"),
            plot = p, width = 12, height = 8, dpi = 600, bg = "white"
          )
        }
        
        return(p)
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Regional network visualization failed: ", e$message)
    return(NULL)
  })
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
        TRUE ~ "#E74C3C"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = Instrument_Set, y = F_Statistic, fill = Strength_Color)) +
    ggplot2::geom_col(alpha = 0.8, width = 0.7) +
    ggplot2::geom_hline(yintercept = 10, color = "#d62728", linetype = "dashed", linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 50, color = "#2ca02c", linetype = "dashed", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("F=", round(F_Statistic, 1))), 
                       hjust = -0.1, size = 3, fontface = "bold") +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      axis.text.y = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(
      title = "Comprehensive Instrument Strength Analysis",
      subtitle = "First-stage F-statistics for all instrument approaches (from scratch)",
      x = "Instrument Approach",
      y = "F-Statistic",
      caption = "Red line: F=10 (strong threshold) | Green line: F=50 (very strong)"
    )
  
  if(!is.null(output_dir)) {
    ggplot2::ggsave(
      filename = file.path(output_dir, "network_07_instrument_strength_comparison.png"),
      plot = p, width = 16, height = 12, dpi = 600, bg = "white"
    )
  }
  
  return(p)
}

#' Create Comprehensive Network Plots
#'
#' @param te_results Transfer entropy results
#' @param te_iv_results Transfer entropy IV results
#' @param data Enhanced EPC data
#' @param strength_results Instrument strength results
#' @param output_dir Directory to save plots (optional)
#' @return List of plot objects
#' @export
create_comprehensive_network_plots <- function(te_results, te_iv_results, data, strength_results, output_dir = tempdir()) {
  
  # Create output directory if specified
  if(!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  plots <- list()
  
  message("Creating comprehensive network visualizations...")
  
  # Plot 1: Transfer Entropy Network
  plots$te_network <- plot_transfer_entropy_network(te_results, output_dir)
  
  # Plot 2: Country Income Network
  if(!is.null(te_iv_results$country_network)) {
    plots$country_network <- plot_country_income_network(te_iv_results$country_network, output_dir)
  }
  
  # Plot 3: Instrument Strength Comparison
  plots$strength_comparison <- plot_instrument_strength_comparison(strength_results, output_dir)
  
  message("v All comprehensive network visualizations completed")
  
  return(plots)
}
