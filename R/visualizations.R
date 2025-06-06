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

#' Create Regional Network Visualization
#'
#' @param data Enhanced EPC data
#' @param output_dir Directory to save plots (optional)
#' @return ggplot object
#' @export
plot_regional_network <- function(data, output_dir = NULL) {
  tryCatch({
    # Create regional network
    regional_data <- data %>%
      dplyr::filter(!is.na(region_enhanced) & region_enhanced != "Other") %>%
      dplyr::group_by(region_enhanced, country, country_code) %>%
      dplyr::summarise(
        avg_co2 = mean(lnCO2, na.rm = TRUE),
        avg_ur = mean(lnUR, na.rm = TRUE),
        avg_gdp = mean(lnPCGDP, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::filter(!is.na(avg_co2) & !is.na(avg_ur) & !is.na(avg_gdp))
    
    if(nrow(regional_data) > 5) {
      # Calculate regional similarities
      n_countries <- nrow(regional_data)
      regional_matrix <- matrix(0, n_countries, n_countries)
      rownames(regional_matrix) <- regional_data$country
      colnames(regional_matrix) <- regional_data$country
      
      for(i in 1:n_countries) {
        for(j in 1:n_countries) {
          if(i != j) {
            # Same region gets higher weight
            if(regional_data$region_enhanced[i] == regional_data$region_enhanced[j]) {
              regional_matrix[i, j] <- 0.8
            } else {
              # Calculate economic similarity
              gdp_diff <- abs(regional_data$avg_gdp[i] - regional_data$avg_gdp[j])
              max_gdp_diff <- max(abs(regional_data$avg_gdp), na.rm = TRUE)
              if(max_gdp_diff > 0) {
                econ_sim <- 1 - (gdp_diff / max_gdp_diff)
                regional_matrix[i, j] <- max(0, econ_sim * 0.5)
              }
            }
          }
        }
      }
      
      regional_threshold <- 0.4
      regional_adj <- ifelse(regional_matrix > regional_threshold, regional_matrix, 0)
      diag(regional_adj) <- 0
      
      if(sum(regional_adj > 0) > 0) {
        regional_network <- igraph::graph_from_adjacency_matrix(regional_adj, mode = "undirected", weighted = TRUE)
        
        # Add attributes
        igraph::V(regional_network)$region <- regional_data$region_enhanced[match(igraph::V(regional_network)$name, regional_data$country)]
        igraph::V(regional_network)$country_code <- regional_data$country_code[match(igraph::V(regional_network)$name, regional_data$country)]
        igraph::V(regional_network)$avg_co2 <- regional_data$avg_co2[match(igraph::V(regional_network)$name, regional_data$country)]
        
        # Handle missing values
        igraph::V(regional_network)$region[is.na(igraph::V(regional_network)$region)] <- "Other"
        igraph::V(regional_network)$country_code[is.na(igraph::V(regional_network)$country_code)] <- substr(igraph::V(regional_network)$name[is.na(igraph::V(regional_network)$country_code)], 1, 3)
        
        # Create the plot
        p <- ggraph::ggraph(regional_network, layout = "stress") +
          ggraph::geom_edge_link(alpha = 0.4, color = "#95A5A6") +
          ggraph::geom_node_point(ggplot2::aes(color = region, size = avg_co2)) +
          ggraph::geom_node_text(ggplot2::aes(label = country_code), repel = TRUE, size = 2.5) +
          ggplot2::scale_color_viridis_d(name = "Region") +
          ggplot2::scale_size_continuous(name = "Avg CO2", range = c(2, 6)) +
          ggplot2::theme_void() +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = "Regional Network with Country Codes",
            subtitle = paste("Network Density:", round(igraph::edge_density(regional_network), 3))
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
  }, error = function(e) {
    cat("Regional network visualization failed:", e$message, "\n")
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
    migration_data <- data %>%
      dplyr::group_by(country, country_code, income_group) %>%
      dplyr::summarise(
        avg_migration = mean(diaspora_network_strength, na.rm = TRUE),
        avg_co2_growth = mean(c(diff(lnCO2), 0), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::filter(!is.na(avg_migration) & !is.na(avg_co2_growth) & !is.na(income_group))
    
    if(nrow(migration_data) > 10) {
      p <- migration_data %>%
        ggplot2::ggplot(ggplot2::aes(x = avg_migration, y = avg_co2_growth, color = income_group)) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
        ggrepel::geom_text_repel(ggplot2::aes(label = country_code), size = 2.5) +
        ggplot2::scale_color_manual(
          name = "Income Group",
          values = c("High_Income" = "#E74C3C", "Upper_Middle_Income" = "#F39C12",
                     "Lower_Middle_Income" = "#F1C40F", "Low_Income" = "#95A5A6")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
          legend.position = "bottom"
        ) +
        ggplot2::labs(
          title = "Migration Network Impact on CO2 Growth",
          subtitle = "Diaspora network strength vs average CO2 growth by income group",
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
  }, error = function(e) {
    cat("Migration impact visualization failed:", e$message, "\n")
    return(NULL)
  })
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
    
    if(nrow(income_data) > 10) {
      # Create network based on similar income levels and economic patterns
      n_countries <- nrow(income_data)
      income_matrix <- matrix(0, n_countries, n_countries)
      rownames(income_matrix) <- income_data$country
      colnames(income_matrix) <- income_data$country
      
      for(i in 1:n_countries) {
        for(j in 1:n_countries) {
          if(i != j) {
            # Same income group gets higher weight
            if(income_data$income_group[i] == income_data$income_group[j]) {
              income_matrix[i, j] <- 0.7
            } else {
              # Calculate economic similarity
              ur_diff <- abs(income_data$avg_ur[i] - income_data$avg_ur[j])
              gdp_diff <- abs(income_data$avg_gdp[i] - income_data$avg_gdp[j])
              
              max_ur_diff <- max(abs(income_data$avg_ur), na.rm = TRUE)
              max_gdp_diff <- max(abs(income_data$avg_gdp), na.rm = TRUE)
              
              if(max_ur_diff > 0 && max_gdp_diff > 0) {
                econ_sim <- 1 - ((ur_diff / max_ur_diff) + (gdp_diff / max_gdp_diff)) / 2
                income_matrix[i, j] <- max(0, econ_sim * 0.4)
              }
            }
          }
        }
      }
      
      income_threshold <- 0.5
      income_adj <- ifelse(income_matrix > income_threshold, income_matrix, 0)
      diag(income_adj) <- 0
      
      if(sum(income_adj > 0) > 0) {
        income_network <- igraph::graph_from_adjacency_matrix(income_adj, mode = "undirected", weighted = TRUE)
        
        # Add attributes
        igraph::V(income_network)$income_group <- income_data$income_group[match(igraph::V(income_network)$name, income_data$country)]
        igraph::V(income_network)$country_code <- income_data$country_code[match(igraph::V(income_network)$name, income_data$country)]
        igraph::V(income_network)$avg_co2 <- income_data$avg_co2[match(igraph::V(income_network)$name, income_data$country)]
        
        # Handle missing values
        igraph::V(income_network)$income_group[is.na(igraph::V(income_network)$income_group)] <- "Other"
        igraph::V(income_network)$country_code[is.na(igraph::V(income_network)$country_code)] <- substr(igraph::V(income_network)$name[is.na(igraph::V(income_network)$country_code)], 1, 3)
        
        # Create the plot
        p <- ggraph::ggraph(income_network, layout = "stress") +
          ggraph::geom_edge_link(alpha = 0.3, color = "#BDC3C7") +
          ggraph::geom_node_point(ggplot2::aes(color = income_group, size = avg_co2)) +
          ggraph::geom_node_text(ggplot2::aes(label = country_code), repel = TRUE, size = 2.5) +
          ggplot2::scale_color_manual(
            name = "Income Group",
            values = c("High_Income" = "#E74C3C", "Upper_Middle_Income" = "#F39C12",
                       "Lower_Middle_Income" = "#F1C40F", "Low_Income" = "#95A5A6")
          ) +
          ggplot2::scale_size_continuous(name = "Avg CO2", range = c(2, 6)) +
          ggplot2::theme_void() +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
          ) +
          ggplot2::labs(
            title = "Cross-Income CO2 Growth Nexus",
            subtitle = paste("Economic similarity network | Density:", round(igraph::edge_density(income_network), 3))
          )
        
        if(!is.null(output_dir)) {
          ggplot2::ggsave(
            filename = file.path(output_dir, "network_03_cross_income_co2_growth_nexus.png"),
            plot = p, width = 12, height = 8, dpi = 600, bg = "white"
          )
        }
        
        return(p)
      }
    }
  }, error = function(e) {
    cat("Cross-income nexus visualization failed:", e$message, "\n")
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
    # Select key instruments for network analysis
    instrument_vars <- c("geo_isolation", "te_isolation", "tech_composite", 
                        "migration_composite", "financial_composite", "spatial_ur_lag", 
                        "bartik_employment", "migration_network")
    
    # Filter data for complete cases
    instrument_data <- data %>%
      dplyr::select(dplyr::all_of(instrument_vars)) %>%
      dplyr::filter(complete.cases(.))
    
    if(nrow(instrument_data) > 50 && ncol(instrument_data) > 3) {
      # Calculate correlation matrix
      cor_matrix <- cor(instrument_data, use = "complete.obs")
      cor_matrix[is.na(cor_matrix)] <- 0
      
      # Create network based on strong correlations
      cor_threshold <- 0.3
      cor_adj <- ifelse(abs(cor_matrix) > cor_threshold, abs(cor_matrix), 0)
      diag(cor_adj) <- 0
      
      if(sum(cor_adj > 0) > 0) {
        instrument_network <- igraph::graph_from_adjacency_matrix(cor_adj, mode = "undirected", weighted = TRUE)
        
        # Add node attributes
        igraph::V(instrument_network)$instrument_type <- dplyr::case_when(
          grepl("geo", igraph::V(instrument_network)$name) ~ "Geographic",
          grepl("te_", igraph::V(instrument_network)$name) ~ "Transfer_Entropy",
          grepl("spatial", igraph::V(instrument_network)$name) ~ "Spatial",
          grepl("bartik", igraph::V(instrument_network)$name) ~ "Bartik",
          grepl("migration", igraph::V(instrument_network)$name) ~ "Migration",
          TRUE ~ "Other"
        )
        
        # Create the plot
        p <- ggraph::ggraph(instrument_network, layout = "stress") +
          ggraph::geom_edge_link(ggplot2::aes(width = weight, alpha = weight), color = "darkblue") +
          ggraph::geom_node_point(ggplot2::aes(color = instrument_type, size = igraph::degree(instrument_network))) +
          ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
          ggplot2::scale_color_manual(
            name = "Instrument Type",
            values = c(
              "Geographic" = "#d62728",
              "Transfer_Entropy" = "#2ca02c",
              "Spatial" = "#ff7f0e",
              "Bartik" = "#1f77b4",
              "Migration" = "#9467bd",
              "Other" = "#8c564b"
            )
          ) +
          ggplot2::scale_edge_width_continuous(range = c(0.5, 2)) +
          ggplot2::scale_edge_alpha_continuous(range = c(0.3, 0.8)) +
          ggplot2::scale_size_continuous(range = c(3, 8)) +
          ggplot2::theme_void() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray40"),
            legend.position = "bottom",
            plot.background = ggplot2::element_rect(fill = "white", color = NA)
          ) +
          ggplot2::labs(
            title = "Instrument Network with Causal Pathways",
            subtitle = "Correlations between different instrument types",
            caption = "Edge thickness shows correlation strength | Colors show instrument types"
          )
        
        if(!is.null(output_dir)) {
          ggplot2::ggsave(
            filename = file.path(output_dir, "network_05_instruments_causal_pathways.png"),
            plot = p, width = 14, height = 10, dpi = 600, bg = "white"
          )
        }
        
        return(p)
      }
    }
  }, error = function(e) {
    cat("Instrument causal pathways visualization failed:", e$message, "\n")
    return(NULL)
  })
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
create_comprehensive_network_plots <- function(te_results, te_iv_results, data, strength_results, output_dir = "plots") {
  
  # Create output directory if specified
  if(!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  plots <- list()
  
  cat("Creating comprehensive network visualizations...\n")
  
  # Plot 1: Transfer Entropy Network
  plots$te_network <- plot_transfer_entropy_network(te_results, output_dir)
  
  # Plot 2: Country Income Network
  if(!is.null(te_iv_results$country_network)) {
    plots$country_network <- plot_country_income_network(te_iv_results$country_network, output_dir)
  }
  
  # Plot 3: Cross-Income CO2 Growth Nexus
  plots$income_nexus <- plot_cross_income_co2_nexus(data, output_dir)
  
  # Plot 4: Migration Impact
  plots$migration_impact <- plot_migration_impact(data, output_dir)
  
  # Plot 5: Instrument Causal Pathways
  plots$instrument_pathways <- plot_instrument_causal_pathways(data, output_dir)
  
  # Plot 6: Regional Network
  plots$regional_network <- plot_regional_network(data, output_dir)
  
  # Plot 7: Instrument Strength Comparison
  plots$strength_comparison <- plot_instrument_strength_comparison(strength_results, output_dir)
  
  cat("✓ All comprehensive network visualizations completed\n")
  
  return(plots)
}
