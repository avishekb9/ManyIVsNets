# Test helper functions for ManyIVsNets package
# These functions are available during both testing and interactive development

#' Create test EPC data for testing
#' @return Data frame with test EPC data
create_test_epc_data <- function() {
  data.frame(
    country = rep(c("United States", "Germany", "Japan", "United Kingdom", "France"), each = 10),
    year = rep(2000:2009, 5),
    CO2_per_capita = exp(rnorm(50, 2.5, 0.3)),
    UR = exp(rnorm(50, 1.5, 0.2)),
    URF = exp(rnorm(50, 1.6, 0.2)),
    URM = exp(rnorm(50, 1.4, 0.2)),
    PCGDP = exp(rnorm(50, 10.5, 0.3)),
    Trade = exp(rnorm(50, 4, 0.2)),
    RES = exp(rnorm(50, 2.5, 0.3)),
    stringsAsFactors = FALSE
  )
}

#' Create test instruments for testing
#' @return Data frame with test instruments
create_test_instruments <- function() {
  countries <- c("United States", "Germany", "Japan", "United Kingdom", "France")
  data.frame(
    country = countries,
    iso3c = c("USA", "DEU", "JPN", "GBR", "FRA"),
    geo_isolation = c(0.4, 0.1, 0.8, 0.7, 0.1),
    
    # Technology instruments - COMPLETE SET
    internet_adoption_lag = c(5, 8, 8, 5, 8),
    mobile_infrastructure_1995 = c(0.8, 0.8, 0.8, 0.8, 0.7),
    telecom_development_1995 = c(0.8, 0.7, 0.6, 0.7, 0.7),
    
    # Migration instruments - COMPLETE SET
    diaspora_network_strength = c(0.2, 0.4, 0.5, 0.4, 0.4),
    english_language_advantage = c(1.0, 0.8, 0.2, 1.0, 0.6),
    migration_cost_index = c(0.8, 0.6, 0.5, 0.6, 0.6),
    net_migration_1990s = c(172060, 160802, -19641, 93356, 64473),
    
    # Geopolitical instruments - COMPLETE SET
    post_communist_transition = c(0, 0, 0, 0, 0),
    nato_membership_early = c(1, 1, 0, 1, 1),
    eu_membership_year = c(9999, 1957, 9999, 1973, 1957),
    cold_war_western = c(1, 1, 1, 1, 1),
    un_membership_early = c(1945, 1955, 1955, 1945, 1945),
    
    # Natural risk instruments - COMPLETE SET
    seismic_risk_index = c(0.7, 0.1, 0.9, 0.1, 0.1),
    island_isolation = c(0, 0, 1, 1, 0),
    landlocked_status = c(0, 0, 0, 0, 0),
    climate_volatility_1960_1990 = c(0.8, 0.2, 0.5, 0.2, 0.2),
    volcanic_risk = c(0.6, 0.1, 0.9, 0.1, 0.1),
    
    # Financial instruments - COMPLETE SET
    financial_market_maturity = c(1.0, 0.95, 0.95, 1.0, 0.95),
    banking_development_1990 = c(0.9, 0.9, 0.9, 0.9, 0.9),
    financial_openness_1990 = c(0.95, 0.8, 0.8, 0.95, 0.8),
    stock_market_development_1990 = c(0.9, 0.9, 0.9, 0.9, 0.9),
    
    stringsAsFactors = FALSE
  )
}

#' Create enhanced test data with all required variables
#' @return Data frame with enhanced test data
create_enhanced_test_data <- function() {
  test_data <- create_test_epc_data()
  test_instruments <- create_test_instruments()
  
  # Add log variables
  test_data <- test_data %>%
    dplyr::mutate(
      lnCO2 = log(pmax(CO2_per_capita, 0.01)),
      lnUR = log(pmax(UR, 0.01)),
      lnURF = log(pmax(URF, 0.01)),
      lnURM = log(pmax(URM, 0.01)),
      lnPCGDP = log(pmax(PCGDP, 1)),
      lnTrade = log(pmax(Trade, 0.01)),
      lnRES = log(pmax(RES, 0.01)),
      time_trend = year - min(year),
      income_group = rep(c("High_Income", "Upper_Middle_Income"), length.out = dplyr::n()),
      region_enhanced = rep(c("North_America", "Europe", "Asia"), length.out = dplyr::n()),
      country_code = rep(c("USA", "DEU", "JPN", "GBR", "FRA"), each = 10)
    )
  
  # Create composite instruments with the corrected function
  test_instruments_complete <- create_composite_instruments(test_instruments)
  
  # Merge data
  enhanced_data <- test_data %>%
    dplyr::left_join(test_instruments_complete, by = "country") %>%
    dplyr::mutate(
      # Add missing alternative SOTA instruments with variation
      spatial_lag_ur = dplyr::lag(lnUR, 1),
      bartik_employment = lnUR * lnPCGDP / mean(lnPCGDP, na.rm = TRUE) + rnorm(dplyr::n(), 0, 0.1),
      migration_network_ur = diaspora_network_strength * lnUR + rnorm(dplyr::n(), 0, 0.1),
      judge_historical_1 = post_communist_transition * time_trend + rnorm(dplyr::n(), 0, 0.1),
      network_clustering_1 = rnorm(dplyr::n(), 0, 1),
      shift_share_tech = tech_composite * (year - 2000) / 10 + rnorm(dplyr::n(), 0, 0.1),
      gravity_trade_1 = lnTrade * geo_isolation + rnorm(dplyr::n(), 0, 0.1),
      
      # TE-based instruments with variation
      te_isolation = runif(dplyr::n(), 0.2, 0.8),
      te_bridging = runif(dplyr::n(), 0.1, 0.6),
      te_integration = runif(dplyr::n(), 0.3, 0.7),
      te_influence = runif(dplyr::n(), 0.2, 0.8)
    )
  
  return(enhanced_data)
}
