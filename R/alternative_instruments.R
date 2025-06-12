#' Create Alternative State-of-the-Art Instruments
#'
#' @param data Enhanced EPC data
#' @return Data frame with alternative SOTA instruments
#' @export
create_alternative_sota_instruments <- function(data) {
  message("Creating alternative state-of-the-art instruments...")
  
  enhanced_data <- data %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      # SPATIAL LAG INSTRUMENTS
      spatial_lag_ur = dplyr::lag(lnUR, 1),
      spatial_lag_co2 = dplyr::lag(lnCO2, 1),
      spatial_lag_gdp = dplyr::lag(lnPCGDP, 1),
      
      # BARTIK INSTRUMENTS (shift-share approach)
      bartik_employment = lnUR * lnPCGDP / mean(lnPCGDP, na.rm = TRUE),
      bartik_trade = lnTrade * lnPCGDP / mean(lnPCGDP, na.rm = TRUE),
      
      # MIGRATION NETWORK INSTRUMENTS
      migration_network_ur = diaspora_network_strength * lnUR,
      migration_network_effect = english_language_advantage * net_migration_1990s / 100000,
      
      # JUDGE HISTORICAL INSTRUMENTS (based on historical patterns)
      judge_historical_1 = post_communist_transition * time_trend,
      judge_historical_2 = nato_membership_early * (year - 1990),
      judge_historical_3 = (eu_membership_year < 2000) * lnPCGDP,
      
      # NETWORK CLUSTERING INSTRUMENTS
      network_clustering_1 = te_network_degree * te_network_betweenness,
      network_clustering_2 = te_integration * financial_composite,
      
      # SHIFT-SHARE INSTRUMENTS
      shift_share_tech = tech_composite * (year - 1990) / 10,
      shift_share_financial = financial_composite * lnPCGDP,
      
      # GRAVITY TRADE INSTRUMENTS
      gravity_trade_1 = lnTrade * geo_isolation,
      gravity_trade_2 = lnTrade * diaspora_network_strength,
      
      # COMPOSITE ALTERNATIVE INSTRUMENTS
      alternative_composite_1 = scale(spatial_lag_ur)[,1] + scale(bartik_employment)[,1],
      alternative_composite_2 = scale(judge_historical_1)[,1] + scale(network_clustering_1)[,1],
      alternative_composite_3 = scale(shift_share_tech)[,1] + scale(gravity_trade_1)[,1]
    ) %>%
    dplyr::ungroup()
  
  message("v Alternative state-of-the-art instruments created")
  return(enhanced_data)
}
