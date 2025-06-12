#' Load and Clean EPC Data
#'
#' @param file_path Path to the EPC data CSV file
#' @return Cleaned EPC data frame
#' @export
#' @examples
#' \dontrun{
#' epc_data <- load_epc_data_corrected("epc_data_new_ar5_indicators.csv")
#' }
load_epc_data_corrected <- function(file_path = "epc_data_new_ar5_indicators.csv") {
  if(file.exists(file_path)) {
    message("Loading AR5 EPC data...")
    epc_data <- readr::read_csv(file_path, show_col_types = FALSE)
    message("v AR5 EPC data loaded: ", nrow(epc_data), " observations")
  } else {
    stop("AR5 EPC data not found. Please ensure the CSV file is available.")
  }
  
  # Clean and prepare data
  epc_data_clean <- epc_data %>%
    dplyr::filter(!is.na(CO2_per_capita) & !is.na(UR) & !is.na(PCGDP) & !is.na(Trade) & !is.na(RES)) %>%
    dplyr::mutate(
      # Create log variables
      lnCO2 = log(pmax(CO2_per_capita, 0.01)),
      lnUR = log(pmax(UR, 0.01)),
      lnURF = log(pmax(URF, 0.01)),
      lnURM = log(pmax(URM, 0.01)),
      lnPCGDP = log(pmax(PCGDP, 1)),
      lnTrade = log(pmax(Trade, 0.01)),
      lnRES = log(pmax(RES, 0.01)),
      
      # Income classification
      income_group = dplyr::case_when(
        PCGDP > 40000 ~ "High_Income",
        PCGDP > 15000 ~ "Upper_Middle_Income",
        PCGDP > 5000 ~ "Lower_Middle_Income",
        TRUE ~ "Low_Income"
      ),
      
      # Regional classification
      region_enhanced = dplyr::case_when(
        country %in% c("Germany", "France", "Italy", "Spain", "Netherlands", "Belgium", "Austria") ~ "Western_Europe",
        country %in% c("Poland", "Czech Republic", "Hungary", "Slovakia", "Estonia", "Latvia", "Lithuania") ~ "Eastern_Europe",
        country %in% c("United States", "Canada") ~ "North_America",
        country %in% c("United Kingdom", "Ireland") ~ "British_Isles",
        country %in% c("Sweden", "Norway", "Denmark", "Finland") ~ "Nordic",
        country %in% c("Japan", "Korea, Rep.") ~ "East_Asia",
        country %in% c("China", "India", "Indonesia", "Thailand", "Malaysia", "Philippines", "Vietnam") ~ "Emerging_Asia",
        country %in% c("Australia", "New Zealand") ~ "Oceania",
        country %in% c("Brazil", "Mexico", "Chile", "Colombia", "Peru") ~ "Latin_America",
        country %in% c("Russian Federation", "Ukraine") ~ "Eastern_Europe_CIS",
        country %in% c("Turkey") ~ "Middle_East",
        country %in% c("South Africa") ~ "Africa",
        TRUE ~ "Other"
      ),
      
      # Create country codes for network visualization
      country_code = dplyr::case_when(
        country == "United States" ~ "USA",
        country == "United Kingdom" ~ "GBR",
        country == "Korea, Rep." ~ "KOR",
        country == "Russian Federation" ~ "RUS",
        TRUE ~ countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE)
      ),
      
      # Time trend
      time_trend = year - min(year, na.rm = TRUE)
    ) %>%
    dplyr::arrange(country, year)
  
  return(epc_data_clean)
}
