# Create sample EPC data for the package
# This script creates a representative dataset based on the analysis structure

library(dplyr)
library(usethis)

set.seed(123)

# Create sample countries representing different income groups and regions
countries <- c(
  "United States", "Germany", "Japan", "United Kingdom", "France",
  "Italy", "Spain", "Canada", "Australia", "Netherlands",
  "Sweden", "Norway", "Denmark", "Finland", "Switzerland",
  "Austria", "Belgium", "Ireland", "New Zealand", "Poland",
  "Czech Republic", "Hungary", "Slovakia", "Estonia", "Latvia",
  "Lithuania", "Slovenia", "Croatia", "Portugal", "Greece",
  "Korea, Rep.", "Chile", "Mexico", "Brazil", "Argentina",
  "Russian Federation", "Ukraine", "Turkey", "South Africa", "China",
  "India", "Indonesia", "Thailand", "Malaysia", "Philippines",
  "Viet Nam", "Colombia", "Peru", "Bulgaria", "Romania"
)

# Time period: 1991-2021 (31 years)
years <- 1991:2021

# Create the sample dataset
sample_epc_data <- expand.grid(
  country = countries,
  year = years,
  stringsAsFactors = FALSE
) %>%
  arrange(country, year) %>%
  mutate(
    # Create realistic CO2 emissions per capita (log-normal distribution)
    CO2_per_capita = case_when(
      country %in% c("United States", "Canada", "Australia") ~ exp(rnorm(n(), 2.8, 0.3)),
      country %in% c("Germany", "Japan", "United Kingdom", "France", "Netherlands", "Sweden", "Norway", "Denmark", "Finland", "Switzerland", "Austria", "Belgium", "Ireland", "New Zealand") ~ exp(rnorm(n(), 2.3, 0.3)),
      country %in% c("Italy", "Spain", "Korea, Rep.", "Poland", "Czech Republic") ~ exp(rnorm(n(), 2.0, 0.3)),
      country %in% c("Hungary", "Slovakia", "Estonia", "Latvia", "Lithuania", "Slovenia", "Croatia", "Portugal", "Greece", "Chile", "Mexico", "Russian Federation", "Turkey") ~ exp(rnorm(n(), 1.7, 0.3)),
      country %in% c("Brazil", "Argentina", "Ukraine", "South Africa", "China", "Thailand", "Malaysia") ~ exp(rnorm(n(), 1.4, 0.3)),
      TRUE ~ exp(rnorm(n(), 1.0, 0.3))
    ),
    
    # Create unemployment rates (realistic ranges by development level)
    UR = case_when(
      country %in% c("Switzerland", "Norway", "Denmark", "Germany", "Netherlands", "Japan") ~ exp(rnorm(n(), 1.2, 0.3)),
      country %in% c("United States", "United Kingdom", "Sweden", "Finland", "Austria", "Canada", "Australia", "New Zealand") ~ exp(rnorm(n(), 1.5, 0.3)),
      country %in% c("France", "Italy", "Belgium", "Ireland", "Korea, Rep.") ~ exp(rnorm(n(), 1.8, 0.3)),
      country %in% c("Spain", "Poland", "Portugal", "Greece", "Slovakia", "Croatia") ~ exp(rnorm(n(), 2.2, 0.3)),
      TRUE ~ exp(rnorm(n(), 1.7, 0.4))
    ),
    
    # Female unemployment (typically slightly higher)
    URF = UR * exp(rnorm(n(), 0.1, 0.2)),
    
    # Male unemployment (typically slightly lower)
    URM = UR * exp(rnorm(n(), -0.05, 0.2)),
    
    # Per capita GDP (realistic by development level)
    PCGDP = case_when(
      country %in% c("Switzerland", "Norway", "United States") ~ exp(rnorm(n(), 11.0, 0.2)),
      country %in% c("Denmark", "Sweden", "Netherlands", "Germany", "Austria", "Finland", "Belgium", "France", "Canada", "Australia", "Japan", "United Kingdom", "Ireland", "New Zealand") ~ exp(rnorm(n(), 10.7, 0.2)),
      country %in% c("Italy", "Spain", "Korea, Rep.", "Israel") ~ exp(rnorm(n(), 10.4, 0.2)),
      country %in% c("Slovenia", "Czech Republic", "Portugal", "Estonia", "Slovakia", "Poland", "Lithuania", "Latvia", "Hungary", "Croatia", "Chile", "Greece") ~ exp(rnorm(n(), 10.0, 0.3)),
      country %in% c("Russian Federation", "Turkey", "Mexico", "Brazil", "Argentina", "Malaysia", "China") ~ exp(rnorm(n(), 9.5, 0.3)),
      TRUE ~ exp(rnorm(n(), 8.8, 0.4))
    ),
    
    # Trade openness (exports + imports as % of GDP)
    Trade = case_when(
      country %in% c("Netherlands", "Belgium", "Ireland", "Switzerland", "Austria", "Denmark", "Sweden", "Germany") ~ exp(rnorm(n(), 4.3, 0.3)),
      country %in% c("Norway", "Finland", "Canada", "New Zealand", "Korea, Rep.", "Malaysia", "Thailand") ~ exp(rnorm(n(), 4.0, 0.3)),
      country %in% c("United Kingdom", "France", "Italy", "Spain", "Poland", "Czech Republic", "Hungary", "Chile") ~ exp(rnorm(n(), 3.7, 0.3)),
      TRUE ~ exp(rnorm(n(), 3.4, 0.4))
    ),
    
    # Renewable energy share (% of total energy consumption)
    RES = case_when(
      country %in% c("Norway", "Sweden", "Finland", "Denmark", "Switzerland", "Austria", "Canada", "New Zealand", "Brazil") ~ exp(rnorm(n(), 3.0, 0.4)),
      country %in% c("Germany", "Spain", "Italy", "Portugal", "France", "United Kingdom") ~ exp(rnorm(n(), 2.5, 0.4)),
      country %in% c("United States", "Netherlands", "Belgium", "Ireland", "Japan", "Australia") ~ exp(rnorm(n(), 2.2, 0.4)),
      TRUE ~ exp(rnorm(n(), 1.8, 0.5))
    ),
    
    # Add time trend to make variables realistic over time
    time_trend = year - 1991,
    
    # Adjust variables for time trends
    CO2_per_capita = CO2_per_capita * exp(time_trend * rnorm(n(), 0.01, 0.005)),
    PCGDP = PCGDP * exp(time_trend * 0.02 + rnorm(n(), 0, 0.01)),
    Trade = Trade * exp(time_trend * 0.015 + rnorm(n(), 0, 0.01)),
    RES = RES * exp(time_trend * 0.03 + rnorm(n(), 0, 0.02))
  ) %>%
  select(-time_trend) %>%
  # Ensure all values are positive and realistic
  mutate(
    CO2_per_capita = pmax(CO2_per_capita, 0.5),
    UR = pmax(pmin(UR, 25), 1),
    URF = pmax(pmin(URF, 30), 1),
    URM = pmax(pmin(URM, 25), 1),
    PCGDP = pmax(PCGDP, 1000),
    Trade = pmax(pmin(Trade, 200), 10),
    RES = pmax(pmin(RES, 80), 0.1)
  )

# Add some missing values to make it realistic
set.seed(456)
missing_indices <- sample(nrow(sample_epc_data), size = floor(nrow(sample_epc_data) * 0.02))
sample_epc_data[missing_indices, sample(c("CO2_per_capita", "UR", "Trade", "RES"), size = length(missing_indices), replace = TRUE)] <- NA

# Save as package data
usethis::use_data(sample_epc_data, overwrite = TRUE)

# Print summary
cat("Sample EPC data created:\n")
cat("Countries:", length(unique(sample_epc_data$country)), "\n")
cat("Years:", min(sample_epc_data$year), "-", max(sample_epc_data$year), "\n")
cat("Total observations:", nrow(sample_epc_data), "\n")
cat("Missing values:", sum(is.na(sample_epc_data)), "\n")
cat("Variables:", paste(names(sample_epc_data), collapse = ", "), "\n")
