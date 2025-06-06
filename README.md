# ManyIVsNets

[![R-CMD-check](https://github.com/avishekb9/ManyIVsNets/workflows/R-CMD-check/badge.svg)](https://github.com/avishekb9/ManyIVsNets/actions)

## Overview

ManyIVsNets is a comprehensive R package for Environmental Phillips Curve (EPC) analysis featuring state-of-the-art econometric methods and network analysis. Based on your complete analysis of 49 countries (1991-2021) with 3,201 observations and 85 created variables.

### Key Results from Your Analysis
- **21 out of 24 instrument approaches** show strong performance (F > 10)
- **Best performing instrument**: Judge Historical SOTA (F = 7,155.39)
- **Main finding**: 1% ↑ unemployment → 0.071% ↓ CO2 emissions
- **Network density**: Transfer entropy (0.095), Country network (0.25)

## Installation
Install from GitHub
devtools::install_github("yourusername/ManyIVsNets")


## Quick Start

library(ManyIVsNets)

Run complete analysis pipeline 
results <- run_complete_epc_analysis(
data_file = "epc_data_new_ar5_indicators.csv", # Your data file
output_dir = "epc_analysis_results"
)

View instrument strength results
print(results$strength_results)


## Features Based on Your Code

- **Complete from-scratch analysis** - no CSV dependencies
- **Real multidimensional instruments** from economic/geographic data
- **Transfer entropy causal discovery** using RTransferEntropy
- **24 different instrument approaches** tested
- **7 publication-quality visualizations** (600 DPI)
- **Comprehensive network analysis** with country codes

## Citation

If you use ManyIVsNets, please cite:

Bhandari, Avishek (2025). ManyIVsNets: Environmental Phillips Curve Analysis
with Multiple Instrumental Variables and Networks.
R package version 0.1.0. https://github.com/avishekb9/ManyIVsNets
