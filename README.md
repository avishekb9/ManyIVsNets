# ManyIVsNets

[![R-CMD-check](https://github.com/avishekb9/ManyIVsNets/workflows/R-CMD-check/badge.svg)](https://github.com/avishekb9/ManyIVsNets/actions)

## Overview

ManyIVsNets is a comprehensive R package for Environmental Phillips Curve (EPC) analysis featuring state-of-the-art econometric methods and network analysis. 

### Key Results based on example data
- **21 out of 24 instrument approaches** show strong performance (F > 10)
- **Main finding**: 1% ↑ unemployment → 0.071% ↓ CO2 emissions
- **Network density**: Transfer entropy (0.095), Country network (0.25)

## Installation
```r

  #Install from GitHub
  devtools::install_github("avishekb9/ManyIVsNets")

```


## Quick Start

```r

library(ManyIVsNets)

#Run complete analysis pipeline 
results <- run_complete_epc_analysis(
data_file = "epc_data_new_ar5_indicators.csv", # Your data file
output_dir = "epc_analysis_results"
)

#View instrument strength results
print(results$strength_results)

```

## Features 

- **Real multidimensional instruments** from economic/geographic data
- **Transfer entropy causal discovery** using RTransferEntropy
- **24 different instrument approaches** tested
- **Comprehensive network analysis** with country codes

## Citation
If you use this package in your research, please cite:

**APA Style:**
Bhandari, A. (2025). *ManyIVsNets: Environmental Phillips Curve Analysis with Multiple Instrumental Variables and Networks* [Computer software]. GitHub. https://github.com/avishekb9/ManyIVsNets

**Chicago Style:**
Bhandari, Avishek. "ManyIVsNets: Environmental Phillips Curve Analysis with Multiple Instrumental Variables and Networks." GitHub, 2025. https://github.com/avishekb9/ManyIVsNets.
