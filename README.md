#Taxonomic vs Functional Contributions to Plant-Bumblebee Netwrok Robustness
This repository contains code and data for analyzing temporal changes in bee community composition, network metrics, and network robustness through both a taxonomic and functional lens from 2018-2024.

## Requirements
- R version 4.0+
- Required packages: car, emmeans, vegan, ggplot2, bipartite, ggalluvial, stats, nlme, pracma

##Repository Structure
- “Data_Filtering_and_PCA.R” should be run before all other scripts, as they all use files generated here.
- “Interannual_Turnover_Stats.R”, “Plotting_Network_Null_Models.R” and “Stepwise_Extinction_Model_Final_code.R” can be run in any order after this.
- "Temperature_code.R" can be run seperately from all scripts
