#' ---
#' title: "organize_data_calenviro.R"
#' author: "David"
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory.

# Set working directory
setwd("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj")

# Source in any useful functions
source("check_packages.R")
source("useful_functions.R")

# Load datasets
library(readr)
ces3results_indicators <- read_csv("input/ces3results_indicators.csv")
ces3results_demographics <- read_csv("input/ces3results_demographics.csv")

# Merge datasets
ces3_merged <- merge(ces3results_indicators,ces3results_demographics,by="censustract")
View(ces3_merged)

# Convert race/ethnicity categories to pct. nonwhite
ces3_merged$pctnonwhite <- 100-ces3_merged$whitepct

# Drop categories I don't need
ces3_subsetted <- subset(ces3_merged, select=c("censustract", "totalpop.x", "cesscore.x", "cespercentile.x", 
                                              "ozone", "pm2.5","dieselpm","drinkingwater","pesticides",
                                              "toxrelease", "traffic", "cleanupsite", "groundwater", "hazwastes",
                                              "impwater", "solidwaste","pollutionburden", "pollutionburdenscore",
                                              "lbw", "poverty", "pctnonwhite"))

# Drop missing values for PM2.5 pollution, traffic, drinking water, poverty, 
# pct. nonwhite, and LBW.
ces3_complete <- na.omit(ces3_subsetted)

# Check for missing values
summary(ces3_subsetted$lbw)
summary(ces3_complete$lbw)

# Save the analytical dataset to the output folder
save(ces3_complete, file="~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj/output/ces3_complete.RData") 


