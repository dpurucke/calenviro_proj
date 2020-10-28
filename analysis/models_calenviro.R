#' ---
#' title: "models_calenviro.R"
#' author: "David"
#' ---

# This script will take cleaned data from the output directory and construct regression models to
# test the strength of associations between various measures of environmental pollution burden 
# on the incidence of low birth-weight in California census tracts.

# Set working directory
setwd("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj")

# Load cleaned dataset
load("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj/output/ces3_complete.RData")

# Construct exploratory models
model1 <- lm(lbw~pm2.5+dieselpm, data=ces3_complete)

# Analyze models
summary(model1)
