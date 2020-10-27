#' ---
#' title: "models_calenviro.R"
#' author: "David"
#' ---

# This script will take cleaned data from the output directory and construct regression models to
# test the strength of associations between various measures of environmental pollution burden 
# on the incidence of low birth-weight in California census tracts.

# Construct exploratory models
model1 <- lm_svy_mi()