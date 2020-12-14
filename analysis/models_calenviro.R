#' ---
#' title: "models_calenviro.R"
#' author: "David Purucker"
#' ---

# This script will take cleaned data from the output directory and construct regression models to
# test the strength of association between various measures of environmental pollution burden 
# on the incidence of low birth-weight in California census tracts. It includes a variety of
# sensitivity analysis tests to ensure that the models are appropriately designed.

# Set working directory
setwd("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj")

# Load cleaned dataset
load("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj/output/ces3_complete.RData")

# Construct models -- all pollution indicators
model1 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste, data=ces3_complete)
summary(model1)
round(coef(model1), 4)

# Construct models -- only pollution burden
model2 <- lm(lbw~pollutionburden, data=ces3_complete)
summary(model2)

# Construct models -- controlling for poverty
model3 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+poverty, data=ces3_complete)
summary(model3)
round(coef(model3), 4)

# Construct models -- controlling for race
model4 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+pctnonwhite, data=ces3_complete)
summary(model4)
round(coef(model4), 4)

# Construt models -- controling for race and poverty together
model5 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+poverty+pctnonwhite, data=ces3_complete)
summary(model5)
round(coef(model5), 4)

# Analyze models -- looking for what pollution indicators are most significant, 
# so we can assemble interaction models
summary(model1)
round(coef(model1), 4)

#we need to adjust the ozone variable because a 1-unit increase is outside the range of the data; The range of the ozone data is from __________ to ____________, 
#so a better way to interpret this data is as a <.01 unit increase in ozone is associated with _____>.

#a 1-unit [look up what the units are] increase in diesel particulate matter is associated with a 0.0147 percentage point increase in the LBW rate in a census tract, on average

#ozone, PM2.5, diesel PM (air pollution) are the pollution indicators to focus on