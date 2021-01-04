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
library(stargazer)

# Load cleaned dataset
load("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj/output/ces3_complete.RData")

# Construct model 1 -- all pollution indicators
model1 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste, data=ces3_complete)
summary(model1)
round(coef(model1), 4)

# Construct model 2 -- controlling for poverty
model2 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+poverty, data=ces3_complete)
summary(model2)
round(coef(model2), 4)

# Construct model 3 -- controlling for race
model3 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+pctnonwhite, data=ces3_complete)
summary(model3)
round(coef(model3), 4)

# Construct model 4 -- controling for race and poverty together
model4 <- lm(lbw~ozone+pm2.5+dieselpm+drinkingwater+pesticides+toxrelease+traffic+
               cleanupsite+groundwater+hazwastes+impwater+solidwaste+poverty+pctnonwhite, data=ces3_complete)
summary(model4)
round(coef(model4), 4)

# Test for model fit
???