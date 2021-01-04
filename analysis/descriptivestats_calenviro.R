#' ---
#' title: "descriptivestats_calenviro.R"
#' author: "David Purucker"
#' ---

#This script will take the cleaned CalEnviro dataset and produce descriptive statistics showing
#the distribution and relationship of key variables for my analysis: low birth-weight, ozone, PM2.5, and diesel PM.

# Load cleaned dataset
load("~/Desktop/sociology/soc_year2/Soc 412:512 (Stats II)/calenviro_proj/output/ces3_complete.RData")

# Histogram showing the distribution of the dependent variable (lbw)
hist(ces3_complete$lbw, main="Low Birth-Weight by Census Tract", xlab="% of births", ylab="# of Census Tracts", col="hotpink")
summary(ces3_complete$lbw)


# Scatterplot of the bivariate relationship between ozone and lbw
plot(ces3_complete$ozone, ces3_complete$lbw, main="Census Tracts by Ozone and Low Birth-Weight", xlab="Ozone", ylab ="Low Birth-Weight %")
abline(lm(ces3_complete$lbw~ces3_complete$ozone))
lm(ces3_complete$lbw~ces3_complete$ozone)


# Scatterplot of the bivariate relationship between PM2.5 and lbw
plot(ces3_complete$pm2.5, ces3_complete$lbw, main="Census Tracts by PM2.5 and Low Birth-Weight", xlab="PM2.5", ylab="Low Birth-Weight %")
abline(lm(ces3_complete$lbw~ces3_complete$pm2.5))
lm(ces3_complete$lbw~ces3_complete$pm2.5)
