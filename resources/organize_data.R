#' ---
#' title: "organize_data.R"
#' author: "David"
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#source in any useful functions
source("check_packages.R")
source("useful_functions.R")


# Load datasets -----------------------------------------------------------
library(readr)
mort1999 = read_csv("mort1999.csv.gz")
mort2000 = read_csv("mort2000.csv.gz")
mort2001 = read_csv("mort2001.csv.gz")
mort2002 = read_csv("mort2002.csv.gz")
mort2003 = read_csv("mort2003.csv.gz")
mort2004 = read_csv("mort2004.csv.gz")

aqi1999_daily = read.csv("daily_aqi_by_county_1999.csv")
aqi2000_daily = read.csv("daily_aqi_by_county_2000.csv")
aqi2001_daily = read.csv("daily_aqi_by_county_2001.csv")
aqi2002_daily = read.csv("daily_aqi_by_county_2002.csv")
aqi2003_daily = read.csv("daily_aqi_by_county_2003.csv")
aqi2004_daily = read.csv("daily_aqi_by_county_2004.csv")

countypop2000 = read.csv("countypop2000.csv")

# Drop unnecessary mortality variables ----------------------------------------------------

mort1999_simple = subset(mort1999, select=c("mandeath", "countyoc", "monthdth"))
mort2000_simple = subset(mort2000, select=c("mandeath", "countyoc", "monthdth"))
mort2001_simple = subset(mort2001, select=c("mandeath", "countyoc", "monthdth"))
mort2002_simple = subset(mort2002, select=c("mandeath", "countyoc", "monthdth"))
mort2003_simple = subset(mort2003, select=c("mandeath", "countyoc", "monthdth"))
mort2004_simple = subset(mort2004, select=c("mandeath", "countyoc", "monthdth"))


# Drop sub-100k counties and create three-digit 'countycode' variable for purposes of merging with AQI dataset --------------------------------------------------

temp_1999 = as.character(mort1999_simple$countyoc)
mort1999_simple$countycode = as.numeric(substr(temp_1999, 3, 5))
mort1999_simple2 = subset(mort1999_simple, mort1999_simple$countycode!=999)

temp_2000 = as.character(mort2000_simple$countyoc)
mort2000_simple$countycode = as.numeric(substr(temp_2000, 3, 5))
mort2000_simple2 = subset(mort2000_simple, mort2000_simple$countycode!=999)

temp_2001 = as.character(mort2001_simple$countyoc)
mort2001_simple$countycode = as.numeric(substr(temp_2001, 3, 5))
mort2001_simple2 = subset(mort2001_simple, mort2001_simple$countycode!=999)

temp_2002 = as.character(mort2002_simple$countyoc)
mort2002_simple$countycode = as.numeric(substr(temp_2002, 3, 5))
mort2002_simple2 = subset(mort2002_simple, mort2002_simple$countycode!=999)

temp_2003 = as.character(mort2003_simple$countyoc)
mort2003_simple$countycode = as.numeric(substr(temp_2003, 3, 5))
mort2003_simple2 = subset(mort2003_simple, mort2003_simple$countycode!=999)

temp_2004 = as.character(mort2004_simple$countyoc)
mort2004_simple$countycode = as.numeric(substr(temp_2004, 3, 5))
mort2004_simple2 = subset(mort2004_simple, mort2004_simple$countycode!=999)

# Separate state character code from county numeric codes to make two-digit 'geonumeric' variable

temp2_1999 = as.character(mort1999_simple2$countyoc)
mort1999_simple2$geonumeric = as.character(substr(temp2_1999, 1, 2))

temp2_2000 = as.character(mort2000_simple2$countyoc)
mort2000_simple2$geonumeric = as.character(substr(temp2_2000, 1, 2))

temp2_2001 = as.character(mort2001_simple2$countyoc)
mort2001_simple2$geonumeric = as.character(substr(temp2_2001, 1, 2))

temp2_2002 = as.character(mort2002_simple2$countyoc)
mort2002_simple2$geonumeric = as.character(substr(temp2_2002, 1, 2))

temp2_2003 = as.character(mort2003_simple2$countyoc)
mort2003_simple2$statecode = as.character(substr(temp2_2003, 1, 2))

temp2_2004 = as.character(mort2004_simple2$countyoc)
mort2004_simple2$statecode = as.character(substr(temp2_2004, 1, 2))

# Make state codes numeric in mort2003 and mort2004 (they are initially coded as characters) ------------------------------------------------

mort2003_simple2$geonumeric[mort2003_simple2$statecode=="AL"]="01"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="AK"]="02"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="AZ"]="04"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="AR"]="05"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="CA"]="06"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="CO"]="08"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="CT"]="09"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="DE"]="10"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="DC"]="11"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="FL"]="12"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="GA"]="13"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="HI"]="15"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="ID"]="16"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="IL"]="17"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="IN"]="18"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="IA"]="19"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="KS"]="20"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="KY"]="21"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="LA"]="22"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="ME"]="23"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MD"]="24"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MA"]="25"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MI"]="26"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MN"]="27"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MS"]="28"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MO"]="29"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="MT"]="30"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NE"]="31"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NV"]="32"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NH"]="33"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NJ"]="34"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NM"]="35"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NY"]="36"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="NC"]="37"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="ND"]="38"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="OH"]="39"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="OK"]="40"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="OR"]="41"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="PA"]="42"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="RI"]="44"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="SC"]="45"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="SD"]="46"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="TN"]="47"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="TX"]="48"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="UT"]="49"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="VT"]="50"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="VA"]="51"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="WA"]="53"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="WV"]="54"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="WI"]="55"
mort2003_simple2$geonumeric[mort2003_simple2$statecode=="WY"]="56"

mort2004_simple2$geonumeric[mort2004_simple2$statecode=="AL"]="01"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="AK"]="02"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="AZ"]="04"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="AR"]="05"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="CA"]="06"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="CO"]="08"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="CT"]="09"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="DE"]="10"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="DC"]="11"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="FL"]="12"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="GA"]="13"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="HI"]="15"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="ID"]="16"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="IL"]="17"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="IN"]="18"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="IA"]="19"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="KS"]="20"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="KY"]="21"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="LA"]="22"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="ME"]="23"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MD"]="24"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MA"]="25"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MI"]="26"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MN"]="27"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MS"]="28"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MO"]="29"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="MT"]="30"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NE"]="31"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NV"]="32"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NH"]="33"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NJ"]="34"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NM"]="35"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NY"]="36"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="NC"]="37"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="ND"]="38"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="OH"]="39"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="OK"]="40"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="OR"]="41"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="PA"]="42"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="RI"]="44"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="SC"]="45"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="SD"]="46"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="TN"]="47"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="TX"]="48"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="UT"]="49"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="VT"]="50"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="VA"]="51"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="WA"]="53"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="WV"]="54"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="WI"]="55"
mort2004_simple2$geonumeric[mort2004_simple2$statecode=="WY"]="56"

# Calculate missing values in mandeath ------------------------------------

sum(is.na(mort1999$mandeath))
sum(is.na(mort1999_simple2$mandeath))
sum(is.na(mort2000$mandeath))
sum(is.na(mort2000_simple2$mandeath))
sum(is.na(mort2001$mandeath))
sum(is.na(mort2001_simple2$mandeath))
sum(is.na(mort2002$mandeath))
sum(is.na(mort2002_simple2$mandeath))
sum(is.na(mort2003$mandeath))
sum(is.na(mort2003_simple2$mandeath))
sum(is.na(mort2004$mandeath))
sum(is.na(mort2004_simple2$mandeath))

# There are a LOT of missing values for manner of death in both
# the original dataset and the 100k pop+ dataset (and they are indeed there
# though they don't show up in the visualizer); this is something to resolve eventually

# Recode missing 'mandeath' values as '8' (non-suicides) --------------------

mort1999_simple2$mandeath[mort1999_simple2$mandeath=="NA"]=8
mort2000_simple2$mandeath[mort2000_simple2$mandeath=="NA"]=8
mort2001_simple2$mandeath[mort2001_simple2$mandeath=="NA"]=8
mort2002_simple2$mandeath[mort2002_simple2$mandeath=="NA"]=8
mort2003_simple2$mandeath[mort2002_simple2$mandeath=="NA"]=8
mort2004_simple2$mandeath[mort2004_simple2$mandeath=="NA"]=8

# Generate county-month values --------------------------------------------

mort1999_simple2$countymonth=paste(mort1999_simple2$countyoc, mort1999_simple2$monthdth, sep = ".")
mort2000_simple2$countymonth=paste(mort2000_simple2$countyoc, mort2000_simple2$monthdth, sep = ".")
mort2001_simple2$countymonth=paste(mort2001_simple2$countyoc, mort2001_simple2$monthdth, sep = ".")
mort2002_simple2$countymonth=paste(mort2002_simple2$countyoc, mort2002_simple2$monthdth, sep = ".")
mort2003_simple2$countymonth=paste(mort2003_simple2$countyoc, mort2003_simple2$monthdth, sep = ".")
mort2004_simple2$countymonth=paste(mort2004_simple2$countyoc, mort2004_simple2$monthdth, sep = ".")

# Generate countymonth2 values --------------------------------------------

mort1999_simple2$countymonth2=paste(mort1999_simple2$geonumeric, mort1999_simple2$countycode, mort1999_simple2$monthdth, sep = ".")
mort2000_simple2$countymonth2=paste(mort2000_simple2$geonumeric, mort2000_simple2$countycode, mort2000_simple2$monthdth, sep = ".")
mort2001_simple2$countymonth2=paste(mort2001_simple2$geonumeric, mort2001_simple2$countycode, mort2001_simple2$monthdth, sep = ".")
mort2002_simple2$countymonth2=paste(mort2002_simple2$geonumeric, mort2002_simple2$countycode, mort2002_simple2$monthdth, sep = ".")
mort2003_simple2$countymonth2=paste(mort2003_simple2$geonumeric, mort2003_simple2$countycode, mort2003_simple2$monthdth, sep = ".")
mort2004_simple2$countymonth2=paste(mort2004_simple2$geonumeric, mort2004_simple2$countycode, mort2004_simple2$monthdth, sep = ".")

# We coded in both formats in order to be compatible with the AQI - we'll find out soon which one is appropriate

# Recode suicides (coded as '2' and '6') as true / non-suicides as false -------------------------

mort1999_simple2$suicide=mort1999_simple2$mandeath==2|mort1999_simple2$mandeath==6
mort2000_simple2$suicide=mort2000_simple2$mandeath==2|mort2000_simple2$mandeath==6
mort2001_simple2$suicide=mort2001_simple2$mandeath==2|mort2001_simple2$mandeath==6
mort2002_simple2$suicide=mort2002_simple2$mandeath==2|mort2002_simple2$mandeath==6
mort2003_simple2$suicide=mort2003_simple2$mandeath==2|mort2003_simple2$mandeath==6
mort2004_simple2$suicide=mort2004_simple2$mandeath==2|mort2004_simple2$mandeath==6

# There are still quite a few N/As in here as of 5/16/19

# Convert observations to county-months -----------------------------------
# The 'suicide' column is a proportion of all deaths in the county-month
# In observation 1, suicides made up about 1% [I think this is the correct interpretation]
# of deaths in Alabama's county whatever in January of 1999.

# But wait - why are there many observations where the suicide rate seems WAY too high?
# e.g. in observation number 769 (and like 8 others), the suicide rate is 1.000.
# Also, we've just discovered that the Panama Canal Zone (07) is listed in our data; so is Guam (14) and Puerto Rico (43)
# I thought I'd removed territories; I need to do this
# But the bigger mystery is these very high suicide rates even in the state counties I'd intended to include
# Ok, now I'm noticing that the state counties with 
# e.g. observation 5222 (a sub-100k pop county in Texas) or observation 582 (a sub-100k pop county in Arkansas)
# but observation 1325 is a Florida county with a pop of 170k and a 
# county-month suicide proportion of 66%; is this plausible? maybe, barely
# However, the trend for all of these high-suicide proportions is sub-100k counties, mostly in territories;
# removing these should solve most of the problem

# the territories?
# the sub-100k counties?

# the MASSACHUSETTS COUNTY (observation 2620) IS NOT A REAL COUNTY - it may be a metro area, need to research in codebook
# there are indeed territories included in the mortality dataset but for now we're going to keep them; they may just be ignored in the merge

mort1999_condensed=aggregate(suicide~countymonth2, mort1999_simple2, sum)
mort2000_condensed=aggregate(suicide~countymonth2, mort2000_simple2, mean)
mort2001_condensed=aggregate(suicide~countymonth2, mort2001_simple2, mean)
mort2002_condensed=aggregate(suicide~countymonth2, mort2002_simple2, mean)
mort2003_condensed=aggregate(suicide~countymonth2, mort2003_simple2, mean)
mort2004_condensed=aggregate(suicide~countymonth2, mort2004_simple2, mean)

# Calculate unique counties and county-equivalents ------------------------

length(unique((mort2004_simple2$countyoc)))

# Drop unnecessary variables in AQI datasets -----------------------------

aqi1999_daily_simple = subset(aqi1999_daily, select=c("State.Code", "County.Code", "Date", "AQI"))
aqi2000_daily_simple = subset(aqi2000_daily, select=c("State.Code", "County.Code", "Date", "AQI"))
aqi2001_daily_simple = subset(aqi2001_daily, select=c("State.Code", "County.Code", "Date", "AQI"))
aqi2002_daily_simple = subset(aqi2002_daily, select=c("State.Code", "County.Code", "Date", "AQI"))
aqi2003_daily_simple = subset(aqi2003_daily, select=c("State.Code", "County.Code", "Date", "AQI"))
aqi2004_daily_simple = subset(aqi2004_daily, select=c("State.Code", "County.Code", "Date", "AQI"))

# Add a statecounty variable
aqi2004_daily_simple$statecounty = paste(aqi2004_daily_simple$State.Code, aqi2004_daily_simple$County.Code, sep = ".")
length(unique(aqi2004_daily_simple$statecounty))

# Check for missing values in AQI

sum(is.na(aqi1999_daily_simple$AQI))
sum(is.na(aqi2000_daily_simple$AQI))
sum(is.na(aqi2001_daily_simple$AQI))
sum(is.na(aqi2002_daily_simple$AQI))
sum(is.na(aqi2003_daily_simple$AQI))
sum(is.na(aqi2004_daily_simple$AQI))
    
# Aggregate days to months in AQI datasets ------------------------------------------------

tempaqi_1999 = as.character(aqi1999_daily_simple$Date)
aqi1999_daily_simple$DateSep = as.numeric(substr(tempaqi_1999, 6, 7))
aqi1999_daily_simple$countymonth2=paste(aqi1999_daily_simple$State.Code,
                                        aqi1999_daily_simple$County.Code, aqi1999_daily_simple$DateSep, sep = ".")

tempaqi_2000 = as.character(aqi2000_daily_simple$Date)
aqi2000_daily_simple$DateSep = as.numeric(substr(tempaqi_2000, 6, 7))
aqi2000_daily_simple$countymonth2=paste(aqi2000_daily_simple$State.Code, 
                                        aqi2000_daily_simple$County.Code, aqi2000_daily_simple$DateSep, sep = ".")

tempaqi_2001 = as.character(aqi2001_daily_simple$Date)
aqi2001_daily_simple$DateSep = as.numeric(substr(tempaqi_2001, 6, 7))
aqi2001_daily_simple$countymonth2=paste(aqi2001_daily_simple$State.Code, 
                                        aqi2001_daily_simple$County.Code, aqi2001_daily_simple$DateSep, sep = ".")

tempaqi_2002 = as.character(aqi2002_daily_simple$Date)
aqi2002_daily_simple$DateSep = as.numeric(substr(tempaqi_2002, 6, 7))
aqi2002_daily_simple$countymonth2=paste(aqi2002_daily_simple$State.Code, 
                                        aqi2002_daily_simple$County.Code, aqi2002_daily_simple$DateSep, sep = ".")

tempaqi_2003 = as.character(aqi2003_daily_simple$Date)
aqi2003_daily_simple$DateSep = as.numeric(substr(tempaqi_2003, 6, 7))
aqi2003_daily_simple$countymonth2=paste(aqi2003_daily_simple$State.Code, 
                                        aqi2003_daily_simple$County.Code, aqi2003_daily_simple$DateSep, sep = ".")

tempaqi_2004 = as.character(aqi2004_daily_simple$Date)
aqi2004_daily_simple$DateSep = as.numeric(substr(tempaqi_2004, 6, 7))
aqi2004_daily_simple$countymonth2=paste(aqi2004_daily_simple$State.Code, 
                                        aqi2004_daily_simple$County.Code, aqi2004_daily_simple$DateSep, sep = ".")

# Calculate mean monthly AQI values per county-month ----------------------

aqi1999_condensed = aggregate(AQI~countymonth2, aqi1999_daily_simple, mean)
aqi2000_condensed = aggregate(AQI~countymonth2, aqi2000_daily_simple, mean)
aqi2001_condensed = aggregate(AQI~countymonth2, aqi2001_daily_simple, mean)
aqi2002_condensed = aggregate(AQI~countymonth2, aqi2002_daily_simple, mean)
aqi2003_condensed = aggregate(AQI~countymonth2, aqi2003_daily_simple, mean)
aqi2004_condensed = aggregate(AQI~countymonth2, aqi2004_daily_simple, mean)

# how many counties are in AQI?

length(unique(aqi2004_daily_simple$County.Code))

# Check myself before I wreck myself
sum(duplicated(mort1999_condensed$countymonth2))
sum(duplicated(aqi1999_condensed$countymonth2))

fulldata = merge(mort1999_condensed, aqi1999_condensed, 
             by="countymonth2", all.x=FALSE, all.y=TRUE)
fulldata$suicide[is.na(fulldata$suicide)] <- 0
fulldata$county = as.numeric(substr(fulldata$countymonth2, ))



#full model will look like this
glm(suicide~AQI+county+month, data=fulldata, family=poisson, offset=log(pop))

# there is a very small degree of overlap (649 county-months), indicating that many counties are not compatible between datasets

sum(is.na(test$suicide))
sum(is.na(test$AQI))
sum(!is.na(test$suicide) & !is.na(test$AQI))

length(unique(mort1999_condensed$countymonth2))
length(unique($countymonth2))

merge
addyear
rbind

# Once they all look the same, rbind(mort1999,mort2000,etc.)

as.data.frame.table(tapply(mort2004_simple2$mandeath==2, mort2004_simple2[,c("countymonth2","mandeath")], sum))
x
