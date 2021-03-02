library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Poverty Status by County
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Get state data
geography=geo.make(state=09)
yearlist=c(2009:2019)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years
    variable =list()      
    for (k in seq_along(1:59)) {
     number = number=paste0("B17001", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }
    variable <- as.character(variable)    
    data <- try(acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key))
    print(data)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    determined <- data[,1]
    acs.colnames(determined) <- "Poverty Status Determined"
    below <- data[,2]
    acs.colnames(below) <- "Below Poverty"
    percentBelow <- divide.acs(below, determined, method="proportion", verbose=T)
    acs.colnames(percentBelow) <- "Percent Below Poverty"

    under18 <- acsSum(data, c(4:9, 18:23, 33:38, 47:52), "Under 18")
    percentUnder18 <- divide.acs(under18, determined, method="proportion", verbose=T)
    acs.colnames(percentUnder18) <- "Percent Under 18"

    under18Below <- acsSum(data, c(4:9, 18:23),"Under 18 Below Poverty")
    percentUnder18Below <- divide.acs(under18Below, under18, method="proportion", verbose=T)
    acs.colnames(percentUnder18Below) <- "Percent Under 18 Below Poverty"

    numberEstimates <- data.table(
            geo,
            estimate(determined),
            estimate(below),
            estimate(under18),
            estimate(under18Below),
            year,
            race,
            "Number",
            "Poverty Status"
        )
    numberMOES <- data.table(
            geo,
            standard.error(determined) * 1.645,
            standard.error(below) * 1.645,
            standard.error(under18) * 1.645,
            standard.error(under18Below) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "County", "FIPS",
            "Poverty Status Determined",
            "Below Poverty",
            "Under 18",
            "Under 18 Below Poverty",
            "Year", "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Poverty Status",
            value.name="Value",
            value.factor = F
         )

    percentEstimate <- data.table(
                geo,
                estimate(percentBelow),
                estimate(percentUnder18),
                estimate(percentUnder18Below),
                year,
                race,
                "Percent",
                "Poverty Status"
            )
    percentMOES <- data.table(
                geo,
                standard.error(percentBelow) * 1.645,
                standard.error(percentUnder18) * 1.645,
                standard.error(percentUnder18Below) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "County", "FIPS",
            "Below Poverty",
            "Under 18",
            "Under 18 Below Poverty",
            "Year", "Race/Ethnicity",
            "Measure Type",
            "Variable"
        )
    setnames(percentEstimate, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimate, percentMOES),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Poverty Status",
            value.name="Value",
            value.factor = F
         )
    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get county data
geography=geo.make(state=09, county="*")

county_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years
    variable =list()      
    for (k in seq_along(1:59)) {
     number = number=paste0("B17001", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }
    variable <- as.character(variable)    
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key) 
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- gsub(", Connecticut", "", geo$NAME)
    geo$county <- gsub("^", "09", geo$county)
    geo$state <- NULL    
    determined <- data[,1]
    acs.colnames(determined) <- "Poverty Status Determined"
    below <- data[,2]
    acs.colnames(below) <- "Below Poverty"
    percentBelow <- divide.acs(below, determined, method="proportion", verbose=T)
    acs.colnames(percentBelow) <- "Percent Below Poverty"

    under18 <- acsSum(data, c(4:9, 18:23, 33:38, 47:52), "Under 18")
    percentUnder18 <- divide.acs(under18, determined, method="proportion", verbose=T)
    acs.colnames(percentUnder18) <- "Percent Under 18"

    under18Below <- acsSum(data, c(4:9, 18:23),"Under 18 Below Poverty")
    percentUnder18Below <- divide.acs(under18Below, under18, method="proportion", verbose=T)
    acs.colnames(percentUnder18Below) <- "Percent Under 18 Below Poverty"

    numberEstimates <- data.table(
            geo,
            estimate(determined),
            estimate(below),
            estimate(under18),
            estimate(under18Below),
            year,
            race,
            "Number",
            "Poverty Status"
        )
    numberMOES <- data.table(
            geo,
            standard.error(determined) * 1.645,
            standard.error(below) * 1.645,
            standard.error(under18) * 1.645,
            standard.error(under18Below) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "County", "FIPS",
            "Poverty Status Determined",
            "Below Poverty",
            "Under 18",
            "Under 18 Below Poverty",
            "Year", 
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Poverty Status",
            value.name="Value",
            value.factor = F
         )

    percentEstimate <- data.table(
                geo,
                estimate(percentBelow),
                estimate(percentUnder18),
                estimate(percentUnder18Below),
                year,
                race,
                "Percent",
                "Poverty Status"
            )
    percentMOES <- data.table(
                geo,
                standard.error(percentBelow) * 1.645,
                standard.error(percentUnder18) * 1.645,
                standard.error(percentUnder18Below) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "County", "FIPS",
            "Below Poverty",
            "Under 18",
            "Under 18 Below Poverty",
            "Year",
            "Race/Ethnicity",            
            "Measure Type",
            "Variable"
        )
    setnames(percentEstimate, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimate, percentMOES),
            id.vars=c("County", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Poverty Status",
            value.name="Value",
            value.factor = F
         )
    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
  }
  county_data <- rbind(county_data, inter_data)
}

dataset <- rbind(state_data, county_data)


#Final Additions, processing
dataset[,`:=`(
    Age = ifelse(str_count(`Poverty Status`, "18") > 0, "Under 18", "Total"),
    `Poverty Status` = ifelse(str_count(`Poverty Status`, "Below") > 0, "Below Poverty Level", "Poverty Status Determined"),
    Value = ifelse(`Measure Type` == "Number", round(Value, 2), round(Value*100, 2))
)]

#set final column order
dataset <- dataset %>% 
  select(County, FIPS, Year, `Race/Ethnicity`, Age, `Poverty Status`, `Measure Type`, Variable, Value) %>% 
  arrange(County, FIPS, Year, `Race/Ethnicity`, Age, `Poverty Status`, `Measure Type`)

write.table(
    dataset,
    file.path("data", "poverty-status-by-county-2019.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)


