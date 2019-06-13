
library(plyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(expss)
library(tibble)

setwd("/Users/ishaandave/Desktop/CDC-Leidos/Data/AIDS Vu State New Diagnoses/")
dataFiles <- lapply(Sys.glob("AIDSVu_State_New-DX-Data-Sets_*.xlsx"), read_xlsx)
dataFiles

allStateData <- do.call("rbind", dataFiles)


census = fread("/Users/ishaandave/Desktop/CDC-Leidos/Data/Census/cc-est2017-alldata.csv")
#want only measurements made on July 1, 2010 and later
# First 3 columns are useless
census2 = census[census$YEAR>2, c(-1, -2, -3)]

censusStateLevel = subset(census2, AGEGRP == 0)

censusStateLevel$YEAR_new = censusStateLevel$YEAR + 2007

names(censusStateLevel)[names(censusStateLevel) == "YEAR_new"] = "Year"
names(censusStateLevel)[names(censusStateLevel) == "STNAME"] = "State"


try = censusStateLevel %>% 
  group_by(State, Year) %>%
  summarise(pop=sum(TOT_POP))

censusNo2017 = try[try$Year != 2017,]
censusNo2017$State = ifelse(censusNo2017$State == "District of Columbia","Washington, D.C.", censusNo2017$State)

## At this point, census data is population in each 2010 - 2017 for each state 
## State data from AIDS Vu has Puerto Rico, census doesn't, so we remove it 

allStateData_noPR = allStateData[allStateData$State != "Puerto Rico",]

mergedCensusState = merge(allStateData_noPR, censusNo2017, by = c("State", "Year"))

mergedCensusState2 = mergedCensusState[, c(1,2,3,ncol(mergedCensusState), 5:ncol(mergedCensusState)-1)]


calc = (mergedCensusState2$`New Diagnoses State Cases`/mergedCensusState2$pop*100000 - mergedCensusState2$`New Diagnoses State Rate`)/mergedCensusState2$`New Diagnoses State Rate`
# mean(calc) ~ 17% difference between calculated and actual

plot(density(calc))

# pretty normally distributed

## Calculated rate of [new diagnoses] / population are ~ 17 