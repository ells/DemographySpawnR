library(dplyr)
library(readr)
library(zipcode)
library(readxl)
library(expss)

data(zipcode)
colnames(zipcode)

zipcode$zip3Char = substr(zipcode$zip, 1, 3)
zipToState= unique(zipcode[, c("zip3Char", "state")])

setwd("/Users/ishaandave/Desktop/CDC-Leidos/data/AIDS Vu PrEP/")
dataFiles <- lapply(Sys.glob("AIDSVu_Zip3_PrEP_*.xlsx"), read_xlsx)
dataFiles

zipDataAll <- do.call("rbind", dataFiles)

zipDataAll$zip3Char = sprintf("%03d", zipDataAll$Zip3)

allData = merge(zipDataAll, zipToState, by = "zip3Char")
# ## Added rows because zip codes such as 
#     020 has Massachussets and Rhode Island
#     030 has Maine and New Hampshire
#     063 has CT and NY
#     ...
#   
# wnat to be able to specfify 
# draw a population of n individuals
#   using these data
# 
#   just try to write something out from 1 year/1 state 
#   


ny2017 = subset(allData, state == "NY" & Year == 2017)
names(ny2017)

ny2017_complete = ny2017[ny2017$`Male PrEP Users` > -1, ]

age13to24 = as.numeric(sum_col_if(gt(-1), ny2017_complete$`Age LE 24 PrEP Users`))
age25to34 = as.numeric(sum_col_if(gt(-1), ny2017_complete$`Age 25-34 PrEP Users`))
age35to44 = as.numeric(sum_col_if(gt(-1), ny2017_complete$`Age 35-44 PrEP Users`))
age45to54 = as.numeric(sum_col_if(gt(-1), ny2017_complete$`Age 45-54 PrEP Users`))
age55     = as.numeric(sum_col_if(gt(-1), ny2017_complete$`Age 55+ PrEP Users`))


n = 1000
set.seed(1234)

patients <- tibble(
        `Age` = sample(c('13-24', '25-34', '35-44', '45-54', '55+'), n, TRUE, 
                       c(age13to24, age25to34, age35to44, age45to54, age55)), 
        `Sex` = sample(c("Male", "Female"), n, TRUE, 
                       prob = c(sum(ny2017_complete$`Male PrEP Users`)/sum(ny2017_complete$`Zip3 PrEP Users`), 
                               (sum(ny2017_complete$`Female PrEP Users`)/sum(ny2017_complete$`Zip3 PrEP Users`)))),
)




merge with census data from each county









n <- 1000

patients <- tibble(
  `Age at Diagnosis` = sample(c('13-24', '25-34', '35-44', '45-54', '55+'), n, TRUE, c(1675, 14740, 9943, 6490, 4882, 1930)),
  `Sex at Birth` = sample(c('Male', 'Female'), n, TRUE, prob = c(.81, .19)),
  `Race/Ethnicity` = sample(c("White", "Hispanic/Latinx", "Black", "American Indian/Alaska Native", "Asian", "Pacific Islander", "Multiple"), n, TRUE, c(10329, 9750, 17450, 243, 969, 48, 871)),
  Region = sample(c('South', 'Northeast', 'West', 'Midwest'), n, TRUE, c(16.8, 11.2, 10.2, 7.5)),
  PWID = sample(c("PWID", "Non-PWID"), n, TRUE, c(.09, .91))
)

patients %>%
  mutate(
    Gender = ifelse(`Sex at Birth` == 'Male',
                    ifelse(runif(n()) < 0.02125, 'Trans Woman', 'Cis Man'),
                    ifelse(runif(n()) < 0.015, 'Trans Man', 'Cis Woman')
    ),
    MSM = ifelse(`Sex at Birth` == "Male" & runif(n()) < .8645, "MSM", "Non-MSM"),
    `Transmission Category` =
      ifelse(MSM == 'MSM' & PWID == 'PWID', 'MSM who Inject Drugs',
             ifelse(MSM == 'MSM', 'MSM',
                    ifelse(PWID == 'PWID', 'People who Inject Drugs',
                           ifelse(runif(n()) > .01, 'Heterosexual', 'Other')))
      ),
    Care = sample(c("In HIV Care", "Not In HIV Care"), n, TRUE, c(73, 27)),
    Year = sample(c("2014 or Prior", 2015, 2016, 2017, 2018), n, TRUE, c(n-30, 4, 6, 3, 20)),
    Cluster = sample(c('A', 'B', 'C', 'D', 'E', 'Not in a Growing Cluster'), n, TRUE, c(21, 17, 13, 9, 5, n-65))
  ) %>%
  select(`Age at Diagnosis`, `Sex at Birth`, Gender, `Race/Ethnicity`, Region, `Transmission Category`, Care, Year, Cluster) -> patients

write_csv(patients, 'fakeinfectedpatients2.csv')