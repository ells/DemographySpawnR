library(readxl)
library(data.table)
library(tidyverse)


# dat = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/mess around.csv")
dat = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/pretend demographics.csv")
# dat = as.data.frame(read_xlsx("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/pretend demographics.xlsx"))
names(dat)

names(dat)<- tolower(names(dat))

gender = dat[, grepl("gender|sex", names(dat))]; gender
 
ageVALUE = dat[, grepl("age", names(dat))]; ageVALUE

raceVALUE = dat[, grepl("race", names(dat))]; raceVALUE

edu = dat[, grepl("edu", names(dat))]; edu
# 
# hisp = dat[, grepl("hisp", names(dat))]; hisp 
# 

n = 1000
set.seed(1234)
patients <- tibble( 
  `Gender`                = sample(c(as.character(as.data.frame(table(gender))$gender)), n, TRUE, 
                             prob = c(as.data.frame(table(gender))$Freq)
                                  ),
  `Race/Ethnicity`        = sample(c(as.character(as.data.frame(table(raceVALUE))$raceVALUE)), n, TRUE,
                              prob = c(as.data.frame(table(raceVALUE))$Freq)
                                  ),
  `Age`                   = round(rnorm(n, mean(ageVALUE), sd = sqrt(var(ageVALUE)))
                                  ),
  `Education Categorical` = sample(c(as.character(as.data.frame(table(edu))$edu)), n, TRUE,
                                   prob = c(as.data.frame(table(edu))$Freq)
                                  ),
  `Education Continuous`  = round(rnorm(n, mean(edu), sd = sqrt(var(edu)))
                                 )
                   )
  
head(patients)
table(patients$Gender)
table(patients$`Race/Ethnicity`)
table(patients$`Education Categorical`)

c(mean(patients$Age), var(patients$Age))
















# 
# listVars <- c("Age", "Gender", "Race/Ethnicity", "Education Categorical", "Education Continuous")
# catVars = c("Gender", "Race/Ethnicity", "Education Categorical")
# table1 =  CreateTableOne(vars = listVars, data = patients, factorVars = catVars)
# 
# # Create a temp file
# tmp <- tempfile(fileext = ".docx")
# 
# # Create a docx file
# read_docx() %>% 
#   body_add_flextable(table1) %>% 
#   print(target = tmp)
# 
# 
