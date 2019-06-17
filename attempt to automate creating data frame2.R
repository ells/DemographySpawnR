library(readxl)
dat = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/mess around.csv")
dat = read_xlsx("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/pretend demographics.xlsx")

names(dat)<- tolower(names(dat))
dat$sex = sample(c("M", "F", "O"), 30, prob = c(0.333, 0.333, 0.333), replace = T)
dat$education = sample(c(1:8), 30, T)

gender = dat[, grepl("gender|sex", names(dat))]; gender
## pick whatever 
ageVALUE = dat[, grepl("age", names(dat))]; ageVALUE

raceVALUE = dat[, grepl("race", names(dat))]; raceVALUE

edu = dat[, grepl("edu", names(dat))]; edu
hisp = dat[, grepl("hisp", names(dat))]; hisp 



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
  