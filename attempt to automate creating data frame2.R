library(readxl)
library(data.table)
library(tidyverse)


# dat = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/mess around.csv")
dat = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/pretend demographics.csv")
# dat = as.data.frame(read_xlsx("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/pretend demographics.xlsx"))
names(dat)

names(dat)<- tolower(names(dat))

gender = dat[, grepl("gender|sex", names(dat))]; gender
 
age = dat[, grepl("age", names(dat))]; age

race = dat[, grepl("race", names(dat))]; race

edu = dat[, grepl("edu", names(dat))]; edu
# 
# hisp = dat[, grepl("hisp", names(dat))]; hisp 
# Easy to add more depending on what variable we have 

n = 1000
set.seed(1234)
patients <- tibble( 
  
  `Gender`                = sample(c(as.character(as.data.frame(table(gender))$gender)), n, TRUE, 
                             prob = c(as.data.frame(table(gender))$Freq)
                                  ),
  `Race/Ethnicity`        = sample(c(as.character(as.data.frame(table(race))$race)), n, TRUE,
                              prob = c(as.data.frame(table(race))$Freq)
                                  ),
  `Age`                   = round(rnorm(n, mean(age), sd = sqrt(var(age)))
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



## Jk pretend we have no clue what data looks like
## loop through all the variables and give counts/percentages or mean/sd for each one
# Idk how to go about doing that but here we go



set.seed(3221)  # this makes the example exactly reproducible
my.data <- data.frame(y=rnorm(5), 
                      x1=c(1:5), 
                      x2=c(TRUE, TRUE, FALSE, FALSE, FALSE),
                      X3=letters[1:5])

a = data.frame(lapply(my.data, class)); a






choices = data.frame(nLevels = double(),
                     var = character())

 for (i in 1:ncol(dat)){
  choices[i,1] = length(unique(dat[,i]))
}

choices$var = colnames(dat)

# if nLevels of the variable < #, then do freq/percentages
  # need to check if factor/character, numeric 
# if nLevels of variable >= #, then sample from ~ N(mean, SD)
# somehow need to populate a data frame and simulate accordingly 









nodedata = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/Demo_outbreak_NodeList.csv")

n = 100
dataProcessing = function (data) {
  
# getting variable names and number of unique levels in each of them
levelsEachVariable = data.frame(nLevels = double(),
                     var = character())

for (i in 1:ncol(data)){
  levelsEachVariable[i,1] = length(unique(data[,i]))
}

levelsEachVariable$var = colnames(data)



simData <- data.frame(matrix(ncol = ncol(data), nrow = n))
colnames(simData) <- paste0("var", c(1:ncol(data)))


## getting distribution of each variable and randomly sampling from that to get new dataset

for (i in 1:ncol(data))
  {
  set.seed(1234)
  if (length(unique(data[,i])) < 5){
    simData[,i] = sample(c(as.character(as.data.frame(table(data[,i]))$Var1)), n, TRUE, 
                         prob = c(as.data.frame(table(data[,i]))$Freq)
                        ) 
                              } 
  else  {
    simData[,i] = round(rnorm(n, mean = mean(data[,i]), sd = sqrt(var(data[,i])) )
                        )
  }
  names(simData) = names(data)
  
  } #close for loop
  View(simData)
  return(data.frame(simData))
}

dataProcessing(nodedata)

# download devtools

# check of the table is < "10" if 
# yagni 