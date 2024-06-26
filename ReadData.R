library(tidyverse)
library(mice)
library(xts)
library(ggplot2)


# Read in data, handle NAs
house <- read.csv("train.csv")

NAtoNone <- c(2,3,7,10,24,25,26,31,32,33,34,36,54,56,58,59,61,64,65,73,74,75,79)
for (i in NAtoNone) {
  housetmp <- house
  housetmp[i][is.na(housetmp[i])] <- "None"
  house <- housetmp
}
house <- house[-1380,]

house <- house %>% mutate_if(is.character, as.factor)
house <- house %>% mutate_at(c('MSSubClass','MoSold'), as_factor) 


YrMoSold <- do.call(paste, c(sep='',list(house$YrSold),list(rep("-", times = length(house$YrSold))),list(house$MoSold),list(rep("-01", times = length(house$YrSold)))))
YrMoSold <- as_date(YrMoSold)
house <- as.data.frame(append(house, list(YrMoSold = YrMoSold), after = 78))



# Take out GarageYrBlt
house <- house[-60]



# Imputation
numhouse <- select_if(house, is.numeric)
imputed_house <- mice(numhouse, 
                      m = 10,  
                      method = 'pmm', 
                      seed = 1)

set.seed(1)
imphouse <- complete(imputed_house)
housetmp <- house
s = 1
for (i in names(imphouse)[2:length(names(imphouse))]) {
  housetmp[i] <- imphouse[i]
}
house <- housetmp

md.pattern(house) # Should get cat picture if it works

