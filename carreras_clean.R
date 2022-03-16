#file for selecting relevant variables from Carreras et al dataset
#doing this is easier than downloading it all from various websites
#resultant dataset can be merged with BES datasets
#data selected is mostly about long-term economic change 
#all data selected is at local authority level, defined by 2016 boundaries
library(tidyverse)
library(haven)
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")


car <- read_dta("data/uk_geography/CPS_Carrerasetal_survey_dataset.dta")
car <- car %>% 
  dplyr::select(lad16cd:perc_manufacturing1997)
#I have gone through these to make sure I'm selecting only variables 
#that don't have lots of missingness

car <- car %>% 
  select(lad16cd, median_age, gdhi_1997, gdhi_pc1997,
         gdhi_2015, gdhi_pc2015, pop1997, real_gdhi_change1997_2015, )