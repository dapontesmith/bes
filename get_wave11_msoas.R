setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(haven)
library(estimatr)
library(sjlabelled)
library(lfe)
library(lmerTest)
library(parlitools) #has various useful data for linking to BES 
library(sf)
library(DescTools)
rm(list = ls())

df <- read_dta("data/bes/internet_panel/bes_wave1_to_wave20.dta")

#get only people who took wave 11 and gave non-missing response on localism question
df <- df %>%
  filter(wave11 == 1)  #belongGroup_2W11 != 9999)
df[df == 9999] <- NA 

df <- df %>%
  rename(belongLocal = belongGroup_2W11, 
         belongRegion = belongGroup_1W11) 

df <- df %>%
  mutate(p_msoa11W11 = get_labels(p_msoa11W11))

#get all variable anmes
vars <- names(df) %>% as_tibble() 

#get the msoa names (which are labels in original data)
msoas <- df %>% 
  dplyr::select(starts_with("p_msoa")) %>% 
  map(function(x) sjlabelled::as_character(x)) %>% 
  as_tibble()
names(msoas) <- str_replace(names(msoas), "msoa","msoa_name")

#add these to the data
df <- cbind(df, msoas) %>%
  as_tibble()
#write to csv
write.csv(df, "data/bes/internet_panel/wave11_all_vars_with_msoas.csv")


