library(parlitools)
library(sjlabelled)
#set up parallel processing
library(MASS)
library(parallel)
library(foreach)
library(doParallel)
library(tidyverse)
library(haven)


numCores = detectCores()
registerDoParallel(numCores - 1)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
df <- read_csv("data/bes/internet_panel/bes_geo_data_wave1_to_wave21.csv")
# <- read_dta("data/bes/internet_panel/BES2019_W21_Panel_v21.0.dta") 
#df <- read_dta("BES2019_W21_Panel_v21.0.dta")


# read in labelled geographic data
df_labels <- read_csv("data/bes/internet_panel/bes_geo_data_wave1_to_wave21_labelled.csv")

# read in pcon_region lookup
pcon_region_lookup <- read_csv("data/uk_geography/pcon_data/pcon_region_lookup_2019.csv")
# read in pcon_unemploment
pcon_unemployment <- read_csv("data/uk_geography/pcon_data/pcon_unemployment_to_2010.csv")

# write function for reading in demographic variable data and shaping to long 
read_demographics <- function( variable){
  # variable = name of varialbe in filepath 
  # needs to match root variable name in BES (i.e. p_gross_household)
  
  # get path
  path <- paste("data/bes/internet_panel/bes_", variable, 
                "_data_wave1_to_wave21.csv", sep = "")
  
  # this is the string to remove from the "wave" varaible
  variable_remove <- paste(variable, "W", sep = "")
  variable_first <- paste(variable, "W1", sep = "")
  variable_last <- paste(variable, "W21", sep = "")
  
  data <- read_csv(path)
  
  long <- data %>% 
    pivot_longer(cols = variable_first:variable_last,
                 names_to = "wave",
                 values_to = variable) %>% 
    mutate(wave = as.numeric(str_remove(wave, variable_remove))) %>%
    # fill nas 
    group_by(id) %>% 
    fill(variable, .direction = "updown")
  long[long == 9999] <- NA
  
  return(long)
  
}
redist_long <- read_demographics("redistSelf")
income_long <- read_demographics("p_gross_household") %>% 
  mutate(p_gross_household = ifelse(
    p_gross_household > 15, NA, p_gross_household
  ))
socgrade_long <- read_demographics("p_socgrade") %>% 
  mutate(p_socgrade = ifelse(
    p_socgrade == 8, NA, p_socgrade
  ))
edlevel_long <- read_demographics("p_edlevel")
leftRight_long <- read_demographics("leftRight")
age_long <- read_demographics("age")

# read in start time data
starttimes <- read_demographics("starttime") %>% 
  mutate(date = str_split(starttime, pattern = " ", simplify = TRUE)) 
starttimes$date <- starttimes$date[,1]
starttimes$year <- substr(starttimes$date, 6, 9)
starttimes$month_char <- substr(starttimes$date, 3, 5)
starttimes <- starttimes %>% 
  mutate(month = case_when(
    month_char == "jan" ~ 01, 
    month_char == "feb" ~ 02, 
    month_char == "mar" ~ 03, 
    month_char == "apr" ~ 04, 
    month_char == "may" ~ 05, 
    month_char == "jun" ~ 06,
    month_char == "jul" ~ 07, 
    month_char == "aug" ~ 08, 
    month_char == "sep" ~ 09, 
    month_char == "oct" ~ 10, 
    month_char == "nov" ~ 11, 
    month_char == "dec" ~ 12
  ))

# read in gender data 
gender <- read_csv("data/bes/internet_panel/bes_gender_data_wave1_to_wave21.csv")
gender <- gender %>% 
  mutate(male = ifelse(gender == 1, 1, 0)) %>% 
  dplyr::select(-gender)


# read in the pcon data 
pcon_income <- read_csv("data/uk_geography/pcon_data/pcon_income_data_2018.csv") 
pcon_income <- pcon_income %>% 
  dplyr::select(PCON19CD, PCON19NM) %>% 
  # make the variable numeric
  mutate(median_total_income = as.numeric(str_remove(median_total_income, ",")),# , 
         # make scaled version of the variable 
         median_total_income_scale = scale(median_total_income))

# NEED TO GET DATES OF SURVEY WAVES TO ACCURATELY MATCH TO UNEMPLOYMENT ESTMATES 




# join long versions of pcon data - get both codes and names 
pcon_data <- left_join(df %>% 
            dplyr::select(id, starts_with("pcon")) %>% 
            pivot_longer(cols = pconW1:pconW21, 
                         names_to = "wave",
                         values_to = "area_code") %>% 
            mutate(wave = as.numeric(str_remove(wave, "pconW")))
          
          , df_labels %>% 
            dplyr::select(id, starts_with("pcon")) %>% 
            pivot_longer(cols = pconW1:pconW21,
                         names_to = "wave",
                         values_to = "area_name") %>% 
            mutate(wave = as.numeric(str_remove(wave, "pconW"))), 
          by = c("id", "wave")) %>% 
  # join redistribution data
  left_join(., redist_long, 
            by = c("id", "wave")) %>% 
  # join income data 
  left_join(., income_long, 
            by = c("id",'wave')) %>% 
  left_join(., gender, by = "id") %>% 
  left_join(., edlevel_long, 
            by = c("id","wave")) %>% 
  left_join(., socgrade_long, 
            by = c("id","wave")) %>% 
  left_join(., age_long, 
            by = c("id","wave")) %>% 
  left_join(., leftRight_long, 
            by = c('id',"wave"))
    

# do the same for the region data - COME BACK TO THIS LATER 
gor_data <- left_join(df %>% 
                    dplyr::select(id, starts_with("gor")) %>% 
                    pivot_longer(cols = gorW1:gorW21, 
                                 names_to = "wave",
                                 values_to = "area_code") %>% 
                    mutate(wave = as.numeric(str_remove(wave, "gorW")))
                  
                  , df_labels %>% 
                    dplyr::select(id, starts_with("gor")) %>% 
                    pivot_longer(cols = gorW1:gorW21,
                                 names_to = "wave",
                                 values_to = "area_name") %>% 
                    mutate(wave = as.numeric(str_remove(wave, "gorW"))), 
                  by = c("id", "wave"))


#get all the unique ids - will loop over this 
ids <- unique(pcon_data$id)

#sample <- pcon_data %>% sample_n(., size = 3000)
# function to report the ids of movers, 
# as well as the wave in which they moved, 
# and the pcon code of their old and new constituencies 
out <- pcon_data %>% 
  #slice(1:5000) %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  # impute geographies in missing survey waves 
  fill(area_code, .direction = "up") %>% 
  fill(area_name, .direction = "up") %>% 
  #fill(redistSelf, .direction = "up") %>% 
  #fill(p_gross_household, .direction = "up") %>% 
  mutate(previous_code = lag(area_code),
         previous_name = lag(area_name),
         # find the redist value in previous wave 
         previous_redist = lag(redistSelf),
         previous_income = lag(p_gross_household),
         previous_edlevel = lag(p_edlevel), 
         previous_socgrade = lag(p_socgrade),
         previous_leftRight = lag(leftRight),
         previous_age = lag(age)) %>% 
  # put variables in nice order for viewing comparison 
  dplyr::select(id, wave, area_code, previous_code, 
         area_name, previous_name, 
         redistSelf, previous_redist, 
         p_gross_household, previous_income,
         p_socgrade, previous_socgrade, 
         p_edlevel, previous_edlevel, 
         leftRight, previous_leftRight, 
         age, previous_age, 
         male) %>% 
  # get wave moved 
  mutate(wave_moved = ifelse(area_code != previous_code & 
                               !is.na(previous_code), 1, 0)) %>% 
  # get change in redistribution and income variables 
  mutate(redist_change = redistSelf - previous_redist, 
         income_change = p_gross_household - previous_income,
         socgrade_change = p_socgrade - previous_socgrade, 
         edlevel_change = p_edlevel - previous_edlevel,
         leftRight_change = leftRight - previous_leftRight,
         age_change = age - previous_age) %>% 
  # get only the moving wave 
  filter(wave_moved == 1 & !is.na(redist_change)) %>% 
  # join in the pcon data 
  left_join(., pcon_income, 
          by = c("area_name" = "PCON19NM")) %>% 
  left_join(., pcon_income, 
            by = c("previous_name" = "PCON19NM"),
            suffix = c("_new","_old")) %>% 
  # make variables for relative and abolsute change between new and old places
  mutate(new_old_diff_absolute = median_total_income_new - median_total_income_old, 
         new_old_diff_relative = median_total_income_scale_new - median_total_income_scale_old) %>% 
  left_join(., pcon_region_lookup, 
            by = c("area_name" = "PCON19NM"))
         # switch scale of redist variables so higher = more supportive 
# join in hte region lookup, to get region names in there as well 



# Do movers become more supportive of redistribution when they move to poorer places? 
local_redist <- lm_robust(data = out, 
                          redist_change ~ new_old_diff_relative + edlevel_change + 
                            income_change + socgrade_change + male + leftRight_change + 
                            age_change, 
                          fixed_effects = region)

modelsummary(local_redist, stars = TRUE)






region_income <- read_csv("data/region_data/region_gva_indices_historical.csv") %>% 
#region_income <- read_csv("region_gva_indices_historical.csv") %>% 
  slice(-(1:3))

names(region_income) <- c("period",
                          "North East","Yorkshire and The Humber",
                          "East Midlands","East of England", "London",
                          "South East","South West", "West Midlands",
                          "North West","Wales","Scotland", "Northern Ireland")
region_income <- region_income %>% 
  pivot_longer(cols = `North East`:`Northern Ireland`,
               names_to = "region",
               values_to = "gva_index") %>% 
  filter(str_detect(period, "Q1") == TRUE) %>% 
  mutate(period = as.numeric(str_remove(period, "Q1"))) %>% 
  group_by(period) %>% 
  mutate(gva_index_scale = scale(as.numeric(gva_index, na.rm = TRUE)))

#  get the 2021 data to match to regions 



