library(parlitools)
library(sjlabelled)
#set up parallel processing
library(MASS)
library(parallel)
library(foreach)
library(doParallel)
library(lfe)
library(modelsummary)
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
# clean up the pcon unemployment dataframe
pcon_unemployment <- pcon_unemployment %>% 
  dplyr::select(ONSConstID, 
                ConstituencyName, 
                DateOfDataset, 
                UnempConstRate,
                UnempCountryRate) %>% 
  # make scaled version of constituency unemployment by date
  group_by(DateOfDataset) %>% 
  mutate(UnempConstRate_scale = scale(UnempConstRate),
         # make difference between national and constituency unemployment, scaled and unscaled
         pcon_nat_rate_diff = UnempConstRate - UnempCountryRate, 
         pcon_nat_rate_diff_scale = scale(pcon_nat_rate_diff)) %>% 
  dplyr::select(-UnempCountryRate) %>% 
  # fix the may 2014 date
  mutate(DateOfDataset = ifelse(DateOfDataset == "5/3/2014", "5/1/2014", DateOfDataset)) 

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
redist_long <- read_demographics("redistSelf") %>% 
  # switch scale of redistribution variable so higher = more supportive
  mutate(redistSelf = 10-redistSelf)
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

# read in gender data 
gender <- read_csv("data/bes/internet_panel/bes_gender_data_wave1_to_wave21.csv")
gender <- gender %>% 
  mutate(male = ifelse(gender == 1, 1, 0)) %>% 
  dplyr::select(-gender)



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

starttimes <- starttimes %>% 
  mutate(date_full = paste(month, "/1/",year, sep = ""))


# read in gender data 
gender <- read_csv("data/bes/internet_panel/bes_gender_data_wave1_to_wave21.csv")
gender <- gender %>% 
  mutate(male = ifelse(gender == 1, 1, 0)) %>% 
  dplyr::select(-gender)


# read in the pcon data 
# pcon_income <- read_csv("data/uk_geography/pcon_data/pcon_income_data_2018.csv") 
# pcon_income <- pcon_income %>% 
#   dplyr::select(PCON19CD, PCON19NM,
#                 median_total_income) %>% 
#   # make the variable numeric
#   mutate(median_total_income = as.numeric(str_remove(median_total_income, ",")),# , 
#          # make scaled version of the variable 
#          median_total_income_scale = scale(median_total_income)[,1])
# 


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
  left_join(., gender, 
            by = "id") %>% 
  left_join(., edlevel_long, 
            by = c("id","wave")) %>% 
  left_join(., socgrade_long, 
            by = c("id","wave")) %>% 
  left_join(., age_long, 
            by = c("id","wave")) %>% 
  left_join(., leftRight_long, 
            by = c('id',"wave")) %>% 
  left_join(., pcon_income, 
            by = c("area_name" = "PCON19NM")) %>% 
  # join in the dates of each survey using starttimes df 
  left_join(., starttimes %>% 
              dplyr::select(id, wave, date_full), 
            by = c("id","wave")) %>% 
  # this is dubious, but assign january 2021 data to may 2021 wave, so as to reduce NAs
  mutate(date_full = ifelse(date_full == "5/1/2021", "1/1/2021", date_full)) %>% 
  # join the pcon unemployment data based on survey dates 
  left_join(., pcon_unemployment, 
            by = c("area_name" = "ConstituencyName", 
                   "date_full" = "DateOfDataset"))

# read in median annual pay data 
pay <- read_csv("data/uk_geography/pcon_data/median_incomes/annual_pay_2015_2021_clean.csv") %>% 
  dplyr::select(-`...1`)

# create year varialbe in pcon_data, then merge in median pay 
pcon_data$year <- str_split(pcon_data$date_full, "/",
                            simplify = TRUE)[,3] 
pcon_data <- pcon_data %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(pay, by = c(
    "year",
    "area_name" = "area",
    "PCON19CD" = "code"
  ))


moddat <- pcon_data %>% 
  # fill NAs upward
  # impute geographies in missing survey waves 
  # fill(redistSelf, .direction = "up") %>% 
  # fill(p_gross_household, .direction = "up") %>% 
  # fill(p_edlevel, .direction = "up") %>% 
  # fill(p_socgrade, .direction = "up") %>% 
  # fill(leftRight, .direction = "up") %>% 
  # fill(median_total_income, .direction = "up") %>%
  # fill(UnempConstRate, .direction = "up") %>% 
  # fill(median_total_income_scale, .direction = "up") %>% 
  # fill(UnempConstRate_scale, .direction = "up") %>% 
  # fill(pcon_nat_rate_diff, .direction = "up") %>% 
  # fill(pcon_nat_rate_diff_scale, .direction = "up") %>% 
  # # make cahnge variables
  mutate(median_total_income_change = median_total_income - lag(median_total_income),
         UnempConstRate_change = UnempConstRate - lag(UnempConstRate),
         UnempConstRate_scale_change = UnempConstRate_scale - lag(UnempConstRate_scale),
         pcon_nat_rate_diff_change = pcon_nat_rate_diff - lag(pcon_nat_rate_diff),
         pcon_nat_rate_diff_scale_change = pcon_nat_rate_diff_scale - lag(pcon_nat_rate_diff_scale),
         redistSelf_change = redistSelf - lag(redistSelf)) 
 

# run initial model of changes in redistribution preference vs. change in relative income of pcon
mod <- felm(data = moddat, 
     redistSelf ~ median_scale
     # fixed effects by individual and wave
        | id + wave | 0 | id)


summary(mod)


nrow(pcon_data %>% 
       filter(!is.na(redistSelf) & !is.na(pcon_nat_rate_diff_scale))) / 
  nrow(pcon_data)


modelsummary(mod, stars = TRUE,
             coef_map = c(
               "pcon_nat_rate_diff_scale_change" = "Unemployment rate difference between pcon and national",
               "p_gross_household" = "Household income",
               "p_edlevel" = "Education",
               "p_socgrade" = 'Social grade',
               "age" = "Age",
               "leftRight" = "Left-right self-placement"), 
               add_rows = bind_cols("Fixed effects","respondent + pcon",),
             title = "Relative local income and redistribution attitudes")

ggplot(moddat) + 
  geom_histogram(aes(x = pcon_nat_rate_diff), 
                 bins = 100)

moddat %>% na.omit %>% ggplot() + 
  geom_histogram(aes(x = redistSelf_change), 
                 bins = 100)
