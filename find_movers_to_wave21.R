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

redist <- read_csv("data/bes/internet_panel/bes_redist_wave1_to_21.csv")
redist[redist == 9999] <- NA

# make long version of redist for merging 
redist_long <- redist %>% 
  pivot_longer(cols = redistSelfW1:redistSelfW21, 
               names_to  = "wave",
               values_to = "redistSelf") %>% 
  mutate(wave = as.numeric(str_remove(wave, "redistSelfW")),
         redistSelf = ifelse(redistSelf > 10, NA, redistSelf))


# read in income time-series 
income <- read_csv("data/bes/internet_panel/bes_income_data_wave1_to_wave21.csv")

# make long version of income for merging 
income_long <- income %>% 
  pivot_longer(cols = p_gross_householdW1:p_gross_householdW21,
               names_to = 'wave',
               values_to = 'p_gross_household') %>% 
  mutate(wave = as.numeric(str_remove(wave, "p_gross_householdW")),
         p_gross_household = ifelse(p_gross_household > 15, NA, p_gross_household))


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
            by = c("id",'wave'))
    

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
find_movers <- function(dataframe, respondent_id){
  out <- dataframe[dataframe$id == respondent_id,] %>% 
    fill(area_code, .direction = "up") %>% 
    fill(area_name, .direction = "up") %>% 
    fill(redistSelf, .direction = "up") %>% 
    fill(p_gross_household, .direction = "up") %>% 
    # find the area in the previous wave 
    mutate(previous_code = lag(area_code),
           previous_name = lag(area_name),
           # find the redist value in previous wave 
           previous_redist = lag(redistSelf),
           previous_income = lag(p_gross_household)) %>% 
    # create indicator for whether hte person moved in a particular wave
    mutate(moved = ifelse(
      area_code != previous_code & !(is.na(previous_code)), 1, 0)
    )
  out$redistSelf <- ifelse(out$redistSelf == 9999, NA, out$redistSelf)
  out$previous_redist <- ifelse(out$previous_redist == 9999, NA, out$previous_redist)
  
  # for each mover, get name/code of old place and new place
  new_code <- out$area_code[out$moved == 1]
  old_code <- out$previous_code[out$moved == 1]
  new_name <- out$area_name[out$moved == 1]
  old_name <- out$previous_name[out$moved == 1]
  
  #also get the redistribution values in the old and new waves
  old_redist <- out$previous_redist[out$moved == 1]
  new_redist <- out$redistSelf[out$moved == 1]
  
  #also get income before and after
  old_income <- out$previous_income[out$moved == 1]
  new_income <- out$p_gross_household[out$moved == 1]
  
  # get the wave in which the respondent moved
  wave_moved <- out$wave[out$moved == 1]
  
  return(list(respondent_id, wave_moved, new_code, old_code, 
              new_name, old_name, 
              new_redist, old_redist,
              new_income, old_income))
}

# function for unpacking the result of the find_movers function
unpack_result <- function(result){
  # result = output of parallelized find_movers function
  out <- result %>% 
    as_tibble() %>% 
    unnest(cols = c(V1, V2, V3, V4, V5, V6, 
                    V7, V8, V9, V10)) %>% 
    rename(id = V1, wave_moved = V2, 
           new_code = V3, old_code = V4, 
           new_name = V5, old_name = V6, 
           new_redist = V7, old_redist = V8,
           new_income = V9, old_income = V10) 
  return(out)
}

# prepare pcon data 
# pcon_data <- in_wave_21 %>% 
#   select(id, starts_with("pcon")) %>% 
#   pivot_longer(cols = pconW1:pconW21, 
#                names_to = "wave",
#                values_to = "area_code") %>% 
#   mutate(wave = str_remove(wave, "pconW"),
#          area_code = as.numeric(area_code))
#   


# run function on the pcon data
result_pcon <- foreach(i = 1:length(ids),
                       .combine = rbind, 
                       .packages = "tidyverse") %dopar% {
                         find_movers(pcon_data, 
                                         ids[i])
                       }
# unpack the pcon data
pcon_out <- unpack_result(result_pcon) %>% 
  # filter out people who weren't in a constituency, or aren't in one now
  filter(new_name != "NOT in a 2010 Parliamentary Constituency" & 
          old_name != "NOT in a 2010 Parliamentary Constituency") 
  
# now match the consituencies to income data

# read in the pcon data 
pcon_income <- read_csv("data/uk_geography/pcon_data/pcon_income_data_2018.csv") 
pcon_income <- pcon_income %>% 
  dplyr::select(PCON19CD, PCON19NM,
         median_total_income) %>% 
  # make the variable numeric
  mutate(median_total_income = as.numeric(str_remove(median_total_income, ",")),# , 
         # make scaled version of the variable 
         median_total_income_scale = scale(median_total_income))

# join with pcon out - do this twice, to account for new and old 
pcon_join <- left_join(pcon_out, pcon_income, 
                  by = c("new_name" = "PCON19NM")) %>% 
  left_join(., pcon_income, 
            by = c("old_name" = "PCON19NM"),
            suffix = c("_new","_old")) %>% 
  # make variables for relative and abolsute change between new and old places
  mutate(new_old_diff_absolute = median_total_income_new - median_total_income_old, 
         new_old_diff_relative = median_total_income_scale_new - median_total_income_scale_old,
         # switch scale of redist variables so higher = more supportive 
         new_redist = 10 - new_redist, 
         old_Redist = 10 - old_redist,
         # make redistribution change variable 
         redist_change = new_redist - old_redist,
         # make income change variable
         income_change = new_income - old_income)

# get only the most recent move 
pcon_join_recent <- pcon_join %>% 
  group_by(id) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == max(n))




# read in bes data
bes <- read_csv("data/bes/internet_panel/clean_data/bes_wave21_clean.csv")

# join bes to pcon_join_recent, by id 
test <- left_join(bes, 
                  pcon_join , 
                  by = "id")
# Do movers become more supportive of redistribution when they move to poorer places? 
local_redist <- lm_robust(data = test, 
                          redist_change ~ new_old_diff_relative + p_edlevel + age + male + p_socgrade + 
                            income_change + leftRight)

modelsummary(local_redist, 
             stars = TRUE,
            # output = "markdown",
             coef_map = c("new_old_diff_relative" = "Relative difference between new and old constituency income", 
                          "p_edlevel" = "Education",
                          "age" = "Age",
                          "male" = "Male",
                          "p_socgrade" = "Social grade",
                          "income_change" = "Income change",
                          "leftRight" = "Left-right self-placement"),
             #coef_omit = "factor",
             title = "Change in context and change in preferences over redistribution among British movers",
             notes = "Linear models with region fixed effects.
            Dependent variable is change in preferences over redistribution over period of moving.")



# do the same for the region data 
gor_data <- in_wave_21 %>% 
  select(id, gorW1, gorW2, gorW3, 
                gorW4, gorW5, gorW6, gorW7, 
                gorW8, gorW9, gorW10, gorW11, 
                gorW12, gorW13, gorW14, gorW15, 
                gorW16, gorW17, gorW18, gorW19, 
                gorW20, gorW21) %>% 
  pivot_longer(cols = gorW1:gorW21, 
               names_to = "wave",
               values_to = "area_code") %>% 
  mutate(wave = str_remove(wave, "gorW"),
         area_name = labelled::to_factor(area_code),
         area_code = as.numeric(area_code))
# run function on the pcon data
result_gor <- foreach(i = 1:length(ids),
                       .combine = rbind, 
                       .packages = "tidyverse") %dopar% {
                         find_movers(gor_data, 
                                     ids[i])
                       }
# unpack the pcon data
gor_out <- unpack_result(result_gor) 

# put together the region and pcon data
out <- left_join(gor_out, pcon_out, 
          by = "id",
          suffix = c("_gor",'_pcon'))





# what to do about people who moved multiple times ? 


# read in region income data
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



