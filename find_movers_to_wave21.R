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

#setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
#df <- read_dta("data/bes/internet_panel/BES2019_W21_Panel_v21.0.dta") 
df <- read_dta("BES2019_W21_Panel_v21.0.dta")

#dplyr::select only the geographic variables
df <- df %>%
  dplyr::select(id,
  starts_with("pconW"), starts_with("gor"),
                starts_with("oslaua")) 
# get only people who appear in wave 21
in_wave_21 <- df %>% 
  filter(!is.na(pconW21))

# get a vector of variable names 
names <- names(in_wave_21) %>% as_tibble()



#get all the unique ids - will loop over this 
ids <- unique(in_wave_21$id)


# function to report the ids of movers, 
# as well as the wave in which they moved, 
# and the pcon code of their old and new constituencies 
find_movers <- function(dataframe, respondent_id){
  out <- dataframe[dataframe$id == respondent_id,] %>% 
    fill(area_code, .direction = "up") %>% 
    fill(area_name, .direction = "up") %>% 
    # find the area in the previous wave 
    mutate(previous_code = lag(area_code),
           previous_name = lag(area_name)) %>% 
    # create indicator for whether hte person moved in a particular wave
    mutate(moved = ifelse(
      area_code != previous_code & !(is.na(previous_code)), 1, 0)
    )
  
  # for each mover, get name/code of old place and new place
  new_area <- out$area_name[out$moved == 1]
  old_area <- out$previous_name[out$moved == 1]
  new_code <- out$area_code[out$moved == 1]
  old_code <- out$previous_code[out$moved == 1]
  
  # get the wave in which the respondent moved
  wave_moved <- out$wave[out$moved == 1]
  
  return(list(respondent_id, new_area, old_area,
              new_code, old_code, wave_moved))
}

# function for unpacking the result of the find_movers function
unpack_result <- function(result){
  # result = output of parallelized find_movers function
  out <- result_pcon %>% 
    as_tibble() %>% 
    unnest(cols = c(V1, V2, V3, V4, V5, V6 )) %>% 
    rename(id = V1, 
           new_area = V2, old_area = V3, 
           new_code = V4, old_code = V5, 
           wave_moved = V6) 
}

# prepare pcon data 
pcon_data <- in_wave_21 %>% 
  select(id, starts_with("pcon")) %>% 
  pivot_longer(cols = pconW1:pconW21, 
               names_to = "wave",
               values_to = "area_code") %>% 
  mutate(wave = str_remove(wave, "pconW"),
         area_name = labelled::to_factor(area_code),
         area_code = as.numeric(area_code))
  


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
  filter(new_area != "NOT in a 2010 Parliamentary Constituency" & 
           old_area != "NOT in a 2010 Parliamentary Constituency")
  

# do the same for the region data 
gor_data <- in_wave_21 %>% 
  select(id, starts_with("gor")) %>% 
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





# now match the consituencies to income data