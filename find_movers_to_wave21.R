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
df <- read_dta("data/bes/internet_panel/BES2019_W21_Panel_v21.0.dta") 

#dplyr::select only the geographic variables
df <- df %>%
  dplyr::select(id,
  starts_with("pconW"), starts_with("gor"),
                starts_with("oslaua")) 

# get a vector of variable names 
names <- names(df) %>% as_tibble()



#get all the unique ids - will loop over this 
ids <- unique(df$id)



#write function to find whether smeone moved pcon between waves
find_wave_moved <- function(dataframe, respondent_id){
  
  dat <- dataframe %>%
    filter(id == respondent_id) %>% 
    #if someone has NA, that meanas they didn't take the wave - 
    #we assume they didn't move between the last wave they took and the NA wave,
    #so we use "fill" to impute down (is this the right direction?)
    fill(value, .direction = "up") %>%
    #find the MSOA in the previous wave 
    mutate(previous_value = dplyr::lag(value),
           #create indicator for the wave in which person newly moved to a place
           moved = ifelse(value != previous_value & !(is.na(previous_value)), 1, 0)) 
  
  #if the person doesn't appear n the data for some reason, just throw them out 
  if (nrow(dat) == 0){
    moved <- NA
    wave_moved <- NA 
    #if the person does appear, do this:
  } else {
    #get vector of whether the person moved in that wave
    vector <- dat %>% pull(moved)
    if (1 %in% vector){
      #if the person moved, assign 1 to indicator
      moved <- 1
      #get wave in which the person moved (but what about double-movers?)
      wave_moved <- dat$wave[which(dat$moved == 1)]
    } else {
      #if person didn't move, assign 0 to indicator and NA to wave variable 
      moved <- 0
      wave_moved <- NA 
    }
  }
  return(list( respondent_id, wave_moved, moved))
} 

#write function to unpack the result of parallelized find_wave_moved function
unpack_multiple_movers <- function(result_output){
  
  #takes as input the output of the find_wave_moved function that results from %dopar%
  df_out <- result_output %>%
    as_tibble() %>% 
    #unnest the ugly df-list-thing
    unnest(cols = c(V1, V2, V3)) %>% 
    group_by(V1) %>% 
    #this gives us something to use as names_from in the id
    mutate(id = 1:n()) %>% 
    ungroup() %>% 
    #now do the unpacking with magic from chris 
    pivot_wider(id_cols = V1, names_from = id, 
                values_from = V2)  %>% 
    #rename columns to be interpretable
    rename_with(.fn = function(x) paste0("wave_", x), 
                .cols = num_range("", 1:12) ) %>% 
    rename(id = V1) %>% 
    #make variable for whether person ever moved
    mutate(moved = ifelse(!is.na(wave_1), 1, 0))
  
  return(df_out)
  
}


#get the right columns, get them in the right order, pivot the data longer
pcon_df <- df %>% 
  select(id, starts_with("pcon")) %>% 
  pivot_longer(cols = pconW1:pconW21,
               names_to = "wave", 
               values_to = "value") %>% 
  mutate(wave= str_remove(wave,"pconW"))

# run the function over constituencies 
result_pcon <- foreach(i = 1:length(ids),
                       .combine = rbind, 
                       .packages = "tidyverse") %dopar% {
                         find_wave_moved(pcon_df, ids[i])
                       }

moved_pcon_df <- unpack_multiple_movers(result_pcon)



# do the same for regions 
gor_df <- df %>% 
  select(id, starts_with("gor")) %>% 
  pivot_longer(cols = gorW1:gorW21, 
               names_to = "wave", 
               values_to = "value") %>% 
  mutate(wave = str_remove(wave, "gorW"))

result_gor <- foreach(i = 1:length(ids), 
                      .combine = rbind, 
                      .packages = "tidyverse") %dopar% { 
                        find_wave_moved(gor_df, ids[i])
                      }


# now get the names of each constituency 



# now match the consituencies to income data