library(parlitools)
library(tidyverse)
library(sjlabelled)
#set up parallel processing
library(MASS)
library(parallel)
library(foreach)
library(doParallel)
numCores = detectCores()
registerDoParallel(numCores - 1)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
df <- read_csv("data/bes/internet_panel/wave11_all_vars_with_msoas.csv") #USE THIS ONE ON PERSONAL COMPUTER
#df <- read_csv("/Users/nod086/Downloads/wave11_all_vars_with_msoas.csv") #USE THIS ONE ON IQSS LAB COMPUTER
#get tibble of df names 
names <- names(df) %>% as_tibble()
#dplyr::select only the geographic variables
df <- df %>%
  dplyr::select(id, starts_with("p_msoa"),
                starts_with("pcon"), starts_with("gor"),
                starts_with("oslaua"))


#get all the unique ids - will loop over this 
ids <- unique(df$id)

#write function to find whether smeone moved MSOA between waves
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
                .cols = num_range("", 1:9) ) %>% 
    rename(id = V1) %>% 
    #make variable for whether person ever moved
    mutate(moved = ifelse(!is.na(wave_1), 1, 0))
  
  return(df_out)
  
}



#prepare MSOA data for find_wave_moved function - 
#get the right columns, get them in the right order, pivot the data longer
msoa_df <- df %>% dplyr::select(id, p_msoa11W10,p_msoa11W11, p_msoa11W12, p_msoa11W13, 
                         p_msoa11W14, p_msoa11W15, p_msoa11W16, p_msoa11W17, 
                         p_msoa11W18, p_msoa11W19, p_msoa11W20 ) %>% 
  pivot_longer(cols = p_msoa11W10:p_msoa11W20, names_to = "wave", values_to = "value") %>% 
  mutate(wave = str_remove(wave, "p_msoa11W"))

#run function for MSOAs
#run the process in parallel]
result_msoa <- foreach(i = 1:length(ids), .combine = rbind,
                  .packages = "tidyverse")  %dopar% {
                    find_wave_moved(msoa_df, ids[i])
                  }
moved_msoa_df <- unpack_multiple_movers(result_msoa)




#prepare pcon data for find_wave_moved function
pcon_df <- df %>%
  dplyr::select(id, starts_with("pconW")) %>% 
  pivot_longer(cols = pconW1:pconW20, names_to = "wave", values_to = "value") %>% 
  mutate(wave = str_remove(wave, "pconW"))


#run the process in parallel
result_pcon <- foreach(i = 1:length(ids), .combine = rbind,
                  .packages = "tidyverse")  %dopar% {
                    find_wave_moved(pcon_df, ids[i])
                  }
moved_pcon_df <- unpack_multiple_movers(result_pcon)


#prepare region data for find_wave_moved function 
region_df <- df %>% 
  dplyr::select(id, starts_with('gor')) %>% 
  pivot_longer(cols = gorW7:gorW20, 
               names_to = 'wave', values_to = "value") %>% 
  mutate(wave = as.numeric(str_remove(wave, "gorW"))) %>%
  group_by(id) %>%
  arrange(wave, .by_group = TRUE)

result_region <- foreach(i = 1:length(ids), .combine = rbind,
                         .packages = "tidyverse") %dopar% {
                           find_wave_moved(region_df, ids[i])
                         }
moved_region_df <- unpack_multiple_movers(result_region) %>% 



#prepare oslaua data for this as well
oslaua_df <- df %>% 
  dplyr::select(id, starts_with('oslaua')) %>% 
  pivot_longer(cols = oslauaW1:oslauaW20,
               names_to = 'wave', values_to = "value") %>% 
  mutate(wave = str_remove(wave, "oslauaW"))

result_oslaua <- foreach(i = 1:length(ids), .combine = rbind,
                         .packages = "tidyverse") %dopar% {
                           find_wave_moved(region_df, ids[i])
                         }
moved_oslaua_df <- unpack_multiple_movers(result_oslaua) %>% 


#join all of them together
join <- left_join(moved_msoa_df, moved_pcon_df, by = "id") %>% 
  left_join(., moved_region_df, by = 'id') %>% 
  left_join(moved_oslaua_df, by = 'id')

#remove unnecessary objects, then save the workspace 
rm(switch_oslaua)
rm(pivot)
rm(ever_switch)
rm(labels)
save.image()


#load the data 
load("data/bes/internet_panel/find_movers.Rdata")

join <- as.data.frame(join)

#make variable for whether respondent moved prior to wave 11
join <- join %>% 
  mutate(moved_msoa_wave11 = case_when(
    wave_moved_msoa_1 == 11 ~ 1, 
    wave_moved_msoa_2 == 11 ~ 1,
    wave_moved_msoa_3 == 11 ~ 1,
    wave_moved_msoa_4 == 11 ~ 1,
    wave_moved_msoa_5 == 11 ~ 1,
    TRUE ~ 0
  ),moved_pcon_wave11 = case_when(
    wave_moved_pcon_1 == 11 ~ 1, 
    wave_moved_pcon_2 == 11 ~ 1,
    wave_moved_pcon_3 == 11 ~ 1,
    wave_moved_pcon_4 == 11 ~ 1,
    wave_moved_pcon_5 == 11 ~ 1,
    TRUE ~ 0 ),
  moved_oslaua_wave11 = case_when(
    wave_moved_oslaua_1 == 11 ~ 1, 
    wave_moved_oslaua_2 == 11 ~ 1,
    wave_moved_oslaua_3 == 11 ~ 1,
    wave_moved_oslaua_4 == 11 ~ 1,
    wave_moved_oslaua_5 == 11 ~ 1,
    TRUE ~ 0),
  moved_region_wave11 = case_when(
    wave_moved_region_1 == 11 ~ 1, 
    wave_moved_region_2 == 11 ~ 1,
    wave_moved_region_3 == 11 ~ 1,
    wave_moved_region_4 == 11 ~ 1,
    wave_moved_region_5 == 11 ~ 1,
    TRUE ~ 0))


sum(join$moved_msoa_wave11 == 1)
sum(join$moved_oslaua_wave11 == 1)
sum(join$moved_pcon_wave11 == 1)
sum(join$moved_region_wave11 == 1)
