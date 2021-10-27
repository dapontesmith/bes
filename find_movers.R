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

#setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
#df <- read.csv("data/bes/internet_panel/wave11_got_localism_all_vars_with_msoas.csv") USE THIS ONE ON PERSONAL COMPUTER
df <- read_csv("/Users/nod086/Downloads/wave11_all_vars_with_msoas.csv") #USE THIS ONE ON IQSS LAB COMPUTER
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
    fill(value, .direction = "down") %>%
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
  return(list( wave_moved, moved))
} 

#write function to unpack the result of parallelized find_wave_moved function
#this deals with cases in which people moved multiple times (up to 5, but adaptable to more)
unpack_multiple_movers <- function(result_output){
  
  #get moved vector 
  moved_vector <- result[,2]
  
  #define empty holders
  wave_moved_1 <- NULL
  wave_moved_2 <- NULL
  wave_moved_3 <- NULL
  wave_moved_4 <- NULL
  wave_moved_5 <- NULL
  for (i in 1:nrow(result)){
    row <- result[i,][[1]]
    if (length(row) == 1){
      wave_moved_1[i] <- row[1]
      wave_moved_2[i] <- NA 
      wave_moved_3[i] <- NA 
      wave_moved_4[i] <- NA 
      wave_moved_5[i] <- NA 
      
    } else if(length(row) == 2) {
      wave_moved_1[i] <- row[1]
      wave_moved_2[i] <- row[2]
      wave_moved_3[i] <- NA 
      wave_moved_4[i] <- NA 
      wave_moved_5[i] <- NA 
    } else if(length(row) == 3){
      wave_moved_1[i] <- row[1]
      wave_moved_2[i] <- row[2]
      wave_moved_3[i] <- row[3]
      wave_moved_4[i] <- NA 
      wave_moved_5[i] <- NA 
    } else if (length(row) == 4){
      wave_moved_1[i] <- row[1]
      wave_moved_2[i] <- row[2]
      wave_moved_3[i] <- row[3]
      wave_moved_4[i] <- row[4]
      wave_moved_5[i] <- NA
    } else{
      wave_moved_1[i] <- row[1]
      wave_moved_2[i] <- row[2] 
      wave_moved_3[i] <- row[3]
      wave_moved_4[i] <- row[4]
      wave_moved_5[i] <- row[5]
    }
  }
  
  #put them together into a dataframe to return
  df_out <- cbind(ids, moved_vector, wave_moved_1, wave_moved_2, 
                  wave_moved_3, wave_moved_4, 
                  wave_moved_5) %>% as_tibble() 
  
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
#run the process in parallel
result_msoa <- foreach(i = 1:length(ids), .combine = rbind,
                  .packages = "tidyverse")  %dopar% {
                    find_wave_moved(msoa_df, ids[i])
                  }
moved_msoa_df <- unpack_multiple_movers(result_msoa)
moved_msoa_df <- moved_msoa_df %>% 
  #rename for eventual joining
  rename(id = ids, moved_msoa = moved_vector)
names(moved_msoa_df) <- str_replace(names(moved_msoa_df),
                                    'wave_moved','wave_moved_msoa')




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
moved_pcon_df <- moved_pcon_df %>% 
  #rename for eventual joining
  rename(id = ids, moved_pcon = moved_vector)
names(moved_pcon_df) <- str_replace(names(moved_pcon_df),
                                    'wave_moved','wave_moved_pcon')

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
  #rename for eventual joining
  rename(id = ids, moved_region = moved_vector)
names(moved_region_df) <- str_replace(names(moved_region_df),
                                    'wave_moved','wave_moved_region')



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
  #rename for eventual joining
  rename(id = ids, moved_oslaua = moved_vector)
names(moved_oslaua_df) <- str_replace(names(moved_oslaua_df),
                                    'wave_moved','wave_moved_oslaua')

#join all of them together
join <- left_join(moved_msoa_df, moved_pcon_df, by = "id") %>% 
  left_join(., moved_region_df, by = 'id') %>% 
  left_join(moved_oslaua_df, by = 'id')

rm(switch_oslaua)
rm(pivot)
rm(ever_switch)
rm(labels)

save.image()

