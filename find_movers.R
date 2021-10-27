library(parlitools)
library(tidyverse)
library(sjlabelled)


setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
df <- read.csv("data/bes/internet_panel/wave11_got_localism_all_vars_with_msoas.csv")

#get tibble of df names 
names <- names(df) %>% as_tibble()
#select only the geographic variables
df <- df %>%
  dplyr::select(id, starts_with("p_msoa"),
                starts_with("pcon"), starts_with("gor"),
                starts_with("oslaua"))

labels <- df %>% 
  dplyr::select(-id) %>% 
  map(function(x) sjlabelled::as_character(x)) %>% 
  as_tibble()

labels <- labels %>%
  dplyr::select(-starts_with("pcont"),
                -starts_with("p_msoa11")) %>%
  mutate(id = df$id) %>% 
  dplyr::select(id, everything())

#get whether respondent switches oslaua wave-by-wave  
switch_oslaua <- labels %>%
  mutate(switch_oslaua_W1W2 = 
           ifelse(oslauaW1 != oslauaW2 & !is.na(oslauaW1) & !is.na(oslauaW2), 
                  1, 0),
         switch_oslaua_W2W3 = 
           ifelse(oslauaW2 != oslauaW3 & !is.na(oslauaW2) & !is.na(oslauaW3), 
                1, 0),
         switch_oslaua_W3W4 = 
           ifelse(oslauaW3 != oslauaW4 & !is.na(oslauaW3) & !is.na(oslauaW4), 
                  1, 0),
         switch_oslaua_W4W5 = 
           ifelse(oslauaW4 != oslauaW5 & !is.na(oslauaW4) & !is.na(oslauaW5), 
                  1, 0),
         switch_oslaua_W5W6 = 
           ifelse(oslauaW5 != oslauaW6 & !is.na(oslauaW5) & !is.na(oslauaW6), 
                  1, 0),
         switch_oslaua_W6W7 = 
           ifelse(oslauaW6 != oslauaW7 & !is.na(oslauaW6) & !is.na(oslauaW7), 
                  1, 0),
         switch_oslaua_W7W8 = 
           ifelse(oslauaW7 != oslauaW8 & !is.na(oslauaW7) & !is.na(oslauaW8), 
                  1, 0),
         switch_oslaua_W8W9 = 
           ifelse(oslauaW8 != oslauaW9 & !is.na(oslauaW8) & !is.na(oslauaW9), 
                  1, 0),
         switch_oslaua_W9W10 = 
           ifelse(oslauaW9 != oslauaW10 & !is.na(oslauaW9) & !is.na(oslauaW10), 
                  1, 0),
         switch_oslaua_W10W11 = 
           ifelse(oslauaW10 != oslauaW11 & !is.na(oslauaW10) & !is.na(oslauaW11), 
                  1, 0),
         switch_oslaua_W11W12 = 
           ifelse(oslauaW11 != oslauaW12 & !is.na(oslauaW11) & !is.na(oslauaW12), 
                  1, 0),
         switch_oslaua_W12W13 = 
           ifelse(oslauaW12 != oslauaW13 & !is.na(oslauaW12) & !is.na(oslauaW13), 
                  1, 0),
         switch_oslaua_W13W14 = 
           ifelse(oslauaW13 != oslauaW14 & !is.na(oslauaW13) & !is.na(oslauaW14), 
                  1, 0),
         switch_oslaua_W14W15 = 
           ifelse(oslauaW14 != oslauaW15 & !is.na(oslauaW14) & !is.na(oslauaW15), 
                  1, 0),
         switch_oslaua_W15W16 = 
           ifelse(oslauaW15 != oslauaW16 & !is.na(oslauaW15) & !is.na(oslauaW16), 
                  1, 0),
         switch_oslaua_W16W17 = 
           ifelse(oslauaW16 != oslauaW17 & !is.na(oslauaW16) & !is.na(oslauaW17), 
                  1, 0),
         switch_oslaua_W17W18 = 
           ifelse(oslauaW17 != oslauaW18 & !is.na(oslauaW17) & !is.na(oslauaW18), 
                  1, 0),
         switch_oslaua_W18W19 = 
           ifelse(oslauaW18 != oslauaW19 & !is.na(oslauaW18) & !is.na(oslauaW19), 
                  1, 0),
         switch_oslaua_W19W20 = 
           ifelse(oslauaW19 != oslauaW20 & !is.na(oslauaW19) & !is.na(oslauaW20), 
                  1, 0))

#get whether respondent switched oslaua prior to wave 11
pivot <- switch_oslaua %>%
  dplyr::select(id, starts_with("switch")) %>% 
  pivot_longer(cols = switch_oslaua_W1W2:switch_oslaua_W10W11)
ever_switch <- NULL
ids <- unique(switch_oslaua$id)
for (i in 1:length(ids)){
  print(i)
  
  if (1 %in% pivot$value[pivot$id == ids[i]]){
    ever_switch[i] <- 1
  } else {
    ever_switch[i] <- 0
  }
}
cbind(ids, ever_switch) %>%
  as_tibble() %>% 
  rename(id = ids, ever_switch_oslaua = ever_switch)

  

