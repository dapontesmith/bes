setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(haven)
library(tidyverse)

full <- read_dta("bes/internet_panel/BES2019_W21_v21.0.dta")
# read in pcon-to-region lookup
lookup <- read_csv("uk_geography/pcon_data/pcon_region_lookup_2019.csv")


full[full == 9999] <- NA
full[full == 9998] <- NA


full <- full %>% 
  select(starts_with("p_"),
         id, wt, turnoutUKGeneral:partyIdSqueeze,
         starts_with("econ"),
         leftRight, redistSelf, euID, immigSelf, ends_with("ness"),
         starts_with("conLook"), starts_with("labLook"),
         ends_with("FairShare"), starts_with("map"), amenities,
         starts_with('area'), ends_with("a_1"),
         starts_with("subjClass"),
         ends_with("Econ"),
         starts_with("status"), starts_with("global"),
         country, age, gender, gor, pcon,oslauaEURef
         ) %>% 
  select(-starts_with("partyContact")) %>% 
  mutate(PCON19NM = labelled::to_factor(pcon)) %>% 
  left_join(., lookup, by = "PCON19NM") 

full$age <- ifelse(full$age < 0 | full$age > 100, NA, full$age)
full$leftRight <- ifelse(full$leftRight > 10, NA, full$leftRight)
full$p_gross_household <- ifelse(full$p_gross_household > 15, NA, full$p_gross_household)
full$p_gross_personal <- ifelse(full$p_gross_personal > 14, NA, full$p_gross_personal)
full$p_education <- ifelse(full$p_education > 18, NA, full$p_education)
full$econPersonalProsp <- ifelse(full$econPersonalProsp > 5, NA, full$econPersonalProsp)
full$redistSelf <- ifelse(full$redistSelf > 10, NA, full$redistSelf)
full$p_ethnicity <- ifelse(full$p_ethnicity > 14, NA, full$p_ethnicity)
#full <- full %>% filter(!is.na(p_ethnicity))
full$is_white <- ifelse(full$p_ethnicity == 1, 1, 0 )
full$has_degree <- ifelse(full$p_edlevel > 3, 1, 0)
full$male <- ifelse(full$gender == 2, 0, full$gender)

# make variable for perceived idfference between london and local fair shares
full <- full %>% 
  mutate(london_local_fair_share = londonFairShare - localFairShare, 
         # make variable for perceived london-local econ difference 
         london_local_econ = londonEcon - localEcon,
         # do the same for region
         london_region_fair_share = londonFairShare - regionFairShare, 
         london_region_econ = londonEcon - regionEcon)

# handle some issues with geographic data
full <- full %>% 
  mutate(lad_name = labelled::to_factor(oslaua),
pcon_name = labelled::to_factor(pcon)) %>%
  rename(PCON19NM = pcon_name) 
  #left_join(., lookup, by = "PCON19NM") %>% 
  #filter(region != "Scotland")


# deal with people missing party Id 
for(i in 1:nrow(full)){
  if(is.na(full$partyId[i])){
    full$partyId[i] <- full$partyIdSqueeze[i]
  } 
}

# recode party id into categorical
full <- full %>% 
  mutate(party_id_cat = case_when(
    partyId == 1  ~ "Con",
    partyId == 2 ~ "Lab",
    partyId == 3 ~ "LD",
    partyId == 4 ~ "SNP",
    partyId == 5 ~ "PC",
    partyId == 6 ~ "UKIP",
    partyId == 7 ~ "Green",
    partyId == 8 ~ "BNP",
    partyId == 9 ~ "Other",
    partyId == 10 ~ "None",
    partyId == 11 ~ "Change",
    partyId == 12 ~ "Brexit"
  ))

# reverse the scale of the redistribution varaible, 
# so higher values = higher support for redistribution
full$redistSelf <- max(full$redistSelf, na.rm = TRUE) - full$redistSelf


write.csv(full, "bes/internet_panel/clean_data/bes_wave21_clean.csv")


