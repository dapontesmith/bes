setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(haven)
library(tidyverse)
library(parlitools)

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
         ends_with("AfterLocal"),
         ends_with("FairShare"), starts_with("map"), amenities,
         changeInequality,
         starts_with('area'), ends_with("a_1"),
         starts_with("subjClass"),
         ends_with("Econ"),
         starts_with("status"), starts_with("global"),
         country, age, gender, gor, pcon,oslauaEURef
         ) %>% 
  select(-starts_with("partyContact")) %>% 
  mutate(PCON19NM = labelled::to_factor(pcon)) %>% 
  left_join(., lookup, by = "PCON19NM") 

# get 

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
# switch scale of social grade variable, so higher = higher class
full$p_socgrade <- max(full$p_socgrade, na.rm = TRUE) - full$p_socgrade
#switch scale of redistriubtion variable, so higher = more support 
full$redistSelf <- 10 - full$redistSelf



# make variable for perceived idfference between london and local fair shares
full <- full %>% 
  mutate(london_local_fair_share = londonFairShare - localFairShare, 
         # make variable for perceived london-local econ difference 
         london_local_econ = londonEcon - localEcon,
         # do the same for region
         london_region_fair_share = londonFairShare - regionFairShare, 
         london_region_econ = londonEcon - regionEcon) %>% 
# make binary variable for perceiving area as richer or poorer than median 
  mutate(areaRichPoor_binary_rich = case_when(
    areaRichPoor <= median(areaRichPoor, na.rm = TRUE) ~ 0,
    areaRichPoor > median(areaRichPoor, na.rm = TRUE) ~ 1,
    TRUE ~ as.numeric(areaRichPoor)
  ), # make varaible for differece between perceived local and national unemployment 
  local_national_unem_diff = nationalUnemployment_a_1 - localUnemployment_a_1)

# handle some issues with geographic data
full <- full %>% 
 # mutate(lad_name = labelled::to_factor(oslaua),
mutate(PCON19NM = labelled::to_factor(pcon)) 
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
  ), # make categorical 2019 vote variable
  vote_2019 = case_when(
    p_past_vote_2019 == 0 ~ "did not vote", 
    p_past_vote_2019 == 1 ~ "con",
    p_past_vote_2019 == 2  ~ "lab",
    p_past_vote_2019 == 3  ~ "ld",
    p_past_vote_2019 == 4  ~ "snp",
    p_past_vote_2019 == 5  ~ "pc",
    p_past_vote_2019 == 6  ~ "ukip",
    p_past_vote_2019 == 7  ~ "green",
    p_past_vote_2019 == 8  ~ "bnp",
    p_past_vote_2019 == 9  ~ "other",
    p_past_vote_2019 == 11  ~ "changeuk",
    p_past_vote_2019 == 12 ~ "brexit",
    p_past_vote_2019 == 13  ~ "independent",
    TRUE ~ as.character(p_past_vote_2019)
  ) )


# merge parlitools 2019 data with the dataframe

bes_to_merge <- bes_2019 %>% 
  select(ons_const_id, constituency_name, 
         constituency_type, winner_19, turnout_19, 
         con_19:other_19) %>% 
  rename(PCON19CD = ons_const_id, 
         PCON19NM = constituency_name)

# set overall conservative share of the vote
con_share_overall <- 43.6

# create variable for difference between constituency con-share and overall con share
bes_to_merge <- bes_to_merge %>% 
  mutate(con_share_diff = con_19 - con_share_overall) 
# larger -> conservatives did better in constitunecy than in the country 

# join this to df by constituency identiifers 
full <- full %>% 
  left_join(bes_to_merge, 
            by = c("PCON19CD", "PCON19NM"))

# read in unemployment data 
unem <- read_csv("uk_geography/pcon_data/pcon_unemployment_to_2010.csv")

unem <- unem %>% 
  # get only 2021 unemployment
  filter(DateOfDataset == "1/1/2021") %>% 
  select(ONSConstID, UnempConstRate) %>% 
  # create scaled variable of unemeployment
  mutate(unemp_const_scale = scale(UnempConstRate))

full <- full %>% 
  left_join(unem, 
            by = c("PCON19CD" = "ONSConstID")) 

# make variable for the extent to which the respondent views the party they voted for as looking after local interests 
lookafterlocal <- full %>% 
  select(id, vote_2019, ends_with("AfterLocal")) %>% 
  filter(vote_2019 %in% c("lab","con","snp")) %>% 
  mutate(party_voted_lookAfterLocal = case_when(
    vote_2019 == "con" ~ conLookAfterLocal, 
    vote_2019 == "lab" ~ labLookAfterLocal,
    vote_2019 == "snp" ~ snpLookAfterLocal
  )) %>% 
  select(id, party_voted_lookAfterLocal)

full <- left_join(full, lookafterlocal, by = "id")



write.csv(full, "bes/internet_panel/clean_data/bes_wave21_clean.csv")


