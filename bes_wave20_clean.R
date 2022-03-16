setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")

library(tidyverse)
library(Amelia)
library(haven)

bes <- read_dta("bes/internet_panel/BES2019_W20_v0.1-3.dta")
lookup <- read_csv("uk_geography/pcon_data/pcon_region_lookup_2019.csv")

bes <- select_if(bes, is.numeric)

bes[bes == 9999] <- NA 


#a.out <- amelia(bes, m = 5)

dat <- bes


dat[dat == 9999] <- NA

dat <- dat %>% 
  #filter(!is.na(generalElectionVote)) %>% 
  filter(generalElectionVote %in% c(1,2,3,6,7,8,11,12)) %>% 
  mutate(switch_to_con = case_when(
    generalElectionVote == 1 & p_past_vote_2017 == 2 ~ 1,
    generalElectionVote == 1 & p_past_vote_2017 == 0 ~ 1, 
    generalElectionVote == 1 & p_past_vote_2017 == 3 ~ 1,
    #add ukip voters switching as well
    generalElectionVote ==1 & p_past_vote_2017 == 6 ~ 1, 
    TRUE ~ 0
  ),
  vote_con = case_when(
    generalElectionVote == 1 ~ 1, #add brexit and ukip voters in here too
    generalElectionVote == 6 ~ 1, 
    generalElectionVote == 12 ~ 1, 
    TRUE ~ 0
  ),
  #make multionomial vote variable - brexit parties are 1, conservatives are 2, 
  #centrists are 3, 
  #labour is 4, greens are 5 
  vote = case_when(
    generalElectionVote == 1 ~ 2,
    generalElectionVote == 6 ~ 1, 
    generalElectionVote == 12 ~ 1, 
    generalElectionVote == 3 ~ 3, 
    generalElectionVote == 11 ~ 3, 
    generalElectionVote == 2 ~ 4, 
    generalElectionVote == 7 ~ 5
  ),
  #get region, pcon, oslaua names
  lad_name = labelled::to_factor(oslaua),
  pcon_name = labelled::to_factor(pcon)) %>%
  rename(PCON19NM = pcon_name) %>% 
  left_join(., lookup, by = "PCON19NM") %>% 
  filter(region != "Scotland")

dat$age <- ifelse(dat$age < 0 | dat$age > 100, NA, dat$age)
dat$leftRight <- ifelse(dat$leftRight > 10, NA, dat$leftRight)
dat$p_gross_household <- ifelse(dat$p_gross_household > 15, NA, dat$p_gross_household)
dat$p_gross_personal <- ifelse(dat$p_gross_personal > 14, NA, dat$p_gross_personal)
dat$p_education <- ifelse(dat$p_education > 18, NA, dat$p_education)
dat$econPersonalProsp <- ifelse(dat$econPersonalProsp > 5, NA, dat$econPersonalProsp)
dat$redistSelf <- ifelse(dat$redistSelf > 10, NA, dat$redistSelf)
dat$p_ethnicity <- ifelse(dat$p_ethnicity > 14, NA, dat$p_ethnicity)
dat <- dat %>% filter(!is.na(p_ethnicity))
dat$is_white <- ifelse(dat$p_ethnicity == 1, 1, 0 )
dat$has_degree <- ifelse(dat$p_edlevel > 3, 1, 0)
dat$satDemUK <- ifelse(dat$satDemUK == 9999, NA, dat$satDemUK)


 

#change working directory to folder that has the other tuff
setwd('C:/Users/dapon/Dropbox/Harvard/G3/stat151/final_project/data')
locals_codes <- read_csv("bes/Local_Authority_Districts_(December_2019)_Names_and_Codes_in_the_United_Kingdom.csv")
unem <- read_csv("economic/localauthorities_unemployment.csv") %>% 
  rename(LAD19CD = code)

#merge dat and local authorities, to get local authority codes in there
dat <- dat %>% 
  mutate(LAD19NM = lad_name) %>% 
  left_join(., locals_codes, by = "LAD19NM")

#change working directory back to main data folder
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
#read in pcon codes and join to data
#pcon_codes <- read_csv("uk_geography/pcon_data/pcon_codes.csv") %>% 
 # dplyr::select(-FID)
#dat <- dat %>% 
 # mutate(PCON19NM = pcon_name) %>% 
#  left_join(., pcon_codes, by= "PCON19NM") %>% 
 # filter(!(is.na(PCON19CD)))

#deprivaiton data
dep <- read_csv("uk_geography/pcon_data/pcon_deprivation.csv")
dep <- dep %>% 
  rename(PCON19CD = ONSConstID, deprivation2015 = `IMD rank 2015`, 
         deprivation2019 = `IMD rank 2019`) %>% 
  dplyr::select(PCON19CD, starts_with("depr"))
#merge to the data
#we do get some missingness here, but it's all scottish constituencies that are still in here for some reason
dat <- left_join(dat, dep, by = "PCON19CD")

#housing data
housing <- read_csv("uk_geography/pcon_data/pcon_house_prices_clean.csv") %>% 
  rename(PCON19CD = pcon) %>% 
  dplyr::select(PCON19CD, starts_with("House"),starts_with("Reg"),starts_with("Const"))
#merge to the data
dat <- left_join(dat, housing, by = "PCON19CD") %>% 
  #filter out remaining scottish constituencies
  mutate(country_letter = substr(PCON19CD, 1, 1)) %>% 
  filter(country_letter %in% c("E","W"))

#read in ethnicity data
eth <- read_csv("uk_geography/pcon_data/pcon_ethnicity.csv") %>% 
  rename(PCON19CD = ONSConstID) %>% 
  dplyr::select(PCON19CD, ends_with("Const%"), ends_with("Region%"))
#merge to teh data
dat <- left_join(dat, eth, by = "PCON19CD")
dat <- dat %>% dplyr::select(-starts_with("rate"))


#read in unemployment data 
unem <- read_csv("uk_geography/pcon_data/pcon_unemployment_to_2010.csv")
dates <- as.Date(c("2019-01-01","2019-07-01","2020-02-01","2020-06-01", "2010-05-01"))

#filter for certain dates, do some pivoting, etc. 
unem <- unem %>% 
  filter(!(RegionName %in% c("Scotland","Northern Ireland"))) %>% 
  mutate(date = as.Date(DateOfDataset, format = "%m/%d/%Y")) %>% 
  filter(date %in% dates) %>% 
  dplyr::select(-ConstituencyName, -RegionID, -CountryID, -CountryName, -DateThisUpdate, -DateOfDataset, -X15,
         -starts_with("UnempCountry"), -ends_with("Number")) %>% 
  pivot_wider(id_cols = c("ONSConstID","RegionName"), names_from = "date", 
              values_from = UnempConstRate:UnempRegionRate)

#do some informative renaming of columns 
unem <- unem %>% 
  rename(unemp_const_covid = `UnempConstRate_2020-06-01`, 
         unemp_region_covid = `UnempRegionRate_2020-06-01`, 
         unem_const_pre_covid = `UnempConstRate_2020-02-01`, 
         unemp_region_pre_covid = `UnempRegionRate_2020-02-01`,
         unemp_const_post_crisis = `UnempConstRate_2010-05-01`,
         unemp_region_post_crisis = `UnempRegionRate_2010-05-01`, 
         PCON19CD = ONSConstID)

#join unemployment and main data
dat <- left_join(dat, unem, by = "PCON19CD")

#assign midpoitns for gross houseohld income
#do this while looking at the tabulation in stata
dat <- dat %>% 
  mutate(gross_household_mid = case_when(
    p_gross_household == 1 ~ 2500, 
    p_gross_household == 2 ~ 7500, 
    p_gross_household == 3 ~ 12500,
    p_gross_household == 4 ~ 17500,
    p_gross_household == 5 ~ 22500,
    p_gross_household == 6 ~ 27500,
    p_gross_household == 7 ~ 32500,
    p_gross_household == 8 ~ 37500,
    p_gross_household == 9 ~ 42500,
    p_gross_household == 10 ~ 47500,
    p_gross_household == 11 ~ 55000,
    p_gross_household == 12 ~ 65000,
    p_gross_household == 13 ~ 85000,
    p_gross_household == 14 ~ 125000,
    p_gross_household == 15 ~ 175000,
    TRUE ~ p_gross_household
  )) %>% 
  mutate(gross_household_mid = ifelse(gross_household_mid %in% c(16, 17), NA, gross_household_mid))

#read in median income data
inc <- read_csv("uk_geography/pcon_data/pcon_income_data_2018.csv") %>% 
  dplyr::select(-starts_with("X"))

dat <- left_join(dat, inc, by = "LAD19CD")


#select relevant variables for concision
df <- dat %>% 
  dplyr::select(wt:partyIdSqueeze, riskPoverty:econGenProsp, 
                starts_with("immig"), leftRight, starts_with("cuts"), satDemUK, 
                globalGoodOverall,privateEnterprise:trustWestminster,
                ends_with("ness"),workingStatus:sector, anyUni, starts_with("life"),
                subjClass, starts_with("ns"), riskScale, oslaua, lr_scale, jobzone:pcon, 
                starts_with("p_"), switch_to_con:`PopOtherConst%`, gross_household_mid, region)



#save cleaned dataset
write.csv(df, "bes/internet_panel/wave_20_clean.csv") 


