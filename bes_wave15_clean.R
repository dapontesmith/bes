library(tidyverse)
library(haven)
library(parlitools)
library(DescTools)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

raw <- read_dta("data/bes/internet_panel/bes_wave15.dta")

df <- raw %>% 
  dplyr::select(id, wt, turnoutUKGeneral, generalElectionVote, partyId, 
         polAttention, pidWeThey, pidMyParty, pidRuinDay, riskPoverty, 
         riskUnemployment, goodTimePurchase, econPersonalRetro, econGenRetro,
         trustMPs, satDemUK, immigEcon, immigCultural, redistSelf, changeEconomy, 
         changeCostLive, mpVoteConstSelf, taxHighIncome, taxMiddleIncome, taxLowIncome,
         spendHealth, spendPensions, spendUnemp, deficitReduce, howToReduceDeficit, 
         regionEcon, localEcon, londonEcon, richEcon, poorEcon, emEcon, wbEcon, mcEcon, 
         wcEcon, selfEcon, anyUni, pano, country, age, gender, gor, pcon, oslaua,
         p_housing, p_marital, p_socgrade, p_work_stat, p_job_sector, p_gross_household, 
         p_gross_personal, p_ethnicity, p_edlevel, starts_with("p_past"))
df[df == 9999 | df == 997] <- NA 


#clean all the variables ... 

df$generalElectionVote <- ifelse(df$generalElectionVote %in% c(0, 9999), NA, df$generalElectionVote)
df$p_gross_household <- ifelse(df$p_gross_household > 15, NA, df$p_gross_household)
df$p_gross_personal <- ifelse(df$p_gross_personal > 14, NA, df$p_gross_personal )
df$age <- ifelse(df$age < 0, NA, df$age)
df$male <- ifelse(df$gender == 1, 1, 0)
df$vote <- ifelse(df$generalElectionVote > 13 | df$generalElectionVote == 0, NA, df$generalElectionVote)
df$voteCon <- ifelse(df$generalElectionVote == 1, 1, 
                      ifelse(is.na(df$generalElectionVote), NA, 0))
df$voteConUkip <- ifelse(df$generalElectionVote %in% c(1, 6), 1, 
                           ifelse(is.na(df$generalElectionVote), NA, 0))
df$p_socgrade <- ifelse(df$p_socgrade > 6, NA, df$p_socgrade)
df$immigEcon <- ifelse(df$immigEcon > 7, NA, df$immigEcon)

#peripheralness variables
df <- df %>% 
  mutate(londonSelfEcon = londonEcon - selfEcon, 
         londonLocalEcon = londonEcon - localEcon,
         londonRegionEcon = londonEcon - regionEcon,
         voteSpectrum = case_when( #lower values = further right 
           vote %in% c(6, 8, 12) ~ 1, #far right 
           vote == 1 ~ 2, #conservative
           vote %in% c(11, 3) ~ 3, #lib dem, change UK
           vote == 2 ~ 4, #labour
           vote %in% c(4, 5, 7) ~ 5, #SNP, plaid, greens, 
           #vote %in% c(9, 13) ~ NA, 
           TRUE ~ vote
         ), voteSpectrum = ifelse(voteSpectrum %in% c(9, 13), NA, voteSpectrum),
         voteConTwoParty = case_when(
           vote == 1 ~ 1, 
           vote == 2 ~ 0, 
           TRUE ~ vote
         ), voteConTwoParty = ifelse(voteConTwoParty != 1 & voteConTwoParty != 0, 
                                     NA, voteConTwoParty),
         londonBetterSelf = case_when(
           londonSelfEcon > -1 ~ 1, 
           londonSelfEcon < 0 ~ 0, 
           TRUE ~ londonSelfEcon
         ), londonBetterLocal = case_when(
           londonLocalEcon > -1 ~ 1, 
           londonLocalEcon < 0 ~ 0, 
           TRUE ~ londonLocalEcon
         ), londonBetterRegion = case_when(
           londonRegionEcon > -1 ~ 1, 
           londonRegionEcon < 0 ~ 0, 
           TRUE ~ londonRegionEcon
         ), #rescale redist self so higher = more pro-redistribution
         redistSelf = max(redistSelf, na.rm = TRUE) - redistSelf,
         p_ethnicity = as.numeric(p_ethnicity),
         white = case_when(
           p_ethnicity %in% c(1, 2) ~ 1, 
           p_ethnicity > 2 & p_ethnicity < 16 ~ 0, 
           TRUE ~ p_ethnicity
         ), white_british = case_when(
           p_ethnicity == 1 ~ 1, 
           p_ethnicity > 1 & p_ethnicity < 16 ~ 0, 
           TRUE ~ p_ethnicity
         ))
#read in geographic variables
geo <- read_csv("data/bes/internet_panel/bes_wave15_geography.csv") %>% 
  rename(region_name = gor, pcon_name = pcon, lad_name = oslaua)
df <- left_join(df, geo, by = "id")

#read in in local authority lookup
locals <- read_csv("data/uk_geography/localauthorities_codes_dec2017.csv")
locals$LAD17NM <- ifelse(locals$LAD17NM == "City of Edinburgh", 
                         "Edinburgh, City of", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Argyll and Bute", 
                         "Argyll & Bute", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Vale of Glamorgan", 
                         "The Vale of Glamorgan", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Rhondda Cynon Taf", 
                         "Rhondda, Cynon, Taff", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Dumfries and Galloway", 
                         "Dumfries & Galloway", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Perth and Kinross", 
                         "Perth & Kinross", locals$LAD17NM)
locals$LAD17NM <- ifelse(locals$LAD17NM == "Na h-Eileanan Siar", 
                         "Eilean Siar", locals$LAD17NM)

df <- left_join(df, locals, by = c("lad_name" = "LAD17NM") )


#read in median income data
income <- read_csv("data/uk_geography/local_authority_data/annual_gross_pay_2017.csv")
#merge this with the df, by loacl authoirty code
income <- income %>% 
  mutate(median_income = as.numeric(str_remove(median, ",")), 
         mean_income = as.numeric(str_remove(mean, ","))) %>%
  dplyr::select(-median, -mean)

df <- left_join(df, income, by = c("lad_name" = "name"))

#read in pcon lookup
pcons <- read_csv("data/uk_geography/pcon_data/pcon_codes.csv") %>%
  mutate(PCON19NM = as.character(PCON19NM))
df <- left_join(df, pcons, by = c("pcon_name" = "PCON19NM")) %>% 
#this leaves six missing values, filter them out
  filter(!is.na(PCON19CD))

#read in deprivation data
dep <- read_csv("data/uk_geography/pcon_data/pcon_deprivation.csv")
dep <- dep %>% 
  rename(PCON19CD = ONSConstID, deprivation2015 = `IMD rank 2015`, 
         deprivation2019 = `IMD rank 2019`) %>% 
  dplyr::select(PCON19CD, starts_with("depr"))
#merge to the data
#we do get some missingness here, but it's all scottish constituencies that are still in here for some reason
df <- left_join(df, dep, by = "PCON19CD")

#housing data
housing <- read_csv("data/uk_geography/pcon_data/pcon_house_prices_clean.csv") %>% 
  rename(PCON19CD = pcon) %>% 
  dplyr::select(PCON19CD, starts_with("House"),starts_with("Reg"),starts_with("Const"))
#merge to the data
df <- left_join(df, housing, by = "PCON19CD") 

#read in unemployment data
unem <- read_csv("data/uk_geography/pcon_data/pcon_unemployment_to_2010.csv")
dates <- as.Date(c("2019-03-01", "2018-03-01", "2010-05-01"))

unem <- unem %>% 
  mutate(date = as.Date(DateOfDataset, format = "%m/%d/%Y")) %>% 
  filter(date %in% dates) %>% 
  dplyr::select(-ConstituencyName, -RegionID, -CountryID, -CountryName, -DateThisUpdate, -DateOfDataset, -X15,
                -starts_with("UnempCountry"), -ends_with("Number")) %>% 
  pivot_wider(id_cols = c("ONSConstID","RegionName"), names_from = "date", 
              values_from = UnempConstRate:UnempRegionRate) %>% 
  mutate(unem_change_10yr_pcon = `UnempConstRate_2019-03-01` - `UnempConstRate_2010-05-01`,
         unem_change_1yr_pcon = `UnempConstRate_2019-03-01` - `UnempConstRate_2018-03-01`,
         unem_const =`UnempConstRate_2019-03-01`) %>% 
  dplyr::select(ONSConstID, starts_with("unem_")) %>% 
  rename(PCON19CD = ONSConstID)

df <- left_join(df, unem, by = "PCON19CD")

#read in local authority unemployment
unem <- read_csv("data/uk_geography/local_authority_data/localauthorities_unemployment.csv")
unem <- unem %>% 
  dplyr::select(name, code, rate2018, rate2017, rate2013, rate2008, rate1998) %>% 
  mutate(rate1998 = as.numeric(rate1998)) %>% 
  mutate(unem_change_1yr_lad = rate2018 - rate2017, 
         unem_change_5yr_lad = rate2018 - rate2013, 
         unem_change_10yr_lad = rate2018 - rate2008,
         unem_change_20yr_lad = rate2018 - rate1998)
df <- left_join(df, unem, by = c("LAD17CD" = "code"))



#create variable from census for sectoal concentration 
#and for percent knowledge-economy and manufacturing jobs 

concen <- census_11 %>% 
  dplyr::select(ons_const_id, pano, starts_with("industry")) %>% 
  mutate(agriculture_mining = industry_agriculture + industry_mining, 
         supply = industry_electricity_supply + industry_water_supply) %>% 
  dplyr::select(-industry_agriculture, -industry_mining, -ends_with("supply")) %>% 
  pivot_longer(cols = industry_manufacturing:agriculture_mining, 
               names_to = "sector", values_to = "value") %>% 
  mutate(sector = str_remove(sector, "industry_")) %>% 
  group_by(ons_const_id, pano) %>% 
  summarize(concen = Herfindahl(value, na.rm = TRUE))

ind <- census_11 %>% 
  mutate(pct_knowledge = industry_professional + industry_finance + 
           industry_communication,
         pct_manufacturing_mining = industry_manufacturing + industry_mining) %>% 
  rename(pct_manufacturing = industry_manufacturing) %>% 
  dplyr::select(ons_const_id, pano, starts_with('pct'))
concen <- left_join(concen, ind, by = c("ons_const_id","pano"))
df <- left_join(df, concen, by = c("pano"))




#census varialbes on ethniity
cen <- census_11 %>% 
  dplyr::select(pano:population_density, ethnicity_white, ethnicity_white_british,
                employed, unemployed, retired, 
                starts_with("nssec")) %>% #variables for broad industries, see parlitools vignette,
  #these match the "ns_sec_analytic" variable in the BES)
  mutate(borough = ifelse(constituency_type == "Borough", 1, 0))
df <- left_join(df, cen, by = "pano")



#get election results from 2017
res <- bes_2017 %>% 
  dplyr::select(pano, constituency_type, winner_17, con_17, lab_17, ld_17, snp_17, pc_17, ukip_17, green_17, 
                turnout_17, ends_with("1517"), ends_with("hanretty"))
df <- left_join(df, res, by = "pano")

write.csv(df, "data/bes/internet_panel/clean_data/wave15_clean.csv")




