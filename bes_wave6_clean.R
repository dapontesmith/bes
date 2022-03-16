library(tidyverse)
library(DescTools)
library(parlitools)
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
raw <- read_dta("data/bes/internet_panel/bes_wave6_2015post.dta")

df <- raw %>% 
  dplyr::select(id, wt, generalElectionVote, 
                  partyId, trustMPs, 
                  goodTimePurchase, riskPoverty, riskUnemployment, 
                  econPersonalRetro, econGenRetro, cutsTooFarNational, 
                  cutsTooFarNHS, cutsTooFarLocal, privatTooFar,
                  immigrationLevel, euRefVote, satDemUK, oslaua, cci, ccinoIT, 
                  justIT, country, gender, age, ageGroup, gor, pcon, pano,
                  p_gross_household, p_gross_personal, p_housing, p_ethnicity, 
                  p_edlevel, p_religion, p_socgrade, redistSelf,
                preferMPLocal, preferMPWorkClass, preferMPYoung, preferMPDegree, 
                econPersonalProsp, econGenProsp)
df[df == 9999] <- NA 
df[df == 997] <- NA

df <- df %>% 
  mutate(male = ifelse(gender == 1, 1, 0),
  age = ifelse(age < 0, NA, age),
  p_gross_household = ifelse(p_gross_household > 15, NA, p_gross_household),
  p_gross_personal = ifelse(p_gross_personal > 14, NA, p_gross_personal), 
  own_house = case_when(
    p_housing %in% c(1, 2, 3) ~ 1, 
    TRUE ~ 0
  ), p_ethnicity = as.numeric(p_ethnicity), 
  white = case_when(
    p_ethnicity %in% c(1, 2) ~ 1, 
    p_ethnicity > 2 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ), white_british = case_when(
    p_ethnicity == 1 ~ 1, 
    p_ethnicity > 1 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ), p_socgrade = ifelse(p_socgrade > 6, NA, p_socgrade), 
  vote = ifelse(is.na(generalElectionVote), NA, generalElectionVote),
  vote_con = case_when(
    vote == 1 ~ 1, 
    vote != 1 & !is.na(vote) ~ 0, 
    TRUE ~ vote
  ), vote_right = case_when(
    vote %in% c(1, 6, 8, 12) ~ 1, 
    vote %in% c(2, 3, 4, 5, 7, 9, 11, 13) ~ 0, 
    TRUE ~ vote
  ), vote_spectrum = case_when( #lower values = further right 
    vote %in% c(6, 8, 12) ~ 1, #far right 
    vote == 1 ~ 2, #conservative
    vote %in% c(11, 3) ~ 3, #lib dem, change UK
    vote == 2 ~ 4, #labour
    vote %in% c(4, 5, 7) ~ 5, #SNP, plaid, greens, 
    #vote %in% c(9, 13) ~ NA, 
    TRUE ~ vote
  ), vote_spectrum = ifelse(vote_spectrum %in% c(9, 13), NA, vote_spectrum))
df$male <- ifelse(df$gender == 1, 1, 0)


#link to census from parlitools package
cen <- census_11 %>% 
  dplyr::select(pano:population_density, ethnicity_white, ethnicity_white_british,
                employed, unemployed, retired, 
                starts_with("nssec")) %>% #variables for broad industries, see parlitools vignette,
  #these match the "ns_sec_analytic" variable in the BES)
  mutate(borough = ifelse(constituency_type == "Borough", 1, 0))
df <- left_join(df, cen, by = "pano")


#link to results from parlitools package
res <- bes_2015 %>% 
  select(pano:other_15, turnout_15, seat_change_1015) 

df <- left_join(df, cen, by = "pano")

#geo 
geo <- read_csv("data/bes/internet_panel/bes_wave6_geography.csv")
geo <- geo %>% 
  rename(pcon_name = pcon, region_name = gor, oslaua_name = oslaua)
df <- left_join(df, geo, by = "id")

#read in and merge local authority data
locals <- read_csv("data/uk_geography/local_authority_data/localauthorities_codes_apr2015.csv") %>% 
  dplyr::select(-FID)
locals$LAD15NM <- ifelse(locals$LAD15NM == "City of Edinburgh", 
                         "Edinburgh, City of", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Argyll and Bute", 
                         "Argyll & Bute", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Vale of Glamorgan", 
                         "The Vale of Glamorgan", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Rhondda Cynon Taf", 
                         "Rhondda, Cynon, Taff", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Dumfries and Galloway", 
                         "Dumfries & Galloway", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Perth and Kinross", 
                         "Perth & Kinross", locals$LAD15NM)
locals$LAD15NM <- ifelse(locals$LAD15NM == "Comhairle nan Eilean Siar", 
                         "Eilean Siar", locals$LAD15NM)
df <- left_join(df, locals, by = c("oslaua_name" = "LAD15NM"))
#this worked without any nas! 

#read in unemployment data 
unem <- read_csv("data/uk_geography/local_authority_data/localauthorities_unemployment.csv")
unem <- unem %>% 
  dplyr::select(name, code, rate2015, rate2014, rate2010, rate2005) %>% 
  mutate(unem_change_1yr = rate2015 - rate2014, 
         unem_change_5yr = rate2015 - rate2010, 
         unem_change_10yr = rate2015 - rate2005)

df <- left_join(df, unem, by = c("LAD15CD" = "code"))



#read in disposable income data
inc <- read_csv("data/uk_geography/local_authority_data/gross_disposable_household_income_lad_2015.csv")
inc <- inc %>% rename(region_name = Region, LAD15CD = `LAU1 code`, oslaua_name = `LA name`) %>% 
  dplyr::select(region_name:oslaua_name, `2005`, `2010`,`2014`,`20151`) %>% 
  mutate(gross_income_change_1yr = `20151` - `2014`, 
         gross_income_change_5yr = `20151` - `2010`,
         gross_income_change_10yr = `20151` - `2005`) %>% 
  rename(gross_disp_income2015 = `20151`,
         gross_disp_income2014 = `2014`,
         gross_disp_income_2010 = `2010`, 
         gross_disp_income_2005 = `2005`) %>% 
  mutate(gross_disp_income2015_scale = scale(gross_disp_income2015))

df <- left_join(df, inc, by = c("region_name", "LAD15CD"))

