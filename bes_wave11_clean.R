setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(haven)
library(estimatr)
library(sjlabelled)
library(lfe)
library(lmerTest)
library(parlitools) #has various useful data for linking to BES 
library(DescTools)
rm(list = ls())



raw <- read.csv("data/bes/internet_panel/bes_wave11_msoa_clean.csv")
#note that there are some people here who are indexed as in the wrong region - 
#some local authorities apparently traverse 3 regions, which is wrong - 
#will need to filter out or otherwise mark these people

df <- raw %>%
  dplyr::select(
    id,
    starts_with("wt"),
    starts_with("p_"),
    ends_with("Econ"),
    starts_with("econ"),
    starts_with("warm"),
    redistSelf,
    starts_with("brit"),
    welfarePreference,
    satDemUK,
    euID,
    starts_with("risk"),
    approveUKGovt,
    starts_with("generalElection"),
    euRefVote,
    radical,
    harkBack,
    partyId,
    starts_with("efficacy"),
    oslaua,
    gor,
    starts_with("belong"),
    starts_with("benInteg"),
    starts_with('change'),
    starts_with("immig"),
    strongestConnection,
    pano,
    age,
    gender,
    houseBuild:soc2010W6W7W8W9
  ) %>%
  rename(
    belongLocal = belongGroup_2,
    belongWorkingClass = belongGroup_4,
    belongMiddleClass = belongGroup_3,
    belongRegion = belongGroup_1,
    msoa = p_msoa11,
    soc2010 = soc2010W6W7W8W9
  ) %>%
  #mark the weirdos
  group_by(gor, oslaua) %>%
  mutate(n = n(),
         in_wrong_region = ifelse(n == 1, 1, 0)) %>%
  ungroup() %>%
  mutate(
    londonEcon = as.numeric(londonEcon),
    localEcon = as.numeric(localEcon),
    selfEcon = as.numeric(selfEcon)
  ) %>% 
  #pget only people who appear once in the data 
  group_by(id) %>% 
  mutate(n = n()) %>%
  filter(n == 1)

#clean all the variables ... 
df[df == 9999] <- NA 
df$londonEcon <- ifelse(df$londonEcon > 5, NA, df$londonEcon)
df$localEcon <- ifelse(df$localEcon > 5, NA, df$localEcon)
df$changeEconomy <- ifelse(df$changeEconomy > 5, NA, df$changeEconomy)
df$changeCostLive <- ifelse(df$changeCostLive > 5, NA, df$changeCostLive)
df$belongLocal <- ifelse(df$belongLocal > 5, NA, df$belongLocal)
df$belongWorkingClass <- ifelse(df$belongWorkingClass > 5, NA, df$belongWorkingClass)
df$belongMiddleClass <- ifelse(df$belongMiddleClass > 5, NA, df$belongMiddleClass)
df$belongRegion <- ifelse(df$belongRegion > 5, NA, df$belongRegion)
df$changeCostLive <- ifelse(df$changeCostLive > 5, NA, df$changeCostLive)
df$changeEconomy <-
  ifelse(df$changeEconomy > 5, NA, df$changeEconomy)
df$changeNHS <- ifelse(df$changeNHS > 5, NA, df$changeNHS)
df$radical <- ifelse(df$radical > 5, NA, df$radical)
df$redistSelf <- ifelse(df$redistSelf > 10, NA, df$redistSelf)
df$satDemUK <- ifelse(df$satDemUK > 5, NA, df$satDemUK)
df$welfarePreference <-
  ifelse(df$welfarePreference > 5, NA, df$welfarePreference)
df$generalElectionVote <-
  ifelse(df$generalElectionVote %in% c(0, 9999),
         NA,
         df$generalElectionVote)
df$regionEcon <- ifelse(df$regionEcon > 5, NA, df$regionEcon)
df$emEcon <- ifelse(df$emEcon > 5, NA, df$emEcon)
df$mcEcon <- ifelse(df$mcEcon > 5, NA, df$mcEcon)
df$selfEcon <- ifelse(df$selfEcon > 5, NA, df$selfEcon)
df$wcEcon <- ifelse(df$wcEcon > 5, NA, df$wcEcon)
df$p_gross_household <-
  ifelse(df$p_gross_household > 15, NA, df$p_gross_household)
df$p_gross_personal <-
  ifelse(df$p_gross_personal > 14, NA, df$p_gross_personal)
df$age <- ifelse(df$age < 0, NA, df$age)
df$male <- ifelse(df$gender == 1, 1, 0)
df$vote <- ifelse(df$generalElectionVote > 13 | df$generalElectionVote == 0, NA, df$generalElectionVote)
df$vote_con <- ifelse(df$generalElectionVote == 1, 1, 
                      ifelse(is.na(df$generalElectionVote), NA, 0))
df$vote_con_ukip <- ifelse(df$generalElectionVote %in% c(1, 6), 1, 
                           ifelse(is.na(df$generalElectionVote), NA, 0))
df$p_socgrade <- ifelse(df$p_socgrade > 6, NA, df$p_socgrade)
#flip so higher values = higher social class
df$p_socgrade <- 7 - df$p_socgrade
df$immigEcon <- ifelse(df$immigEcon > 7, NA, df$immigEcon)
#make variable for difference in perception between local/regional economies and london's 
#0 = same, positive = london did better, negative = local/region did better
df$londonLocalEcon <- df$londonEcon - df$localEcon
df$londonRegionEcon <- df$londonEcon - df$regionEcon
df$londonSelfEcon <- df$londonEcon - df$selfEcon
df$regionLocalEcon <- df$localEcon - df$regionEcon
df$vote <- as.numeric(df$vote)
df <- df %>% mutate(
  vote_spectrum = case_when( #lower values = further right 
    vote %in% c(6, 8, 12) ~ 1, #far right 
    vote == 1 ~ 2, #conservative
    vote %in% c(11, 3) ~ 3, #lib dem, change UK
    vote == 2 ~ 4, #labour
    vote %in% c(4, 5, 7) ~ 5, #SNP, plaid, greens, 
    #vote %in% c(9, 13) ~ NA, 
    TRUE ~ vote
  ), vote_spectrum = ifelse(vote_spectrum %in% c(9, 13), NA, vote_spectrum),
  #higher values = more concenr about redistrubtion
  #redistSelf = max(redistSelf, na.rm = TRUE ) - redistSelf, 
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
  ), p_ethnicity = as.numeric(p_ethnicity),
  white = case_when(
    p_ethnicity %in% c(1, 2) ~ 1, 
    p_ethnicity > 2 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ), white_british = case_when(
    p_ethnicity == 1 ~ 1, 
    p_ethnicity > 1 & p_ethnicity < 17 ~ 0, 
    TRUE ~ p_ethnicity
  ), own_house = case_when(
    p_housing %in% c(1, 2, 3) ~ 1, 
    TRUE ~ 0
  ), partyIdName = case_when(
    partyId == 1 ~ "Conservative", 
    partyId == 2 ~ "Labour",
    partyId == 3 ~ "Lib Dem", 
    partyId == 4 ~ "SNP", 
    partyId == 5 ~ "PC", 
    partyId == 6 ~ "UKIP",
    partyId == 7 ~ "Green",
    partyId == 9 ~ "Other",
    partyId == 10 ~ "None"
  ), euID = as.numeric(euID), 
  remainID = case_when(
    euID == 1 ~ 1, 
    euID == 2 ~ 0,
    TRUE ~ euID
  ), 
  remainID = ifelse(remainID == 3, NA, remainID))
#higher values of redistSelf = more anti-immigration
df$immigSelf <- max(df$immigSelf, na.rm = TRUE) - df$immigSelf

df <- df %>% 
  mutate(
    strongestConnection = as.numeric(strongestConnection), 
    strongestConnectionTown = case_when(
    strongestConnection == 3 ~ 1,
    strongestConnection == 0 ~ 0,
    strongestConnection == 1 ~ 0,
    strongestConnection == 2 ~ 0,
    strongestConnection == 4 ~ 0,
    strongestConnection == 5 ~ 0,
    strongestConnection == 6 ~ 0,
    strongestConnection == 7 ~ 0,
    strongestConnection == 8 ~ 0,
    strongestConnection == 9 ~ 0,
    TRUE ~ NA_real_
  ), strongestConnectionLocal = case_when(
    strongestConnection == 1 ~ 1,
    strongestConnection == 3 ~ 1,
    strongestConnection == 5 ~ 1,
    strongestConnection == 7 ~ 1,
    strongestConnection == 0 ~ 0,
    strongestConnection == 2 ~ 0,
    strongestConnection == 4 ~ 0,
    strongestConnection == 6 ~ 0,
    strongestConnection == 8 ~ 0,
    strongestConnection == 9 ~ 0,
    TRUE ~ NA_real_ 
  ))

#create variable for whether respondent has children in household
df <- df %>% 
  mutate(p_hh_children = as.numeric(p_hh_children),
    children_in_household = case_when(
    p_hh_children == 1 ~ 0,
    p_hh_children == 2 ~ 1, 
    p_hh_children == 3 ~ 1, 
    p_hh_children == 4 ~ 1,
    p_hh_children == 5 ~ 1, 
    p_hh_children == 6 ~ 1
  ))
df$children_in_household <- ifelse((is.na(df$p_hh_children) | (df$p_hh_children %in% c(8,9))), NA, df$children_in_household )
#variable for whether respondent reads local newspaper most often
df$read_local <- ifelse(df$p_paper_read == 14, 1, 0)



#read in local authority lookup
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
#The only NAs left here are people with oslaua "None"
df <- left_join(df, locals, by = c("oslaua" = "LAD17NM") )






#read in median income data
income <- read_csv("data/uk_geography/local_authority_data/annual_gross_pay_2017.csv")
#merge this with the df, by loacl authoirty code
income <- income %>% 
  mutate(median_income = as.numeric(str_remove(median, ",")), 
         mean_income = as.numeric(str_remove(mean, ","))) %>%
  dplyr::select(-median, -mean)

df <- left_join(df, income, by = c("LAD17CD" = "code"))



#read in house price data
houses <-
  read_csv("data/uk_geography/local_authority_data/house_prices_to_1995.csv") %>%
  rename(
    region_code = `Region/Country code`,
    region_name = `Region/Country name`,
    lad_code = `Local authority code`,
    lad_name = `Local authority name`
  ) %>%
  pivot_longer(
    cols = `Year ending Dec 1995`:`Year ending Jun 2020`,
    names_to = "period",
    values_to = "median_price"
  ) %>%
  dplyr::select(-X104,-X105) %>%
  filter(str_detect(period, "Dec") == TRUE) %>%
  mutate(year = as.numeric(str_remove(period, "Year ending Dec "))) %>%
  dplyr::select(-period) %>%
  filter(year < 2018) %>%
  group_by(region_name, region_code, lad_code, lad_name) %>%
  summarize(
    price_2017 = median_price[year == 2017],
    price_2007 = median_price[year == 2007],
    price_2016 = median_price[year == 2016],
    price_2012 = median_price[year == 2012]
  ) %>%
  mutate(
    pct_price_change_1yr = (price_2017 - price_2016) / price_2016,
    pct_price_change_5yr = (price_2017 - price_2012) / price_2012,
    pct_price_change_10yr = (price_2017 - price_2007) / price_2007
  ) %>%
  dplyr::select(region_name:price_2017, starts_with("pct"))

house_with_scot <- read.csv("data/uk_geography/local_authority_data/house_prices_1995_2017_with_scotland.csv")

scothouse <- house_with_scot %>% 
  rename(date = Date, lad_name = Region_Name, price = Average_Price) %>% 
  dplyr::select(-ends_with("Change"), -Average_Price_SA) %>% 
  filter(str_detect(date, "12/1") == TRUE) %>% 
  mutate(year = str_remove(date, "12/1/")) %>% 
  dplyr::select(-date) %>% 
  pivot_wider(names_from = "year", 
              names_prefix = "price", values_from = "price") %>% 
  mutate(price_change5yr = ((price2016 - price2011 ) / price2011)*100, 
         price_change10yr = ((price2016 - price2006 ) / price2006)*100, 
         price_change20yr = ((price2016 - price1996 ) / price1996)*100) %>% 
  dplyr::select(lad_name, price2016, price2011, price2006, price1996,  ends_with("yr")) %>% 
  mutate(lad_name = case_when(
    lad_name == "City of Bristol" ~ "Bristol, City of", 
    lad_name == "Vale of Glamorgan" ~ "The Vale of Glamorgan",
    lad_name == "City of Edinburgh" ~ "Edinburgh, City of", 
    lad_name == "City of Aberdeen" ~ "Aberdeen City",
    lad_name == "Argyll and Bute" ~ "Argyll & Bute",
    lad_name == "Dumfries and Galloway" ~ "Dumfries & Galloway",
    lad_name == "City of Kingston upon Hull" ~ "Kingston upon Hull, City of",
    lad_name == "City of Dundee" ~ "Dundee City",
    lad_name == "Perth and Kinross" ~ "Perth & Kinross", 
    lad_name == "Rhondda Cynon Taf" ~ "Rhondda, Cynon, Taff", 
    lad_name == "Kensington And Chelsea" ~ "Kensington and Chelsea",
    lad_name == "Herefordshire" ~ "Herefordshire, County of",
    lad_name == "St Helens" ~ "St. Helens",
    lad_name == "City of Peterborough" ~ "Peterborough", 
    lad_name == "City of Westminster" ~ "Westminster",
    lad_name == "City of Glasgow" ~ "Glasgow City",
    lad_name == "City of Derby" ~ "Derby",
    lad_name == "Na h-Eileanan Siar" ~ "Eilean Siar",
    lad_name == "City of Nottingham" ~ "Nottingham",
    lad_name == "City of Plymouth" ~ "Plymouth",
    TRUE ~ lad_name
  ))



df <- left_join(df, scothouse, by = c("oslaua" = "lad_name"))

#join this in ith the df 
#df <- left_join(df, houses, by = c("region_name", "oslaua_name" = "lad_name"))


#join constituency results, using bes_2017 tibble from parlitools
res2017 <- bes_2017 %>% 
  dplyr::select(pano:constituency_type, ends_with("hanretty"),
                ends_with("17")) %>% 
  dplyr::select(-contains("ppc"), -rejected_vote_17) %>% 
  mutate(seat_flip = ifelse(is.na(seat_change_1517), 0, 1))
res2017$party_gain <- NA 
for(i in 1:nrow(res2017)){
  res2017$party_gain[i] <- 
    str_split(res2017$seat_change_1517, " ")[[i]][1]
}
df <- left_join(df, res2017, by = "pano")

#read in unemployment data
unem <- read_csv("data/uk_geography/local_authority_data/localauthorities_unemployment.csv")
unem <- unem %>% 
  dplyr::select(name, code, rate2017, rate2016, rate2012, rate2007) %>% 
  mutate(unem_change_1yr = rate2017 - rate2016, 
         unem_change_5yr = rate2017 - rate2012, 
         unem_change_10yr = rate2017 - rate2007)

df <- left_join(df, unem, by = c("LAD17CD" = "code"))

#read in pcon unemployment data
unem_pcon <- read_csv("data/uk_geography/pcon_data/pcon_unemployment_to_2010.csv")

unem_pcon <- unem_pcon %>%
  filter(DateOfDataset == "5/1/2017") %>%
  dplyr::select(ONSConstID, UnempConstRate) %>% 
  rename(unem_const_rate = UnempConstRate)

#get 1- and 5-year change in pcon unemployment
unem_pcon_change <- unem_pcon %>%
  filter(DateOfDataset %in% c("5/1/2012", "5/1/2016", "5/1/2017")) %>%
  dplyr::select(ONSConstID, DateOfDataset, UnempConstRate) %>% 
  pivot_wider(id_cols = ONSConstID, 
              names_from = DateOfDataset, values_from = UnempConstRate) %>% 
  mutate(unem_const_1yr = `5/1/2017` - `5/1/2016`,
         unem_const_5yr = `5/1/2017` - `5/1/2012`) %>%
  dplyr::select(ONSConstID, unem_const_5yr, unem_const_1yr)

#join them together, then join to the main dataframe
unem_pcon <- left_join(unem_pcon, unem_pcon_change, by = "ONSConstID")

df <- left_join(df, unem_pcon, by = c("ons_const_id" = "ONSConstID"))

#do industrial concentration measures
concen <- census_11 %>%
  dplyr::select(ons_const_id, pano, starts_with("industry")) %>%
  mutate(
    agriculture_mining = industry_agriculture + industry_mining,
    supply = industry_electricity_supply + industry_water_supply
  ) %>%
  dplyr::select(-industry_agriculture,
                -industry_mining,
                -ends_with("supply")) %>%
  pivot_longer(
    cols = industry_manufacturing:agriculture_mining,
    names_to = "sector",
    values_to = "value"
  ) %>%
  mutate(sector = str_remove(sector, "industry_")) %>%
  group_by(ons_const_id, pano) %>%
  summarize(concen = Herfindahl(value, na.rm = TRUE))

ind <- census_11 %>% 
  mutate(pct_knowledge = industry_professional + industry_finance + 
           industry_communication,
         pct_manufacturing_mining = industry_manufacturing + industry_mining) %>% 
  rename(pct_manufacturing = industry_manufacturing) %>% 
  dplyr::select(ons_const_id, pano, starts_with('pct'))
concen <- left_join(concen, ind, by = c("ons_const_id", "pano"))
df <- left_join(df, concen, by = c("ons_const_id", "pano"))

cen <- census_11 %>% 
  dplyr::select(pano:population_density, ethnicity_white, ethnicity_white_british,
                employed, unemployed, retired, 
                starts_with("nssec")) #variables for broad industries, see parlitools vignette,
  #these match the "ns_sec_analytic" variable in the BES)
df <- left_join(df, cen, by = c("ons_const_id","pano"))


####################
## code the soc 2010 variable
#################
df$knowledge_sector <- ifelse(substr(df$soc2010, 1, 2) == "11", 1, df$soc2010)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "211", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "212", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "213", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "214", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "215", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "221", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 4) == "2311", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 4) == "2312", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "241", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "247", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "311", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "312", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "313", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "341", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "342", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "344", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "356", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "353", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "411", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "412", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(substr(df$knowledge_sector, 1, 3) == "413", 1, df$knowledge_sector)
df$knowledge_sector <- ifelse(df$knowledge_sector != 1 & 
                                !(is.na(df$knowledge_sector)), 0, df$knowledge_sector)



#read in the house tenure data
tenure <- read_csv("data/uk_geography/housing_data/housing_tenure_by_constituency.csv")

tenure <- tenure %>% 
  dplyr::select(ONSConstID:CountryName, 
                ends_with("All"), starts_with("CON")) %>%
  dplyr::select(ONSConstID:`CON%Own_mort`, -starts_with("REG") , - starts_with("CTRY")) %>% 
  dplyr::select(ONSConstID, ConstituencyName, `CON%Own`, 
                `CON%Own_out`, `CON%Own_mort`)



df <- left_join(df, tenure, by = c("pcon" = "ConstituencyName"))




#read in wave 10 data
ethno <- read_csv("data/bes/internet_panel/bes_wave10_ethno_vars.csv")
ethno <- ethno %>%
  filter(wave10 == 1 & wave11 == 1) %>%
  dplyr::select(-wave10, -wave11)
ethno[ethno == 9999] <- NA

#recode ethno variables so higher = more ethnocentric / less cosmopolitan
#refer to the BES full codebook for this purpose
ethno <- ethno %>%
  mutate(ethno1 = max(ethno1W10, na.rm = TRUE) + 1 - ethno1W10,
         ethno3 = max(ethno3W10, na.rm = TRUE) + 1 - ethno3W10,
         ethno6 = max(ethno6W10, na.rm = TRUE) + 1 - ethno6W10,
         ethno4 =ethno4W10,
         ethno2 = ethno2W10,
         ethno5 = ethno5W10) %>%
  dplyr::select(id, ethno1:ethno5)




#join this to the ethnocentrism variables
test <- left_join(df, ethno, by = "id")

write.csv(test, "data/bes/internet_panel/clean_data/wave11_clean.csv")






test_minus_ethno <- test %>%
  dplyr::select(-ethno1, -ethno2, -ethno3, -ethno4, -ethno5, -ethno6)

id_twice <- df %>%
  group_by(id) %>%  
  mutate(n = n()) %>%
  filter(n > 1)

id_twice_ethno <- test_minus_ethno %>%
  group_by(id) %>% 
  mutate(n = n()) %>%
  filter(n > 1)

all(apply(id_twice, 2, function(x) length(unique(x)) == 1) == TRUE)

