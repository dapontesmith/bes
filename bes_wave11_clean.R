setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(haven)
library(estimatr)
library(sjlabelled)
library(lfe)
library(lmerTest)
library(parlitools) #has various useful data for linking to BES 
library(sf)


raw <- read_dta("data/bes/internet_panel/bes_wave11_2017pre.dta")
#note that there are some people here who are indexed as in the wrong region - 
#some local authorities apparently traverse 3 regions, which is wrong - 
#will need to filter out or otherwise mark these people

df <- raw %>% 
  dplyr::select(id, wt, starts_with("p_"), ends_with("Econ"), starts_with("econ"),
                redistSelf, welfarePreference, satDemUK, 
                starts_with("risk"), approveUKGovt, starts_with("generalElection"),
                euRefVote, radical, harkBack, partyId, 
                oslaua, gor, starts_with("belong"), starts_with('change'),
                strongestConnection, pano, age, gender) %>% 
  rename(belongLocal = belongGroup_2, 
         belongWorkingClass = belongGroup_4, 
         belongMiddleClass = belongGroup_3, 
         belongRegion = belongGroup_1) %>% 
  #mark the weirdos
  group_by(gor, oslaua) %>% 
  mutate(n = n(),
         in_wrong_region = ifelse(n == 1, 1, 0))

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
df$changeEconomy <- ifelse(df$changeEconomy > 5, NA, df$changeEconomy)
df$changeNHS <- ifelse(df$changeNHS > 5, NA, df$changeNHS)
df$radical <- ifelse(df$radical > 5, NA, df$radical)
df$redistSelf <- ifelse(df$redistSelf > 5, NA, df$redistSelf)
df$satDemUK <- ifelse(df$satDemUK > 5, NA, df$satDemUK)
df$welfarePreference <- ifelse(df$welfarePreference > 5, NA, df$welfarePreference)
df$generalElectionVote <- ifelse(df$generalElectionVote %in% c(0, 9999), NA, df$generalElectionVote)
df$regionEcon <- ifelse(df$regionEcon > 5, NA, df$regionEcon)
df$emEcon <- ifelse(df$emEcon > 5, NA, df$emEcon)
df$mcEcon <- ifelse(df$mcEcon > 5, NA, df$mcEcon)
df$selfEcon <- ifelse(df$selfEcon > 5, NA, df$selfEcon)
df$wcEcon <- ifelse(df$wcEcon > 5, NA, df$wcEcon)
df$p_gross_household <- ifelse(df$p_gross_household > 15, NA, df$p_gross_household)
df$p_gross_personal <- ifelse(df$p_gross_personal > 14, NA, df$p_gross_personal )
df$age <- ifelse(df$age < 0, NA, df$age)
df$male <- ifelse(df$gender == 1, 1, 0)
df$vote <- ifelse(df$generalElectionVote > 13 | df$generalElectionVote == 0, NA, df$generalElectionVote)
df$vote_con <- ifelse(df$generalElectionVote == 1, 1, 
                      ifelse(is.na(df$generalElectionVote), NA, 0))
df$vote_con_ukip <- ifelse(df$generalElectionVote %in% c(1, 6), 1, 
                           ifelse(is.na(df$generalElectionVote), NA, 0))
df$p_socgrade <- ifelse(df$p_socgrade > 6, NA, df$p_socgrade)
df$immigEcon <- ifelse(df$immigEcon > 7, NA, df$immigEcon)
#make variable for difference in perception between local/regional economies and london's 
#0 = same, positive = london did better, negative = local/region did better
df$londonLocalEcon <- df$londonEcon - df$localEcon
df$londonRegionEcon <- df$londonEcon - df$regionEcon
df$londonSelfEcon <- df$londonEcon - df$selfEcon
df$regionLocalEcon <- df$localEcon - df$regionEcon
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
  redistSelf = max(redistSelf, na.rm = TRUE ) - redistSelf, 
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
    p_ethnicity > 1 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ))

#LINK LOCAL AUTHORITY AND PCON-LEVEL VARIABLES
#read in geography lookups
geo <- read_csv("data/bes/internet_panel/bes_wave11_geography.csv") %>% 
  rename(oslaua_name = oslaua, 
         pcon_name = pcon, region_name = gor)

#merge this in with df 
df <- left_join(df, geo, by = "id")

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
df <- left_join(df, locals, by = c("oslaua_name" = "LAD17NM") )

nas <- test %>% 
  filter(is.na(oslaua_name)) %>% select(oslaua_name, oslaua, LAD17CD)

#read in median income data
income <- read_csv("data/uk_geography/local_authority_data/annual_gross_pay_2017.csv")
#merge this with the df, by loacl authoirty code
income <- income %>% 
  mutate(median_income = as.numeric(str_remove(median, ",")), 
         mean_income = as.numeric(str_remove(mean, ","))) %>%
  dplyr::select(-median, -mean)

df <- left_join(df, income, by = c("oslaua_name" = "name"))


#read in house price data
houses <- read_csv("data/uk_geography/local_authority_data/house_prices_to_1995.csv") %>% 
  rename(region_code = `Region/Country code`, region_name = `Region/Country name`,
         lad_code = `Local authority code`, lad_name = `Local authority name` ) %>% 
  pivot_longer(cols = `Year ending Dec 1995`:`Year ending Jun 2020`, 
               names_to = "period", values_to = "median_price") %>% 
  dplyr::select(-X104, -X105) %>% 
  filter(str_detect(period, "Dec") == TRUE) %>% 
  mutate(year = as.numeric(str_remove(period, "Year ending Dec "))) %>% 
  dplyr::select(-period) %>%
  filter(year < 2018) %>% 
  group_by(region_name, region_code, lad_code, lad_name) %>% 
  summarize(price_2017 = median_price[year == 2017], 
            price_2007 = median_price[year == 2007], 
            price_2016 = median_price[year == 2016],
            price_2012 = median_price[year == 2012]) %>%
  mutate(pct_price_change_1yr = (price_2017 - price_2016) / price_2016,
         pct_price_change_5yr = (price_2017 - price_2012) / price_2012, 
         pct_price_change_10yr = (price_2017 - price_2007) / price_2007) %>% 
  dplyr::select(region_name:price_2017, starts_with("pct"))

#join this in with the df 
df <- left_join(df, houses, by = c("region_name", "oslaua_name" = "lad_name"))

#this is just for viewing purposes
df <- df %>% 
  select(oslaua, oslaua_name, pcon_name, region_name, LAD17CD, code, 
         median_income, everything())

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
concen <- left_join(concen, ind, by = c("ons_const_id", "pano"))
df <- left_join(df, concen, by = c("ons_const_id", "pano"))


write.csv(df, "data/bes/internet_panel/clean_data/wave11_clean.csv")



#read in carreras data




#make dataframe of proportion o
london_region_diff <- df %>% 
  group_by(gor, region_name) %>% 
  summarize(regionLondonEconDiff = mean(regionLondonEconDiff, na.rm = TRUE))

london_local_diff_oslaua <- df %>% 
  group_by(LAD17CD, oslaua_name, oslaua) %>% 
  summarize(localLondonEconDiff = mean(localLondonEconDiff, na.rm = TRUE),
            n = n()) %>% 
  filter(n > 1)














#read in shapefile of regions 
shp <- st_read("data/uk_geography/shapefiles/NUTS_Level_1_(January_2018)_Boundaries.shp")
shp <- shp %>% 
  mutate(region_name = str_replace(nuts118nm, " \\(England\\)",""),
         region_name = ifelse(region_name == 'Yorkshire and The Humber',
                              "Yorkshire and the Humber", region_name))
diffs_shp <- left_join(shp, london_region_diff, by = "region_name")
diffs_shp <- st_transform(diffs_shp, "+proj=longlat +datum=WGS84")

#read in shapefile of oslauas
oslaua_shp <- st_read("data/uk_geography/shapefiles/lad17.shp")

local_diffs_shp <- left_join(oslaua_shp, london_local_diff_oslaua, 
                             by = c("lad17cd" = "LAD17CD") )

local_diffs_shp <- st_transform(local_diffs_shp, "+proj=longlat +datum=WGS84")

local_diffs_shp %>% 
  ggplot() + 
  geom_sf(aes(fill = localLondonEconDiff), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank())
