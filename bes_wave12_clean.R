setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(haven)
library(estimatr)
library(sjlabelled)
library(lfe)
library(lmerTest)
raw <- read_dta("data/bes/internet_panel/bes_wave12_2017campaign.dta")

df <- raw %>% 
  dplyr::select(id, wt, generalElectionVote, starts_with("cuts"),
         starts_with("local"), starts_with("econ"), changeEconomy, 
         starts_with("p_"),  oslaua, pcon, partyId, houseBuild,
         councilHouse, age, gender, gor, redistSelf, leftRight)

df$cutsTooFarLocal <- ifelse(df$cutsTooFarLocal > 5, NA, df$cutsTooFarLocal)
df$cutsTooFarNational <- ifelse(df$cutsTooFarNational > 5, NA, df$cutsTooFarNational)
df$changeEconomy <- ifelse(df$changeEconomy > 5, NA, df$changeEconomy)
df$econGenRetro <- ifelse(df$econGenRetro > 5, NA, df$econGenRetro)
df$econPersonalRetro <- ifelse(df$econPersonalRetro > 5, NA, df$econPersonalRetro)
df$localEcon1520Yr <- ifelse(df$localEcon1520Yr > 5, NA, df$localEcon1520Yr)
df$localEconNow <- ifelse(df$localEconNow > 5, NA, df$localEconNow)
df$p_ethnicity <- ifelse(df$p_ethnicity == 16, NA, df$p_ethnicity)
df$leftRight <- ifelse(df$leftRight > 10, NA, df$leftRight)
df$redistSelf <- ifelse(df$redistSelf > 10, NA, df$redistSelf)
df$p_ethnicity <- as.numeric(df$p_ethnicity)
df <- df %>% 
  mutate(white = case_when(
    p_ethnicity %in% c(1, 2) ~ 1, 
    p_ethnicity > 2 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ), white_british = case_when(
    p_ethnicity == 1 ~ 1, 
    p_ethnicity > 1 & p_ethnicity < 16 ~ 0, 
    TRUE ~ p_ethnicity
  ), own_house = case_when(
    p_housing %in% c(1, 2, 3) ~ 1, 
    TRUE ~ 0
  ))
df$p_gross_household <- ifelse(df$p_gross_household > 15, NA, df$p_gross_household)
df$p_gross_personal <- ifelse(df$p_gross_personal > 14, NA, df$p_gross_personal )
df$age <- ifelse(df$age < 0, NA, df$age)
df$male <- ifelse(df$gender == 1, 1, 0)
df$vote <- ifelse(df$generalElectionVote > 13 | df$generalElectionVote == 0, NA, df$generalElectionVote)
df <- df %>% filter(!is.na(df$vote))
df$vote_con <- ifelse(df$generalElectionVote == 1, 1, 0)

#read in geography lookups
geo <- read_csv("data/bes/internet_panel/bes_wave12_geography.csv") %>% 
  rename(oslaua_name = oslaua, 
         pcon_name = pcon, region_name = gor)

#merge this in with df 
df <- left_join(df, geo, by = "id")

#read in local authority lookup
locals <- read_csv("data/uk_geography/localauthorities_codes_dec2017.csv")
#this leaves some NAs, probably beacuse of faulty names ... will deal later
df <- left_join(df, locals, by = c("oslaua_name" = "LAD17NM") )
testview <- df %>% dplyr::select(oslaua, oslaua_name, starts_with("LAD"), gor, everything())

#read in median income data
income <- read_csv("data/uk_geography/local_authority_data/annual_gross_pay_2017.csv")
#merge this with the df, by loacl authoirty code
income <- income %>% 
  mutate(median_income = as.numeric(str_remove(median, ",")), 
         mean_income = as.numeric(str_remove(mean, ","))) %>%
  dplyr::select(-median, -mean)

df <- left_join(df, income, by = c("LAD17CD" = "code"))


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
test <- left_join(df, houses, by = c("region_name", "LAD17CD" = "lad_code"))


#read in pcon codes
pcon_codes <- read_csv("data/uk_geography/pcon_data/pcon_codes.csv")


summary(felm(data = test, leftRight ~ localEconNow*econGenRetro + median_income + 
               age + gender  + white_british + p_gross_household + 
               price_2017))


mod <- lmer(data = test, redistSelf ~ localEconNow *changeEconomy + median_income + 
               age + male + white_british + p_gross_household + 
               price_2017 + pct_price_change_10yr + (1 | region_name))

expand.grid(localEconNow = unique(test$localEconNow),
            region_name = "South East",
            white_british = 1, #white  
            age = 40, #this is median age in Britain
            male = 1,
            p_gross_household = median(test$p_gross_household, na.rm = TRUE), 
            median_income = median(test$median_income[test$region_name == "South East"], na.rm = TRUE),
            price_2017 = median(test$price_2017[test$region_name == "South East"], na.rm =  TRUE),
            pct_price_change_10yr = median(test$pct_price_change_10yr[test$region_name == "South East"], na.rm = TRUE),
            changeEconomy = (unique(test$changeEconomy))) %>% 
  as_tibble() %>% na.omit() %>% 
  mutate(predicted =  predict(mod, newdata =.,
                              type = "response", allow.new.levels = TRUE)) %>% 
  ggplot() + 
  geom_line(aes(x = localEconNow, y = predicted, color = factor(changeEconomy)))
