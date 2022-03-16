setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
#read in data and select relevant variables 
dat <- read_csv("bes/internet_panel/wave_20_clean.csv")
dat <- dat %>% 
  mutate(vote_choice = case_when(
    vote == 1 ~ "Brexit/Ukip",
    vote == 2 ~ "Tory", 
    vote == 3 ~ "Lib Dem / Change UK",
    vote == 4 ~ "Labour", 
    vote == 5 ~ "Greens / Plaid Cymru"
  )) 

names(dat) <- str_remove(names(dat), "%")

#brms wants this to be a factor
#dat$vote <- as.factor(dat$vote)

#scale gross_household_mid
dat$gross_household_mid_scale <- scale(dat$gross_household_mid)

#make binary satdem variable
dat <- dat %>% 
  mutate(satDemBinary = case_when(
    satDemUK %in% c(3, 4) ~ 1, 
    satDemUK %in% c(1, 2) ~ 0, 
    TRUE ~ satDemUK
  ))
#just scale everyhting
dat_scale <- dat %>% 
  mutate(age = scale(age),
         gross_household_mid = scale(gross_household_mid),
         ConstPercentChangeFiveYr = scale(ConstPercentChangeFiveYr),
         HouseConstMedianPrice = scale(HouseConstMedianPrice),
         male = case_when(
           gender == 2 ~ 1, 
           gender == 1 ~ 0, 
           TRUE ~ gender
         ))

logit_income_region <- lmer(data = dat_scale,
                              immigEcon ~ p_edlevel + 
                                age + male + p_gross_household + 
                              ConstPercentChangeFiveYr*HouseConstMedianPrice + 
                              deprivation2019 + leftRight + 
                                (1 + p_edlevel | region))
                             #family = binomial(link = "logit"))
                             #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

dep_region <- dat_scale %>%
  group_by(region) %>% 
  summarise(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            HouseConstMedianPrice = median(HouseConstMedianPrice, na.rm = TRUE),
            deprivation2019 = median(deprivation2019, na.rm = TRUE))
tograph <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                       region =  unique(dat9$region),
                       is_white = 1, #white  
                       age = 0, #this is median age in Britain
                       male = 1,
                       p_gross_household = median(dat_scale$p_gross_household, na.rm = TRUE), 
                       leftRight = 5) %>%  
  as_tibble() %>% na.omit() %>% 
  left_join(., dep_region, by = "region") %>% 
  mutate(predicted =  predict(logit_income_region, newdata =.,
                              type = "response", allow.new.levels = TRUE)) 

tograph %>%
  #mutate(Region = factor(region, levels = levels)) %>% 
  ggplot(.) + 
  geom_line(aes(x = p_edlevel, y = predicted, color = region)) + 
  theme_minimal()
