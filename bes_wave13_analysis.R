setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(tidyverse)
library(haven)

#read in wave 13
bes <- read_dta("bes/internet_panel/bes_wave13_2017post.dta")

bes <- select_if(bes, is.numeric)

bes[bes == 9999] <- NA 
#a.out <- amelia(bes, m = 5)
dat <- bes
dat[dat == 9999] <- NA
dat <- dat %>% 
  #filter(!is.na(generalElectionVote)) %>% 
  filter(generalElectionVote %in% c(1,2,3,6,7,8,11,12)) %>% 
  filter(country %in% c(1, 3)) %>% 
  filter(!is.na(oslaua) & !is.na(pcon)) %>% 
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
  )) %>% 
  filter(gor != "Wales")

dat$age <- ifelse(dat$age < 0 | dat$age > 100, NA, dat$age)
dat$leftRight <- ifelse(dat$leftRight > 10, NA, dat$leftRight)
dat$p_gross_household <- ifelse(dat$p_gross_household > 15, NA, dat$p_gross_household)
dat$p_gross_personal <- ifelse(dat$p_gross_personal > 14, NA, dat$p_gross_personal)
dat$p_education <- ifelse(dat$p_education > 18, NA, dat$p_education)
#dat$econPersonalProsp <- ifelse(dat$econPersonalProsp > 5, NA, dat$econPersonalProsp)
#dat$redistSelf <- ifelse(dat$redistSelf > 10, NA, dat$redistSelf)
dat$p_ethnicity <- ifelse(dat$p_ethnicity > 14, NA, dat$p_ethnicity)
dat <- dat %>% filter(!is.na(p_ethnicity))
dat$is_white <- ifelse(dat$p_ethnicity == 1, 1, 0 )
dat$has_degree <- ifelse(dat$p_edlevel > 3, 1, 0)

#read in geography variables
geo <- read_csv("bes/internet_panel/bes_wave13_geography.csv")
geo <- geo %>%
  rename(region = gor, 
         lad = oslaua, 
         constit = pcon)
dat <- left_join(dat, geo, by = "id")

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



#run model
linear_educ_region <-  lmer(data = dat, 
                            vote_con ~ p_edlevel + is_white + 
                              age  + gender + p_gross_household + 
                              (1 + p_edlevel | region))

ndat_educ <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                         region =  unique(dat$region),
                         is_white = 1, #white  
                         age = 40, #this is median age in Britain
                         gender = 2, 
                         p_gross_household = median(dat$p_gross_household, na.rm = T))
#omit NAs
ndat_educ <- na.omit(ndat_educ)
#predict
probs <- predict(linear_educ_region, newdata = ndat_educ)
ndat_educ$predicted <- probs


#change variable type for graphing
ndat_educ$White <- as.factor(ndat_educ$is_white)

#create plot 
educ_plot_linear <- ggplot(ndat_educ) + 
  geom_line(aes(x = p_edlevel, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()


logit_educ_region <-  glmer(data = dat, family = binomial(link = "logit"),
                            vote_con ~ p_edlevel + is_white + 
                              age  + gender + p_gross_household + 
                              (1 + p_edlevel | region))

ndat_educ <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                         region =  unique(dat$region),
                         is_white = 1, #white  
                         age = 40, #this is median age in Britain
                         gender = 2, 
                         p_gross_household = median(dat$p_gross_household, na.rm = T))
#omit NAs
ndat_educ <- na.omit(ndat_educ)
#predict
probs <- predict(logit_educ_region, newdata = ndat_educ, type = "response")
ndat_educ$predicted <- probs


#change variable type for graphing
ndat_educ$White <- as.factor(ndat_educ$is_white)

#create plot 
educ_plot_logit <- ggplot(ndat_educ) + 
  geom_line(aes(x = p_edlevel, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()

linear_educ_region <-  lmer(data = dat, 
                            vote_con ~ p_edlevel + is_white + 
                              age  + gender + gross_household_mid + 
                              (1 + p_edlevel | region))

ndat_educ <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                         region =  unique(dat$region),
                         is_white = 1, #white  
                         age = 40, #this is median age in Britain
                         gender = 2, 
                         gross_household_mid = median(dat$gross_household_mid, na.rm = T))
#omit NAs
ndat_educ <- na.omit(ndat_educ)
#predict
probs <- predict(linear_educ_region, newdata = ndat_educ)
ndat_educ$predicted <- probs


#change variable type for graphing
ndat_educ$White <- as.factor(ndat_educ$is_white)

#create plot 
educ_plot_linear <- ggplot(ndat_educ) + 
  geom_line(aes(x = p_edlevel, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()


logit_income_region <- glmer(data = dat, 
                             family = binomial(link = "logit"), 
                             vote_con ~ p_edlevel + age + gender + 
                               p_gross_household +
                               (1 + p_gross_household | region))



ndat_income <- expand.grid(p_edlevel = 5, #A-level, but not undergraduate degree
                           region =  unique(dat$region),
                           is_white = 1, #white  
                           age = 40, #this is median age in Britain
                           gender = 2, 
                           p_gross_household= unique(dat$p_gross_household))
#omit NAs
ndat_income <- na.omit(ndat_income)
#predict
probs <- predict(logit_income_region, newdata = ndat_income, type = "response")
ndat_income$predicted <- probs


#change variable type for graphing
ndat_income$White <- as.factor(ndat_income$is_white)

#create plot 
income_plot_logit <- ggplot(ndat_income) + 
  geom_line(aes(x = p_gross_household, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Income level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()

