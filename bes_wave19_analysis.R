library(tidyverse)
library(lmerTest)
library(sjstats)
library(arm)
library(stargazer)
library(ordinal)
library(rstan)
library(brms)
library(sjPlot)
library(expss)
library(xtable)
library(parallel)
#Clear workspace - BEWARE
rm(list = ls())

#setwd 
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")

dat <- read_csv("bes/internet_panel/bes_wave19_clean.csv")

# call this once to distribute chains across cpu cores:
options(mc.cores=parallel::detectCores())



medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE),
            deprivation2019 = median(deprivation2019, na.rm = TRUE),
            gross_household = median(gross_household, na.rm = TRUE), 
            redistSelf = median(redistSelf, na.rm = TRUE))

#try a logistic model
logit_region <- glmer(data = dat, 
                      family = binomial(link = "logit"), 
                      vote_con ~ edlevel + age + gender + 
                        gross_household + redistSelf + PopWhiteConst + 
                        ConstPercentChangeFiveYr + deprivation2019 + 
                        (1 + edlevel | region))

#get median percent change in house prices by region 
#get median popwhite by region 

#create new dat for predicting - we hold a bunch at median here 
expand.grid(edlevel = unique(dat$edlevel),
            region =  unique(dat$region),
            #is_white = 1, #white  
            age = 40, #this is median age in Britain
            gender = 1) %>% 
  as_tibble() %>% na.omit() %>% 
  left_join(., medians, by = "region") %>% 
  mutate(predicted =  predict(logit_region, newdata =.,
                              type = "response", allow.new.levels = TRUE)) %>% 
  ggplot(.) + 
  geom_line(aes(x = edlevel, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()

ggsave(educ_plot, filename = "educ_plot.jpeg")

#do this again with income 

#try a logistic model
logit_region <- glmer(data = dat, 
                      family = binomial(link = "logit"), 
                      vote_con ~ edlevel + age + gender + 
                        gross_household  + PopWhiteConst + 
                        ConstPercentChangeFiveYr + deprivation2019 + 
                        (1 + gross_household | region))

medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE),
            deprivation2019 = median(deprivation2019, na.rm = TRUE))
# gross_household = median(gross_household, na.rm = TRUE), 
# redistSelf = median(redistSelf, na.rm = TRUE))

#get median percent change in house prices by region 
#get median popwhite by region 

#create new dat for predicting - we hold a bunch at median here 
expand.grid(edlevel = 3, #A levels but no degree
            region =  unique(dat$region),
            #is_white = 1, #white  
            age = 40, #this is median age in Britain
            gender = 1,
            gross_household = unique(dat$gross_household)) %>% 
  as_tibble() %>% na.omit() %>% 
  left_join(., medians, by = "region") %>% 
  mutate(predicted =  predict(logit_region, newdata =.,
                              type = "response", allow.new.levels = TRUE)) %>% 
  ggplot(.) + 
  geom_line(aes(x = gross_household, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Household income level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()

stargazer(logit_region, type = "text")

###########################################################
#MULTINOMIAL MODELS ON MULTINOMIAL OUTCOME

linear_region <- lmer(data = dat, 
                      vote ~ edlevel + age + gender + 
                        gross_household  + PopWhiteConst + 
                        ConstPercentChangeFiveYr + deprivation2019 + 
                        (1 + gross_household | region))

medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE),
            deprivation2019 = median(deprivation2019, na.rm = TRUE))

expand.grid(edlevel = 3, #A levels but no degree
            region =  unique(dat$region),
            #is_white = 1, #white  
            age = 40, #this is median age in Britain
            gender = 1,
            gross_household = unique(dat$gross_household)) %>% 
  as_tibble() %>% na.omit() %>% 
  left_join(., medians, by = "region") %>% 
  mutate(predicted =  predict(logit_region, newdata =.,
                              type = "response", allow.new.levels = TRUE)) %>% 
  ggplot(.) + 
  geom_line(aes(x = gross_household, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()


dat$vote <- as.factor(dat$vote)

touse <- dat %>% 
  dplyr::select(vote, edlevel, gross_personal, is_white, age, gender, 
                deprivation2019, ConstPercentChangeFiveYr, PopWhiteConst, region, lad_name, PCON19NM) %>% 
  filter(!is.na(vote)) %>% 
  mutate(vote = as.factor(vote))

#run models - this takes 6.5 hours - so BEWARE BEFORE RUNNING 
brms_edlevel <- brm(
  formula = as.integer(vote) ~ edlevel + gross_personal + 
    is_white + age + gender + deprivation2019 + ConstPercentChangeFiveYr + 
    PopWhiteConst + (1 + edlevel | region), 
  data = touse, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 3000
)
save(brms_edlevel, file = "bes/internet_panel/models/brms_edlevel_region_nointeract.rda")

brms_edlevel_pcon <- brm(
  formula = as.integer(vote) ~ edlevel + gross_personal + 
    is_white + age + gender + deprivation2019 + ConstPercentChangeFiveYr + 
    PopWhiteConst + 
    (1 + edlevel | PCON19NM), 
  data = touse, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 3000
)

save(brms_edlevel_pcon, file = "bes/internet_panel/models/brms_edlevel_pcon_nointeract.rda")


brms_income <- brm(
  formula = vote ~ edlevel + gross_personal + 
    is_white + age + gender + deprivation2019 + ConstPercentChangeFiveYr + 
    PopWhiteConst + 
    (1 + gross_personal | region), 
  data = dat, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 3000
)
save(brms_fit, file = "C:/Users/dapon/Dropbox/Harvard/G3/stat151/final_project/data/brms_fit_big.rda")


brms_age_region <- brm(
  formula = as.integer(vote) ~ edlevel + gross_personal + 
    is_white + age + gender + deprivation2019 + ConstPercentChangeFiveYr + 
    PopWhiteConst + (1 + age | region), 
  data = touse, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 3000
)

save(brms_age_region, file = "bes/internet_panel/models/brms_age_region_nointeract.rda")


brms_age_pcon <- brm(
  formula = as.integer(vote) ~ edlevel + gross_personal + 
    is_white + age + gender + deprivation2019 + ConstPercentChangeFiveYr + 
    PopWhiteConst + (1 + age | PCON19NM), 
  data = touse, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 3000
)

save(brms_age_pcon, file = "bes/internet_panel/models/brms_age_pcon_nointeract.rda")


###########################
##
load("bes/internet_panel/models/brms_edlevel_region_nointeract.rda")

#get regional medians of constituency-level terms in the model 
medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE),
            deprivation2019 = median(deprivation2019, na.rm = TRUE), 
            gross_personal = median(gross_personal, na.rm = TRUE))


newdat <- expand.grid(edlevel = unique(dat$edlevel), #A levels but no degree
            region =  unique(dat$region),
            is_white = 1, #white  
            age = 60, #this is median age in Britain
            gender = 1) %>% 
  as_tibble() %>% na.omit() %>% 
  left_join(., medians, by = "region")

probs <- predict(brms_edlevel, newdata = newdat, type = "response")
#rearrange data for graphing - this is a bit roundabout
ndatBrexit <- newdat
ndatTory <- newdat
ndatCenter <- newdat
ndatLabour <- newdat
ndatLeft <- newdat
probs <- probs %>%
  as_tibble() %>%
  rename(p1 = `P(Y = 1)`,
         p2 = `P(Y = 2)`,
         p3 = `P(Y = 3)`,
         p4 = `P(Y = 4)`,
         p5 = `P(Y = 5)`)
ndatBrexit$prob <- probs$p1
ndatBrexit$Party <- "Brexit"
ndatTory$prob <- probs$p2
ndatTory$Party <- "Tory"
ndatCenter$prob <- probs$p3
ndatCenter$Party <- "Center"
ndatLabour$prob <- probs$p4
ndatLabour$Party <- "Labour"
ndatLeft$prob <- probs$p5
ndatLeft$Party <- "Left"

#rbind together
test <- rbind(ndatBrexit, ndatTory, ndatCenter, ndatLabour, ndatLeft)

test %>% 
  filter(Party == "Tory") %>% 
  ggplot(.) + 
  geom_line(aes(x = edlevel, y = prob, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability") + 
  theme_minimal()



