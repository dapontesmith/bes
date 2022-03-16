library(sf)
library(modelsummary)
library(texreg)
library(lfe)
library(lmerTest)
library(tidyverse)
library(haven)
library(estimatr)
library(sjlabelled)
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

df <- read_csv("data/bes/internet_panel/clean_data/bes_wave4_clean.csv")


#run a bunch of individual-level models
mod1 <- felm(data = df,
             formula = localismImportance ~ age + male + p_edlevel + p_gross_household + 
               vote_spectrum | pcon| 0 | pcon)

mod2 <- felm(data = df, 
             localismImportance ~  redistSelf + econGenRetro + 
               econPersonalRetro + cutsTooFarLocal | pcon | 0 | pcon )
screenreg(list(mod1, mod2))

#run a bunch of pcon-level models
mod2 <- felm(data = df, 
             localismImportance ~ 
               (population) + (population_density)  + 
               (rate2015) + (gross_disp_income2015) + 
               (unem_change_5yr) + (gross_income_change_5yr) | 
               pcon )
mod3 <- lmer(data = df, 
             localismImportance ~ 
               scale(population) + scale(population_density)  + 
               scale(gross_disp_income2015) + 
               scale(unem_change_10yr) + scale(gross_income_change_10yr)*scale(rate2015))
screenreg(list(mod2, mod3))


#run some cross-level models 
mod1_cross <- lmer(data = df, localismImportance ~ 
                     p_gross_household + male + p_edlevel +
                     scale(gross_disp_income2015) + vote_spectrum*rate2015 + 
                     (1 | region))
screenreg(mod1_cross)



#On all dependent variables, higher values = more localist
mod1 <- felm(data = modeldat, 
             localismImportance ~ age + male + p_edlevel + p_gross_household + 
               econGenRetro + vote_con + redistSelf + econPersonalRetro | pcon| 0 | pcon)
mod2 <- lmer(data = modeldat, 
             localismImportance ~ age + male + p_edlevel + p_gross_household + 
               econGenRetro + vote_con + redistSelf + econPersonalRetro + 
               (1 |region_name))

mod3 <- lmer(data = modeldat, 
             localismImportance ~ population_density + ethnicity_white + borough + 
               nsseclongterm_unemployed + (1 + population_density | pcon))
screenreg(list(mod1))


localism <- df %>% 
  group_by(pcon) %>% 
  summarise(localism = mean(localismImportance, na.rm = TRUE))