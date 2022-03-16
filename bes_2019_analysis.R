library(tidyverse)
library(haven)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")

bes <- read_dta("bes/election_studies/2019BES.dta")

bes[bes < 0] <- NA

bes <- bes %>% 
  dplyr::select(finalserialno, region, Constit_Code, Constit_Name, 
                Age, edlevel, y09, starts_with("Y01"),
                starts_with("y01"), b02, u05, starts_with("LA")) %>% 
  rename(gender = y09, vote = b02, vote17 = u05) %>% 
  mutate(gender = as.numeric(gender), 
    vote_con = case_when(
    vote == 2 ~ 1, 
    TRUE ~ 0
  ), female = case_when(
    gender == 2 ~ 1, 
    gender == 1 ~ 0, 
    TRUE ~ gender
  ), female = ifelse(gender %in% c(3,4), NA, female)) %>% 
  rename(income_annual = y01_Annual)

#read in geography data
geo <- read_csv("bes/election_studies/bes_2019_facetoface_geography.csv")
geo <- geo %>% 
  rename(Region = region, 
         constit_code = Constit_Code, 
         constit_name = Constit_Name,
         oslaua_code = LA_UA_Code, 
         oslaua_name = LA_UA_Name)

dat <- left_join(bes, geo, by = "finalserialno") %>% 
  filter(!(Region %in% c("Scotland","Wales")))

logit_edlevel_region <- glmer(data = dat, 
                             family = binomial(link = "logit"), 
                             vote_con ~ Age + edlevel + female + income_annual + 
                               (1 + edlevel | Region))

ndat_educ <- expand.grid(edlevel = unique(dat$edlevel),
                         Region =  unique(dat$Region),
                         #is_white = 1, #white  
                         Age = 40, #this is median age in Britain
                         female = 0, 
                         income_annual = median(dat$income_annual, na.rm = T))
#omit NAs
ndat_educ <- na.omit(ndat_educ)
#predict
probs <- predict(logit_edlevel_region, newdata = ndat_educ, type = "response")
ndat_educ$predicted <- probs


#change variable type for graphing
ndat_educ$White <- as.factor(ndat_educ$is_white)

#create plot 
educ_plot <- ggplot(ndat_educ) + 
  geom_line(aes(x = edlevel, y = predicted, color = Region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()


logit_income_region <- glmer(data = dat, 
                              family = binomial(link = "logit"), 
                              vote_con ~ Age + edlevel + female + income_annual + 
                                (1 + income_annual | Region))

ndat_income <- expand.grid(edlevel = 3,
                         Region =  unique(dat$Region),
                         #is_white = 1, #white  
                         Age = 40, #this is median age in Britain
                         female = 0, 
                         income_annual = unique(dat$income_annual, na.rm = T))
#omit NAs
ndat_income <- na.omit(ndat_income)
#predict
probs <- predict(logit_income_region, newdata = ndat_income, type = "response")
ndat_income$predicted <- probs

#create plot 
income_plot <- ggplot(ndat_income) + 
  geom_line(aes(x = income_annual, y = predicted, linetype = Region, color = Region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Income level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()
