
library(tidyverse)
library(haven)
library(estimatr)
library(stargazer)
#read in data 
bes <- read_dta("bes/internet_panel/bes_wave19_2019post.dta")
lookup <- read_csv("uk_geography/pcon_data/pcon_region_lookup_2019.csv")
#read in pcon to region lookup

dat <- bes
dat[dat < 0] <- NA
dat[dat == 9999] <- NA
dat <- dat %>% 
  mutate(lad_name = labelled::to_factor(dat$oslaua),
         constit = labelled::to_factor(dat$pcon),
         PCON19NM = constit) %>% 
  left_join(., lookup, by = "PCON19NM") %>% 
  dplyr::select(id, lad_name, constit, oslaua, gor, pcon, starts_with("PCON"), region, 
                jobzone, generalElectionVote, partyId, starts_with("p_"),
                starts_with("satDem"), leftRight, howLongLivedHere,
                age, gender, redistSelf, workingStatus, econPersonalProsp,
                starts_with("ukAttach"),
                starts_with("subnat")) %>% 
  mutate(vote_character = labelled::to_factor(dat$generalElectionVote)) %>% 
  filter(!(generalElectionVote %in% c(0, 9, 13, 9999))) %>% 
  mutate(vote_con = case_when(
    vote_character %in% c("Conservative", "Brexit Party","United Kingdom Independence Party (UKIP)") ~ 1,
    TRUE ~ 0
  ), vote = case_when(
    vote_character %in% c("Brexit Party","United Kingdom Independence Party (UKIP)",
                          "British National Party (BNP)") ~ 1, 
    vote_character %in% c("Conservative") ~ 2, 
    vote_character %in% c("Liberal Democrat", "Change UK- The Independent Group") ~ 3, 
    vote_character %in% c("Labour") ~ 4, 
    vote_character %in% c("Green Party","Plaid Cymru") ~ 5
  )) 

dat$age <- ifelse(dat$age < 0 | dat$age > 100, NA, dat$age)
dat$leftRight <- ifelse(dat$leftRight > 10, NA, dat$leftRight)
dat$p_gross_household <- ifelse(dat$p_gross_household > 15, NA, dat$p_gross_household)
dat$p_gross_personal <- ifelse(dat$p_gross_personal > 14, NA, dat$p_gross_personal)
dat$p_education <- ifelse(dat$p_education > 18, NA, dat$p_education)
dat$econPersonalProsp <- ifelse(dat$econPersonalProsp > 5, NA, dat$econPersonalProsp)
dat$redistSelf <- ifelse(dat$redistSelf > 10, NA, dat$redistSelf)
dat$p_ethnicity <- ifelse(dat$p_ethnicity > 14, NA, dat$p_ethnicity)
dat <- dat %>% filter(!is.na(p_ethnicity))
dat$is_white <- ifelse(dat$p_ethnicity == 1, 1, 0 )
dat$has_degree <- ifelse(dat$p_edlevel > 3, 1, 0)
dat$socgrade <- ifelse(dat$socgrade %in% c(7, 8), NA, dat$socgrade)

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

#get rid of p_ prefixes
names <- str_remove(names(dat), "p_")
names(dat) <- names

# reverse scale of attachment variables 4-6, to match directionality of 1-3
dat <- dat %>% 
  # reverse scale of 4-6 variables
  mutate(ukAttach4 = max(ukAttach4, na.rm = TRUE) - ukAttach4,
         ukAttach5 = max(ukAttach5, na.rm = TRUE) - ukAttach5,
         ukAttach6 = max(ukAttach6, na.rm = TRUE) - ukAttach6)

# do factor analysis of the 
attach <- dat %>% 
  select(id, starts_with("ukAttach")) %>% 
  filter(!is.na(ukAttach1) & 
           !is.na(ukAttach2) &
           !is.na(ukAttach3) &
           !is.na(ukAttach4) &
           !is.na(ukAttach5) &
           !is.na(ukAttach6))

# do the pca analysis 
fit <- princomp(attach[,2:7], cor=TRUE)
# plot pca - looks like they map to one component 
plot(fit, type = "lines")
# extract the first component 
# multiply by -1 so higher scores = more attached to UK
attach$uk_attach_score <- fit$scores[,1]*-1
# join first component back with the data
dat <- left_join(dat, attach %>% 
                   select(id, uk_attach_score), by = "id")


# suggests that people with higher UK attachment favor lower redistribution 
mod1 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
             ukAttach1 + 
             edlevel + is_white + socgrade + gross_household + leftRight + 
             as.factor(partyId)))
mod2 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
              ukAttach2 + 
              edlevel + is_white + socgrade + gross_household + leftRight + 
              as.factor(partyId)))
mod3 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
              ukAttach3 + 
              edlevel + is_white + socgrade + gross_household + leftRight + 
              as.factor(partyId)))
mod4 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
              ukAttach4 + 
              edlevel + is_white + socgrade + gross_household + leftRight + 
              as.factor(partyId)))
mod5 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
              ukAttach5 + 
              edlevel + is_white + socgrade + gross_household + leftRight + 
              as.factor(partyId)))
mod6 <- (lm(data = dat,# %>% filter(!(region %in% c("Scotland","Wales"))),
            redistSelf ~ 
              ukAttach6 + 
              edlevel + is_white + socgrade + gross_household + leftRight + 
              as.factor(partyId)))

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text")

lm_robust(data = dat, 
          redistSelf ~ uk_attach_score + edlevel + is_white + socgrade + 
            gross_household + gender)





