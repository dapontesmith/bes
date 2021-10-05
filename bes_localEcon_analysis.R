#Not sure I need all these packages but here we go! 
library(tidyverse)
library(lmerTest)
library(broom)
library(sjstats)
library(arm)
library(stargazer)
library(ordinal)
library(effects)
library(ggeffects)
library(brms)
library(sjPlot)
library(expss)
library(xtable)
library(parallel)
library(haven)
library(patchwork)
library(modelsummary)
library(survival)
rm(list = ls())
options(mc.cores=parallel::detectCores())


setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

raw11 <- read_csv("data/bes/internet_panel/clean_data/wave11_clean.csv") 
#transform the vote variables so higher = further right (easier to intrpret for some reason)
raw15 <- read_csv("data/bes/internet_panel/clean_data/wave15_clean.csv")
raw11 <- raw11 %>% 
  mutate(voteSpectrum = case_when(
    vote_spectrum == 1 ~ 5, 
    vote_spectrum == 2 ~ 4, 
    vote_spectrum == 4 ~ 2, 
    vote_spectrum == 5 ~ 1, 
    TRUE ~ vote_spectrum
  )) %>%
  rename(region = region.x)
raw15 <- raw15 %>% 
  mutate(voteSpectrum = case_when(
    voteSpectrum == 1 ~ 5, 
    voteSpectrum == 2 ~ 4, 
    voteSpectrum == 4 ~ 2, 
    voteSpectrum == 5 ~ 1, 
    TRUE ~ voteSpectrum
  ), own_house = case_when(
    p_housing %in% c(1, 2, 3) ~ 1, 
    TRUE ~ 0
  )) 
#raw11$median <- as.numeric(str_remove(raw11$median, ","))
raw11$median_income_scale <- scale(raw11$median_income)
raw11$price_change20yr_scale <- scale(raw11$price_change20yr)
#Make data sets for England and Wales alone
dat15 <- raw15 %>% 
  filter(country.y %in% c("England","Wales"))
dat11 <- raw11 %>% filter(country.y %in% c("England","Wales"))
eng11 <- raw11 %>% filter(country.y == "England") %>% 
  filter(region != "Wales")



########################################################################
#########################################################################
## LOCALISM AS DEPENDENT VARIABLE
#####################################################################
#####################################################################

mod1_belong_demo <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                      p_edlevel + age + male + p_socgrade + white_british + own_house + 
                      (1 | pcon))

mod1_belong_region <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                      p_edlevel + age + male + p_socgrade + white_british + 
                        as.factor(region)  + (1 | pcon))

mod1_belong_party <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                             p_edlevel + age + male + p_socgrade + white_british + 
                             as.factor(partyIdName) + (1 | pcon))

mod1_belong_remain <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                             p_edlevel + age + male + p_socgrade + white_british + 
                             remainID + (1 | pcon))

mod1_belong_policy <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                             p_edlevel + age + male + p_socgrade + white_british + 
                             houseBuild + cutsTooFarLocal + (1 | pcon))

mod1_belong_context <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel + 
               age + male + p_socgrade + white_british + (1 | pcon) + 
               median_income_scale + scale(price_change5yr) + rate2016 + 
                 population_density + constituency_type.x + ethnicity_white_british)




#####################################

#graph the context results 
context_plot <- cbind(names(fixef(mod1_belong_context)),
                      summary(mod1_belong_context)$coef[,1],
                      summary(mod1_belong_context)$coef[,2]) %>% 
  as_tibble() %>% 
  rename(var = V1, est = V2, sd = V3) %>% 
  mutate(var = str_remove(var, "\\("), var = str_remove(var, "\\)")) %>%
  filter(var %in% c("median_income_scale","scaleprice_change5yr",
                    "rate2016","population_density","constituency_type.xCounty",
                    "ethnicity_white_british")) %>% 
  mutate(var_name = case_when(
    var == "median_income_scale" ~ "Median income",
    var == "scaleprice_change5yr" ~ "5-yr pct. chg. house prices",
    var == "rate2016" ~ "Unemployment rate (2016)",
    var == "population_density" ~ "Density", 
    var == "constituency_type.xCounty" ~ "County constituency",
    var == "ethnicity_white_british" ~ "Pct. White British"
  ), est = as.numeric(est), sd = as.numeric(sd)) %>% 
  mutate(conf.low = est - 1.96*sd,
         conf.high = est + 1.96*sd) %>% 
  ggplot() + 
  geom_pointrange(aes(x = var_name, y = est, ymin = conf.low , ymax = conf.high)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  theme_minimal() + 
  labs(x = "Variable", y = "Estimate",
       title = "Localism and elements of local context",
       caption = "Multilevel linear models, random effects at constituency level.
       Models include but do not report demographic covariates.
       Median income and house-price variables are scaled to have mean of 0 and s.d. of 1.
       Baseline for the country constituency variable is borough.")
ggsave("drafts/paper1/figures/context_plot.pdf", context_plot)



#graph the region results 
region_plot <- cbind(names(fixef(mod1_belong_region)), 
      summary(mod1_belong_region)$coef[,1], 
      summary(mod1_belong_region)$coef[,2]) %>%
  as_tibble() %>% 
  rename(var = V1, est = V2, sd = V3) %>%
  filter(str_detect(var, "as.factor") == TRUE) %>%
  mutate(var = str_remove(var, "as.factor"),
         var = str_remove(var, "region_name"),
         var = str_remove(var, "\\(\\)"),
         est = as.numeric(est), sd = as.numeric(sd)) %>%
  mutate(conf.low = est - 1.96*sd,
         conf.high = est + 1.96*sd) %>% 
  ggplot() + 
  geom_pointrange(aes(x = var, y = est, ymin = conf.low, ymax = conf.high)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  theme_minimal() + 
  labs(x = "Region", y = "Estimate")




#graph the party results
party_plot <- cbind(names(fixef(mod1_belong_party)), 
      summary(mod1_belong_party)$coef[,1], 
      summary(mod1_belong_party)$coef[,2]) %>%
  as_tibble() %>% 
  rename(var = V1, est = V2, sd = V3) %>%
  filter(str_detect(var, "as.factor") == TRUE) %>%
  mutate(var = str_remove(var, "as.factor"),
         var = str_remove(var, "partyIdName"),
         var = str_remove(var, "\\(\\)"),
         est = as.numeric(est), sd = as.numeric(sd)) %>%
  mutate(conf.low = est - 1.96*sd,
         conf.high = est + 1.96*sd) %>% 
  ggplot() + 
  geom_pointrange(aes(x = var, y = est, ymin = conf.low, ymax = conf.high)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  theme_minimal() + 
  labs(x = "Party", y = "Estimate")

region_party_plot <- region_plot + party_plot + plot_annotation(
  title = "Regional and partisan correlates of local belonging",
  caption = "Results from regressions with fixed effects by region and party identification respectively.
  Both regressions include demographic covariates. The baseline region is the 
  East of England; the baseline party is Conservative. Each point represents the estimated 
  propensity of members of the region or party to identify with the local community, above the baseline.
  Errors bars represent 95 percent confidence intervals."
)
ggsave("drafts/paper1/figures/region_party_plot.pdf", region_party_plot)


mod_effic1 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel + 
                      age + male + white_british + p_socgrade + as.factor(partyIdName) + 
                      efficacyPolCare + (1 | pcon))
mod_effic2 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel + 
                      age + male + white_british + p_socgrade + as.factor(partyIdName)+ 
                      efficacyNoMatter + (1 | pcon))
mod_effic3 <- lmer(data = raw11, belongLocal ~ p_gross_household + 
                     p_edlevel + p_socgrade + white_british + as.factor(partyIdName) + 
                     efficacyVoteEffort + (1 | pcon))
mod_effic4 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel + 
                      age + male + white_british + p_socgrade + as.factor(partyIdName) + 
                      satDemUK + (1 | pcon))
class(mod_effic1) <- "lmerMod" ; class(mod_effic2) <- "lmerMod"
class(mod_effic3) <- "lmerMod" ; class(mod_effic4) <- "lmerMod"

stargazer(mod_effic1, mod_effic2, mod_effic3, mod_effic4, type = "latex",
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant", 
                   "as.factor\\(partyIdName\\)Green", "as.factor\\(partyIdName\\)Labour",
                   "as.factor\\(partyIdName\\)Lib Dem", "as.factor\\(partyIdName\\)None",
                   "as.factor\\(partyIdName\\)Other","as.factor\\(partyIdName\\)PC",
                   "as.factor\\(partyIdName\\)UKIP","as.factor\\(partyIdName\\)SNP"),
          dep.var.caption = "Local belonging (0/1)",
          dep.var.labels.include = FALSE,
          label = "tab:effic_mods",
         covariate.labels = c("Voting is a lot of effort",
                               "Politicians don't care about people like me",
                              "Doesn't matter which party is in power",
                              "Satifaction with UK democracy"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
          model.numbers = TRUE, column.sep.width = "3pt")


############################################################################
#### COSMOPOLITANISM / FEELING THERMOMETER ANALYSES 
###################################################################3

#same as above, including variables for attitudes towards britishness and towards immigrants



mod_warm_syrian <- lmer(data = raw11, warmSyrians ~ p_gross_household + p_edlevel + 
                     age + male + white_british + p_socgrade + 
                     belongLocal + ethnicity_white_british + rate2017 + (1 | pcon))
mod_warm_indian <- lmer(data = raw11, warmIndian ~ p_gross_household + p_edlevel + 
                          age + male + white_british + p_socgrade + 
                          belongLocal +  ethnicity_white_british + rate2017 + (1 | pcon))
mod_warm_eastern <- lmer(data = raw11, warmEastern ~ p_gross_household + p_edlevel + 
                          age + male + white_british + p_socgrade + 
                          belongLocal +  ethnicity_white_british + rate2017 + (1 | pcon))
mod_customs <- lmer(data = raw11, britCustoms ~ p_gross_household + 
                      p_edlevel + age + male + white_british + p_socgrade + 
                      belongLocal + ethnicity_white_british + rate2017 +  (1 | pcon))
mod_christian <- lmer(data = raw11, britChristian ~ p_gross_household + 
                        p_edlevel + age + male + white_british + p_socgrade + 
                        belongLocal +  ethnicity_white_british + rate2017 + (1 | pcon))





#plot the customs results and chrstian results
names <- c("intercept","p_gross_household","p_edlevel",
           "age","male","white_british","p_socgrade","belongLocal")
rbind(cbind(summary(mod_customs)$coef[,1],
      summary(mod_customs)$coef[,2], names),
      cbind(summary(mod_christian)$coef[,1],
      summary(mod_christian)$coef[,2], names)) %>% 
  as_tibble() %>%
  filter(names != "intercept") %>% 
  mutate(variable = case_when(
    names == "p_gross_household" ~ "Income", 
    names == "p_edlevel" ~ "Education", 
    names == "age" ~ "Age",
    names == "male" ~ "Male", 
    names == "white_british" ~ "White British",
    names == "p_socgrade" ~ "Social grade",
    names == "belongLocal" ~ "Local belonging"
  ),
  model = c(rep("Customs model", 7), rep("Christianity model", 7))) %>%
  rename(coef = V1, sd = V2) %>% 
  mutate(coef = as.numeric(coef),
         sd =as.numeric(sd)) %>% 
  mutate(upper = coef + 1.96*sd, 
         lower = coef - 1.96*sd) %>% 

  ggplot() + 
  geom_pointrange(aes(x = variable, y = coef, ymin = lower, ymax = upper)) + 
  facet_wrap(~ model) + 
  theme_minimal()  + 
  coord_flip() + 
  labs(x = "Attribute", y = "Estimate") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
 # rename(customs_coef = V1, customs_sd = V2, 
  #       christian_coef = V3, christian_sd = V4) %>%
 # filter(names == "belongLocal") 


class(mod_warm_syrian) <- "lmerMod"
class(mod_warm_indian) <- "lmerMod"
class(mod_warm_eastern) <- "lmerMod"
class(mod_customs) <- "lmerMod"
class(mod_christian) <- "lmerMod"

stargazer(mod_warm_syrian, mod_warm_indian, mod_warm_eastern, mod_customs, mod_christian,
          type = "latex", 
          keep = c("belongLocal","ethnicity_white_british","rate2017"),
          dep.var.labels.include = FALSE,
          covariate.labels = c("Local belonging", "% White British", "Unemployment Rate"),
          column.labels = c("Syrians", "Indians","East Europeans","Customs","Christianity"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = "tab:mod_cosmopolitan",
          header = FALSE)




mod_immig <- lmer(data = raw11, immigSelf ~ p_gross_household + p_edlevel + 
                    age + male + white_british + p_socgrade + 
                    belongLocal + as.factor(partyIdName) + 
                    (1 | pcon))
class(mod_immig) <- "lmerMod"

stargazer(mod_immig,
          type = "latex", 
          keep = "belongLocal",
          #dep.var.labels.include = FALSE,
          dep.var.labels = "Immigration preferences (high-low)",
          covariate.labels = "Local belonging",
         # column.labels = c("Syrians", "Indians","East Europeans","Customs","Christianity"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          label = "tab:mod_immig_cosmo",
          header = FALSE)


mod_immig_cosmo_plot <- 
  plot(ggpredict(mod_immig, terms = c("partyIdName","belongLocal")), 
       colors = "bw",
       ci.style = "dash",
       show.title = FALSE) +
  ggtitle("Predicted immigration preferences, by party and local belonging") + 
  labs(x = "Party", 
       y = "Immigration preferences (higher values = fewer immigrants)",
       shape = "Local belonging") + 
  theme(title = element_blank())

ggsave("drafts/paper1/figures/mod_immig_cosmo_plot.pdf",
       mod_immig_cosmo_plot)

####basic local Econ analyses, plus interaction with belongLoacl
mod1 <- lmer(data = raw11, vote_con ~ localEcon + p_gross_household + 
               p_edlevel + age + male + p_socgrade + white_british +(1 | pcon))
mod2 <- lmer(data = raw15, voteCon ~ localEcon +p_gross_household +
               p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon))
mod3 <- lmer(data = raw11, vote_con ~ localEcon + p_gross_household +
               econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon))
mod4 <- lmer(data = raw15, voteCon ~ localEcon + p_gross_household + econGenRetro + econPersonalRetro + 
             p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon))
mod5 <- lmer(data = raw11, vote_con ~ localEcon + belongLocal + econGenRetro + 
               econPersonalRetro + p_edlevel + age + male + p_socgrade + p_gross_household + 
               white_british + (1 | pcon))
mod6 <- lmer(data = raw11, vote_con ~ localEcon*belongLocal + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade + p_gross_household + white_british +  (1 | pcon))




class(mod1) <- "lmerMod" ; class(mod2) <- "lmerMod" ; class(mod3) <- "lmerMod" 
class(mod4) <- "lmerMod" ; class(mod5) <- "lmerMod" ; class(mod6) <- "lmerMod"



stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "latex",
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant"),
          dep.var.caption = "Vote Conservative (0/1)",
          dep.var.labels.include = FALSE,
          label = "tab:vote_mods",
         covariate.labels = c("Local econ","Local belong",
                             "General econ","Personal econ",
                             "Local econ * belong"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
         font.size = "small",
          model.numbers = TRUE, column.sep.width = "3pt",
          column.labels = c("2017","2019","2017","2019",
                            "2017","2017"))


#Table on asset-based localism and vote choice
mod_home_2017_1 <- lmer(data = raw11, vote_con ~ own_house + econGenRetro + econPersonalRetro + 
                        p_edlevel + age + male + p_socgrade + p_gross_household + white_british +  (1 | pcon))
mod_home_2017_2 <- lmer(data = raw11, vote_con ~ localEcon*own_house + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade + p_gross_household + white_british +  (1 | pcon))
mod_home_2019_1 <- lmer(data = raw15, voteCon ~ own_house + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade + p_gross_household + white_british +  (1 | pcon))
mod_home_2019_2 <- lmer(data = raw15, voteCon ~ localEcon*own_house + econGenRetro + econPersonalRetro + 
                        p_edlevel + age + male + p_socgrade + p_gross_household + white_british +  (1 | pcon))
class(mod_home_2017_1) <- "lmerMod" ; class(mod_home_2019_1) <- "lmerMod"
class(mod_home_2017_2) <- "lmerMod" ; class(mod_home_2019_2) <- "lmerMod"

stargazer(mod_home_2017_2, mod_home_2019_2,type = "text",
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant"),
          dep.var.caption = "Vote Conservative (0/1)",
          dep.var.labels.include = FALSE,
          label = "tab:house_mods",
          #covariate.labels = c("Local econ","Own house",
           #                    "General econ","Personal econ",
            #                   "Local econ * own house"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
          model.numbers = TRUE, column.sep.width = "3pt",
          column.labels = c("2017","2019"))

plot_mod_home_2017 <- plot(ggpredict(mod_home_2017, terms = c("localEcon","own_house", "
                                               econGenRetro [3]","white_british [1]"))) + 
  labs(x = "Local economic evaluation (low - high)") + 
  theme(axis.title.y = element_blank(),
        title = element_blank()) + 
  guides(colour=guide_legend(title=str_wrap("Own house", 7)))





#LOGIT MODELS, CLUSTERED STANDARD ERRORS AT PCON
mod1glm <- glm(data = raw11, voteConTwoParty ~ localEcon + p_gross_household + 
               p_edlevel + age + male + p_socgrade + white_british,
             family = binomial(link = "logit"))
mod2glm <- glm(data = raw15, voteConTwoParty ~ localEcon +p_gross_household +
               p_edlevel + age + male + p_socgrade +  white_british,
               family = binomial(link = "logit"))
mod3glm <- glm(data = raw11, voteConTwoParty ~ localEcon + p_gross_household +
               econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade +  white_british,
               family = binomial(link = "logit"))
mod4glm <- glm(data = raw15, voteConTwoParty ~ localEcon + p_gross_household + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade +  white_british,
               family = binomial(link = "logit"))
mod5glm <- glm(data = raw11, voteConTwoParty ~ localEcon*belongLocal + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade + p_gross_household + white_british,
               family = binomial(link = "logit"))
clust1 <- sandwich::vcovCL(mod1glm, raw11$pcon)
mod1glm <- coeftest( mod1glm, vcov. = clust1 )
clust2 <- sandwich::vcovCL(mod2glm, raw15$pcon)
mod2glm <- coeftest( mod2glm, vcov. = clust2 )
clust3 <- sandwich::vcovCL(mod3glm, raw11$pcon)
mod3glm <- coeftest( mod3glm, vcov. = clust3 )
clust4 <- sandwich::vcovCL(mod4glm, raw15$pcon)
mod4glm <- coeftest( mod4glm, vcov. = clust4 )
clust5 <- sandwich::vcovCL(mod5glm, raw11$pcon)
mod5glm <- coeftest( mod5glm, vcov. = clust5 )



#stargaze the linear random-intercept models
stargazer(mod1, mod2, mod3, mod4, mod5, type = "latex", 
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant"),
          dep.var.caption = "Vote Conservative (0/1)",
          dep.var.labels.include = FALSE,
          label = "tab:vote_mods",
          column.labels = c("2017","2019","2017","2019","2017"),
          covariate.labels = c("Local econ","Local belonging",
                               "General econ","Personal econ","Local econ * belong"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
          model.numbers = TRUE, column.sep.width = "3pt")






clogit1 <- clogit(data = raw11, vote_con ~ localEcon + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
clogit2 <- clogit(data = raw15, vote_con ~ localEcon + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
clogit3 <- clogit(data = raw11, vote_con ~ localEcon + econGenRetro + econPersonalRetro + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
clogit4 <- clogit(data = raw15, vote_con ~ localEcon + econGenRetro + econPersonalRetro + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
clogit5 <- clogit(data = raw11, vote_con ~ localEcon*belongLocal + econGenRetro + econPersonalRetro + 
                 p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                 strata(pcon))
clogit6 <- clogit(data = raw11, vote_con ~ localEcon + belongLocal + econGenRetro + econPersonalRetro + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
stargazer(clogit1, clogit3, clogit6, clogit5, type = "text", 
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant"),
          dep.var.caption = "Vote Labour/Con (0/1)",
          dep.var.labels.include = FALSE,
          column.labels = c("2017","2019","2017","2019","2017","2017"),
          covariate.labels = c("Local econ","Local belonging",
                               "General econ","Personal econ","Local econ * belong"),
          no.space = TRUE, 
          label = "tab:vote_mods",
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
          model.numbers = TRUE, column.sep.width = "3pt",
          omit.stat = c("wald","lr","logrank"))

clogit6 <- clogit(data = raw11, voteConTwoParty ~ localEcon + own_house + 
                    econGenRetro + econPersonalRetro + 
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british + 
                    strata(pcon))
stargazer(clogit6, type = "text",
          omit = c("p_edlevel","age","male",'p_socgrade',"p_gross_household",
                   "white_british","Constant"))

#make predicted values plots from the models
plot_mod3 <- plot(ggpredict(mod3, terms = c("localEcon","econGenRetro [3]",
                                  "male [1]", "white_british [1]"))) + 
            labs(x = "Local economic evaluation (low - high)",
            y = "Predicted probability") + 
  theme(title = element_blank())
plot_mod5 <- plot(ggpredict(mod5, terms = c("localEcon","belongLocal", "
                                               econGenRetro [3]","white_british [1]"))) + 
  labs(x = "Local economic evaluation (low - high)") + 
  theme(axis.title.y = element_blank(),
        title = element_blank()) + 
  guides(colour=guide_legend(title=str_wrap("Local belonging", 7)))

plot_local_vote_logit <- plot_mod3 + plot_mod5 + 
  plot_annotation(
    title = "Predicted probability of Conservative vote",
    caption = "Probabilities predicted from random-effects linear regression. 
    Probabilities are for white British, male respondent with neutral general 
    economic evaluation and mean values of all other covariates."
    )
ggsave("drafts/paper1/figures/plot_local_vote.pdf")




plot_mod3_random <- plot(ggpredict(mod3, terms = c("localEcon","econGenRetro [3]", 
                               "male [1]","white_british [1]"))) + 
  labs(x = "Local economic evaluation (low-high)", 
       y = "Predicted probability") + 
  theme(title = element_blank())

plot_mod5_random <- plot(ggpredict(mod5, terms = c("localEcon", "belongLocal",
                                                   "econGenRetro [3]", "male [1]"))) + 
  labs(x = "Local economic evaluation (low-high)", 
       y = "Predicted probability") + 
  theme(axis.title.y = element_blank(), 
        title = element_blank()) + 
  guides(colour = guide_legend(title = str_wrap("Local belonging", 7)))
plot_local_vote_random <- plot_mod3_random + plot_mod5_random + 
  plot_annotation(
    title = "Predicted probability of Conservative vote",
    caption = "Probabilities predicted from linear regression with random intercepts
    at parliamentary constituency. Probabilities are for white British, male respondent with
    neutral general economic evaluation and mean values of all other covariates."
  )
ggsave("drafts/paper1/figures/plot_local_vote_random.pdf", 
       plot_local_vote_random)



#run logistic models with brms to resolve convergence issues
brms1 <- brm(
  formula = voteConTwoParty ~ localEcon + p_gross_household + 
    p_edlevel + age + male + p_socgrade + white_british + (1 | pcon),
  data = raw11, 
  family = bernoulli,
  cores = 4, 
  iter = 5000
)

brms2 <- brm(
  formula = voteConTwoParty ~ localEcon + p_gross_household + 
    p_edlevel + age + male + p_socgrade + white_british + (1 | pcon),
  data = raw15, 
  family = bernoulli,
  cores = 4, 
  iter = 5000
)

brms3 <- brm(
  formula = voteConTwoParty ~ localEcon + econGenRetro + econPersonalRetro + 
    p_gross_household + 
    p_edlevel + age + male + p_socgrade + white_british + (1 | pcon),
  data = raw11, 
  family = bernoulli,
  cores = 4, 
  iter = 5000
)

brms4 <- brm(
  formula = voteConTwoParty ~ localEcon + econGenRetro + econPersonalRetro + 
    p_gross_household + 
    p_edlevel + age + male + p_socgrade + white_british + (1 | pcon),
  data = raw15, 
  family = bernoulli,
  cores = 4, 
  iter = 5000
)

brms5 <- brm(
  formula = voteConTwoParty ~ localEcon*belong + econGenRetro + econPersonalRetro + 
    p_gross_household + p_edlevel + age + male + p_socgrade + white_british + (1 | pcon),
  data = raw11, 
  family = bernoulli,
  cores = 4, 
  iter = 5000
)



#Stargaze
stargazer(mod1, mod2, mod3, mod4, mod5, type  = "text",
          dep.var.caption = "Vote choice (1-5, right-left)", 
          dep.var.labels.include = FALSE,
          column.labels = c("2017","2019","2017","2019","2017"),
          no.space = TRUE, 
          omit = c("p_edlevel","age","male","p_socgrade","white_british", "p_gross_household"),
          covariate.labels = c("Local econ eval", "Local belonging",
                               "General econ retro", "Personal econ retro",
                               "Local econ * Local belonging"),
          title = "local_econ_mod",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("aic","bic"),
          header = FALSE,
          model.numbers = TRUE,
          column.sep.width = "3pt")

#############################
## Context models: Are voters responsive to objective local context?
#mod1_context: includes unemployment, annual change in median income, 20 year house price change
#mod2_context: adds general and personal retrospective evaluations
#unemployment and price change stay significant in both 
#############################
mod1_context <- lmer(data = raw11, voteConTwoParty ~ median_per_change_2017 + rate2017 + 
                       price_change20yr_scale +
               p_edlevel + age + male + p_socgrade + white_british + p_gross_household)
mod2_context <- glmer(data = raw11, voteConTwoParty ~ price_change20yr_scale + econGenRetro + 
                       econPersonalRetro + rate2017 + median_per_change_2017 + 
                       p_edlevel + age + male + p_socgrade + white_british + p_gross_household +
                       (1 | pcon), family = binomial(link = "logit"))

class(mod1_context) <- "lmerMod" ; class(mod2_context) <- "lmerMod"
stargazer(mod1_context, mod2_context, type = "text",
          omit = c("p_edlevel","age","male","p_socgrade",
                   "white_british","p_gross_household"),
          star.cutoffs = c(0.05, 0.01,0.001),
          covariate.labels = c("Pct. annual change median income",
                               "Unemployment rate",
                               "House price 20-year pct. change",
                               "General econ eval",
                               "Personal econ eval"),
          omit.stat = c('aic',"bic"), 
          dep.var.labels = "Vote choice (1-5, left-right)")



########################33
#### LONDON ECON MODELS
mod6 <- lmer(data = raw11, voteConTwoParty ~ londonLocalEcon + 
             p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod7 <- lmer(data = raw15, voteConTwoParty ~ londonLocalEcon +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod10 <- lmer(data = raw11, voteSpectrum ~ londonLocalEcon*belongLocal + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod11 <- lmer(data = eng11, voteSpectrum ~ londonLocalEcon*belongLocal + 
                p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))

#change model classes
class(mod6) <- "lmerMod" ; class(mod7) <- "lmerMod" ; class(mod8) <- "lmerMod" 
class(mod11) <- "lmerMod" ; class(mod10) <- "lmerMod"

#Stargazer
stargazer(mod6, mod7, mod10, mod11, type = "latex", header = FALSE, 
          model.numbers = TRUE, 
          dep.var.labels.include = FALSE, 
          dep.var.caption = "Vote choice (1-5, right-left)", 
          no.space = TRUE, 
          column.labels = c("2017","2019","2017", "2017 (England)"),
          title = "london_local_mod",
          omit = c("p_edlevel", "age","male","p_socgrade","white_british"),
          covariate.labels = c("London-local diff", "Local belonging", "London-local * Local belonging"),
          omit.stat = c("aic","bic"),
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.sep.width = "3pt")

#ggplot predictions 
ggplot(ggpredict(mod10, terms = c("londonLocalEcon","belongLocal"))) + 
  geom_line(aes(x, predicted, color = group)) + 
  geom_ribbon(aes(x, predicted, ymin = conf.low, ymax = conf.high, 
                  fill = group), alpha = 0.2)








####################################################################
########## REDISTRIBUTION MODELS 
##########################################################3

red1 <- lmer(data = raw11, redistSelf ~ localEcon + p_gross_household + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red2 <- lmer(data = raw15, redistSelf ~ localEcon + p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red3 <- lmer(data = raw11, redistSelf ~ localEcon + econGenRetro + econPersonalRetro + 
               p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red4 <- lmer(data = raw15, redistSelf ~ localEcon + econGenRetro + econPersonalRetro + 
               p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red5 <- lmer(data = raw11, redistSelf ~ localEcon*belongLocal  + econGenRetro + econPersonalRetro + p_gross_household + 
                p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
class(red1) <- "lmerMod"; class(red2) <- "lmerMod"
class(red3) <- 'lmerMod' ; class(red4) <- "lmerMod" ; class(red5) <- "lmerMod"

stargazer(red1, red2, red3, red4, red5, type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001), 
          omit = c("p_gross_household","p_edlevel","age",
                   "male","p_socgrade", "white_british","Constant"),
          label = "redist_mods",
          omit.stat = c("aic","bic"),
          no.space = TRUE, 
         covariate.labels = c("Local econ", "Local belong", "General econ","Personal econ",
                               "Econ * belong"),
          dep.var.labels = "Redistribution attitudes (0-10, left-right)",
          column.labels = c("2017","2019","2017","2019","2017"))

###########################################3
#### immigration models
########################################################33
immig1 <- lmer(data = raw11, immigSelf ~ localEcon + p_gross_household + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig2 <- lmer(data = raw15, immigSelf ~ localEcon + p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig3 <- lmer(data = raw11, immigSelf ~ localEcon + econGenRetro + econPersonalRetro + 
               p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig4 <- lmer(data = raw15, immigSelf ~ localEcon + econGenRetro + econPersonalRetro + 
               p_gross_household  +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig5 <- lmer(data = raw11, immigSelf ~ localEcon*belongLocal + econGenRetro + econPersonalRetro + p_gross_household + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
class(immig1) <- "lmerMod"; class(immig2) <- "lmerMod"
class(immig3) <- 'lmerMod' ; class(immig4) <- "lmerMod" ; class(immig5) <- "lmerMod"

stargazer(immig1, immig2, immig3, immig4, immig5, type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001), 
          omit = c("p_gross_household","p_edlevel","age",
                   "male","p_socgrade", "white_british"),
          omit.stat = c("aic","bic"),
          no.space = TRUE,
          label = "tab:immig_mods",
          covariate.labels = c("Local econ", "Local belong"," General econ","Personal econ",
                              "Local econ * belong"),
          dep.var.labels = "Immigration attitudes (0-10, higher-lower)",
          column.labels = c("2017","2019","2017","2019","2017"))
#plot the interaction from the last model
immig_plot <- ggpredict(immig5, terms = c("localEcon","belongLocal", 
                                          "econGenRetro [3]","econPersonalRetro [3]")) %>% 
  plot() + 
  labs(x = "Local economic evaluation", 
       y = "Immigration preferences",
       title = "Predicted immigration attitudes, by local economic evaluations") + 
  guides(colour=guide_legend(title=str_wrap("Local belonging", 7)))
ggsave("drafts/paper1/figures/immig_plot.pdf", immig_plot)

#for people who belong local, better econ = more anti-immigrant
#for people who don't belong local, better econ = less anti-immigrant




################################################################################
######LOCAL ECONOMIC MODELS
#################################################3

local1_constant <- lmer(data = raw11, 
               voteSpectrum ~ scale(median_income) + 
                 scale(price2016) + rate2017 + log(population_density) + 
                 pct_knowledge + ethnicity_white_british + 
                 (1 | pano))
class(local1_constant) <- "lmerMod"
plot(ggpredict(local1_constant, terms = "pct_knowledge"))

#make scaled versions of the variables

local1_dynamic <- lmer(data = moddat, 
                       voteConTwo ~ scale(price_change20yr) + 
                         unem_change_10yr + median_per_change_2017 +  log(population_density) + 
                         ethnicity_white_british + pct_knowledge + (1 | pano))
class(local1_dynamic) <- "lmerMod"
stargazer(local1_constant, local1_dynamic, type = "text")















##########################################
##CONTEXT/INTERACTION MODELS
####################################
eng <- raw11 %>% 
  filter(country.x %in% c("England","Wales"))


m1 <- glmer(data = eng, voteConTwoParty ~ 
              londonLocalEcon + price_change20yr + scale(median_income) + 
             p_gross_household + p_edlevel + age + male + 
             p_socgrade + white_british + (1 | oslaua_name),
            family = binomial(link = "logit"))

m2 <- glmer(data = eng, voteConTwoParty ~ 
              londonRegionEcon + price_change20yr + scale(median_income) + 
              p_gross_household + p_edlevel + age + male + 
              p_socgrade + white_british + (1 | oslaua_name),
            family = binomial(link = "logit"))
#interesting result - 
#people in places whose economy has improved but who think London's doing better
#are more inclined to support conservatives ... is there some cultural story here? 
class(m1) <- "lmerMod" ; class(m2) <- "lmerMod"
stargazer(m1, m2, type = "text")

#make predicitons of model 2
predict <- ggpredict(m1, terms = c("londonLocalEcon"))
plot(predict)





















#make plot of group belonging
raw11 <- raw11 %>% 
  rename(belongEthnicity = belongGroup_5)
belongs <- raw11 %>% dplyr::select(starts_with("belong")) %>% 
  rename(belongNone = belongGroup_111) %>%
  dplyr::select(-belongGroup_99)
 names <- c("Region","Locality","Middle Class","Working Class","Ethnicity","None")


belong_plot <- belongs %>% 
  pivot_longer(cols = belongRegion:belongNone, 
               names_to = "variable", values_to = "value") %>% 
  mutate(variable = str_remove(variable, "belong"),
         variable = str_replace(variable, "Class", " class"),
         variable = str_replace(variable, "Local", "Local community")) %>% 
  group_by(variable) %>% 
  summarize(prop = sum(value, na.rm = TRUE) / n()) %>% 
  arrange(desc(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = prop, y = reorder(variable, prop))) + 
  theme_minimal() + 
  ggtitle("Proportion of respondents with sense of group belonging") + 
  xlab("Proportion") + ylab("Group")

ggsave("drafts/paper1/figures/belonging_plot.pdf", belong_plot)

#do this for strongestconnection as well
strongest_connection_plot <- raw11 %>%
  mutate(connection = case_when(
    strongestConnection == 0 ~ "None of the above",
    strongestConnection == 1 ~ "Sports team",
    strongestConnection == 2 ~ "Musician or band",
    strongestConnection == 3 ~ "Town or city",
    strongestConnection == 4 ~ "Film star or celebrity",
    strongestConnection == 5 ~ "Social club or community group",
    strongestConnection == 6 ~ "A charity",
    strongestConnection == 7 ~ "Local church or religious group",
    strongestConnection == 8 ~ "A school, college, or university",
    strongestConnection == 9 ~ "A film/TV series"
  )) %>% 
  dplyr::select(connection) %>% 
  group_by(connection) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(connection)) %>%
  mutate(total = sum(n),
         prop = n/total) %>% 
  arrange(desc(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = prop, y = reorder(connection, prop))) + 
  theme_minimal() + 
  labs(x = "% with group as strongest connection", 
       y = "Group") 
ggsave("drafts/paper1/figures/strongest_connection_plot.pdf", 
       strongest_connection_plot)
 

#read in data from Understandng Society wave 8
us <- read_dta("data/understanding_society/bhps/stata/stata13_se/ukhls_w8/h_indresp.dta")

us <- us %>% 
  dplyr::select(starts_with("h_ethid"))

us <- us %>% 
  rename(language_home = h_ethid2, 
         language_english = h_ethid3, 
         religion_own = h_ethid4a, religion_raised = h_ethid4b, 
         region_live = h_ethid5, country_born = h_ethid6, 
         region_raised = h_ethid7, color = h_ethid14) 
us[us < 0] <- NA 


#make the belonging plot again, but for the understanding society data 
self <- us %>% 
  select(-starts_with("h_")) %>% 
  pivot_longer(cols = language_home:color, names_to = "id", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  group_by(id) %>%
  summarize(n = n(), very_fairly = sum(value == 1 | value == 2),
            very = sum(value == 1)) %>% 
  mutate(prop_important = very_fairly / n, 
         prop_very_important = very / n, 
         var = case_when(
           id == "religion_raised" ~ "Religion (raised)", 
           id == "religion_own" ~ "Religion (current)", 
           id == "region_raised" ~ "Region (raised)",
           id == "region_live" ~ "Region (current)", 
           id == "language_home" ~ "Language (home)", 
           id == "language_english" ~ "English language", 
           id == "country_born" ~ "Country of birth", 
           id == "color" ~ "Skin color"
         )) %>% 
  filter(id != "religion_raised") %>% 
  ggplot() +  
  geom_col(aes(x = prop_important, y = var)) + 
  xlab("Proportion very/fairly important") + 
  ylab("Category") + 
  ggtitle("Importance of social groupings to sense of self") + 
  theme_minimal()

ggsave("prospectus/figures/ukhls_senseofself.pdf", self)
