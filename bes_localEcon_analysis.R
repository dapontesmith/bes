# Not sure I need all these packages but here we go!
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
library(sf)
library(haven)
library(patchwork)
library(modelsummary)
library(survival)
library(janitor)
rm(list = ls())
options(mc.cores = parallel::detectCores())


setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

raw11 <- read_csv("data/bes/internet_panel/clean_data/wave11_clean.csv")
# transform the vote variables so higher = further right (easier to intrpret for some reason)
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
# raw11$median <- as.numeric(str_remove(raw11$median, ","))
raw11$median_income_scale <- scale(raw11$median_income)
raw11$price_change20yr_scale <- scale(raw11$price_change20yr)
# Make data sets for England and Wales alone
dat15 <- raw15 %>%
  filter(country.y %in% c("England", "Wales"))
dat11 <- raw11 %>% filter(country.y %in% c("England", "Wales"))
eng11 <- raw11 %>%
  filter(country.y == "England") %>%
  filter(region != "Wales")

#make version of data for vote models for fixed effect
raw11_merge <- raw11 %>%
  dplyr::select(pcon, p_edlevel, white_british, p_gross_household, age, male,
                p_socgrade, localEcon, econGenRetro, econPersonalRetro, 
                own_house, vote_con) %>% 
  mutate(year = 2017)
raw15_merge <- raw15 %>%
  dplyr::select(pcon, p_edlevel, white_british, p_gross_household, age,
                male, p_socgrade, localEcon, econGenRetro, econPersonalRetro,
                own_house, voteCon) %>%
  rename(vote_con = voteCon) %>% mutate(year = 2019)

full_raw <- rbind(raw11_merge, raw15_merge)
non_localists <- raw11 %>% filter(belongLocal == 0)

#read in region shapefile, rename regions
shp <- st_read("data/uk_geography/shapefiles/NUTS_Level_1_(January_2018)_Boundaries.shp")
#change coordinate system to WGS 84
shp <- st_transform(shp, "+proj=longlat +datum=WGS84") %>%
  mutate(region = case_when(
    nuts118cd == "UKC" ~ "North East",
    nuts118cd == "UKD" ~ "North West",
    nuts118cd == "UKE" ~ "Yorkshire and The Humber",
    nuts118cd == "UKF" ~ "East Midlands",
    nuts118cd == "UKG" ~ "West Midlands",
    nuts118cd == "UKH" ~ "East of England",
    nuts118cd == "UKI" ~ "London",
    nuts118cd == "UKJ" ~ "South East",
    nuts118cd == "UKK" ~"South West",
    nuts118cd == "UKL" ~ "Wales",
    nuts118cd == "UKM" ~ "Scotland",
    nuts118cd == "UKN" ~ "Northern Ireland",
  )) %>% 
  filter(region != "Northern Ireland")



########################################################################
#########################################################################
## LOCALISM AS DEPENDENT VARIABLE
#####################################################################
#####################################################################

mod_local <- lmer(data = raw11, belongLocal ~ p_edlevel +
  age + male + p_socgrade + white_british +
  p_gross_household + (1 | pcon))
mod_local_house <- lmer(data = raw11, belongLocal ~ p_edlevel +
  age + male + p_socgrade + white_british +
  p_gross_household + own_house +
  (1 | pcon))
mod_local_children <- lmer(data = raw11, belongLocal ~ p_edlevel +
  age + male + p_socgrade + white_british +
  p_gross_household + children_in_household +
  (1 | pcon))

#set model classes for stargazer
class(mod_local) <- "lmerMod"
class(mod_local_house) <- "lmerMod"
class(mod_local_children) <- "lmerMod"

#stargaze these to produce table 2 
stargazer(mod_local, mod_local_house, mod_local_children,
  type = "text", header = FALSE,
  dep.var.labels.include = FALSE,
  dep.var.caption = "Local belonging (0/1)",
  no.space = TRUE,
  model.numbers = TRUE,
  label = "belong_local_mod",
  title = "Local belonging (0/1)",
  omit.stat = c("aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  column.sep.width = "3pt",
    covariate.labels = c(
      "Education", "Age", "Male", "Social grade",
      "White British", "Household income",
      "Owns house", "Children at home"
    )
)


#run more complex models - house, region, party, remainer, policy
mod1_belong_demo <- lmer(data = raw11, belongLocal ~ p_gross_household +
  p_edlevel + age + male + p_socgrade + white_british + own_house +
  (1 | pcon), weights = wt_full_)

mod1_belong_region <- lmer(data = raw11, belongLocal ~ p_gross_household +
  p_edlevel + age + male + p_socgrade + white_british +
  as.factor(region) + (1 | pcon))

mod1_belong_party <- lmer(data = raw11, belongLocal ~ p_gross_household +
  p_edlevel + age + male + p_socgrade + white_british +
  as.factor(partyIdName) + (1 | pcon))

mod1_belong_context <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel +
  age + male + p_socgrade + white_british + (1 | pcon) +
  median_income_scale + scale(price_change5yr) + rate2016 +
  population_density + constituency_type.x, weights = wt_full_)


#####################################

# graph the context results
context_plot <- cbind(
  names(fixef(mod1_belong_context)),
  summary(mod1_belong_context)$coef[, 1],
  summary(mod1_belong_context)$coef[, 2]
) %>%
  as_tibble() %>%
  rename(var = V1, est = V2, sd = V3) %>%
  mutate(var = str_remove(var, "\\("), var = str_remove(var, "\\)")) %>%
  filter(var %in% c(
    "median_income_scale", "scaleprice_change5yr",
    "rate2016", "population_density", "constituency_type.xCounty"
  )) %>%
  mutate(var_name = case_when(
    var == "median_income_scale" ~ "Median income",
    var == "scaleprice_change5yr" ~ "5-yr pct. chg. house prices",
    var == "rate2016" ~ "Unemployment rate (2016)",
    var == "population_density" ~ "Density",
    var == "constituency_type.xCounty" ~ "County constituency"
  ), est = as.numeric(est), sd = as.numeric(sd)) %>%
  mutate(
    conf.low = est - 1.96 * sd,
    conf.high = est + 1.96 * sd
  ) %>%
  ggplot() +
  geom_pointrange(aes(x = var_name, y = est, ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    x = "Variable", y = "Estimate",
    title = "Localism and elements of local context",
    caption = "Multilevel linear models, random effects at constituency level.
       Models include but do not report demographic covariates.
       Median income and house-price variables are scaled to have mean of 0 and s.d. of 1.
       Baseline for the county constituency variable is borough."
  )
ggsave("drafts/paper1/figures/context_plot.pdf", context_plot)



# graph the region results
region_plot <- cbind(
  names(fixef(mod1_belong_region)),
  summary(mod1_belong_region)$coef[, 1],
  summary(mod1_belong_region)$coef[, 2]
) %>%
  as_tibble() %>%
  rename(var = V1, est = V2, sd = V3) %>%
  filter(str_detect(var, "as.factor") == TRUE) %>%
  mutate(
    var = str_remove(var, "as.factor"),
    var = str_remove(var, "region"),
    var = str_remove(var, "\\(\\)"),
    est = as.numeric(est), sd = as.numeric(sd)
  ) %>%
  mutate(
    conf.low = est - 1.96 * sd,
    conf.high = est + 1.96 * sd
  ) %>%
  ggplot() +
  geom_pointrange(aes(x = var, y = est, ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Region", y = "Estimate")




# graph the party results
party_plot <- cbind(
  names(fixef(mod1_belong_party)),
  summary(mod1_belong_party)$coef[, 1],
  summary(mod1_belong_party)$coef[, 2]
) %>%
  as_tibble() %>%
  rename(var = V1, est = V2, sd = V3) %>%
  filter(str_detect(var, "as.factor") == TRUE) %>%
  mutate(
    var = str_remove(var, "as.factor"),
    var = str_remove(var, "partyIdName"),
    var = str_remove(var, "\\(\\)"),
    est = as.numeric(est), sd = as.numeric(sd)
  ) %>%
  mutate(
    conf.low = est - 1.96 * sd,
    conf.high = est + 1.96 * sd
  ) %>%
  ggplot() +
  geom_pointrange(aes(x = reorder(var, est), y = est, ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Party", y = "Estimate",
       caption = "Coefficients on party fixed effects in linear multilevel regressions of local
      belonging on demographic predictors. Baseline party is the Conservatives.",
       title = "Partisan predictors of local belonging")
ggsave("drafts/paper1/figures/party_plot.pdf", party_plot)

#make map of region coefficients


region_df <- cbind(
  names(fixef(mod1_belong_region)),
  summary(mod1_belong_region)$coef[, 1],
  summary(mod1_belong_region)$coef[, 2]
) %>%
  as_tibble() %>%
  rename(region = V1, est = V2, sd = V3) %>%
  filter(str_detect(region, "as.factor") == TRUE) %>%
  mutate(
    region = str_remove(region, "as.factor"),
    region = str_remove(region, "region"),
    region = str_remove(region, "\\(\\)"),
    est = as.numeric(est), sd = as.numeric(sd),
    upper = est + 1.96*sd, 
    lower = est - 1.96*sd,
    sig = case_when(
      upper > 0 & lower > 0 ~ 1, 
      upper > 0 & lower < 0 ~ 0,
      upper < 0 & lower < 0 ~ 1
    )
  ) %>% 
  dplyr::select(region, est) %>% 
  add_row(region = "East Midlands", est = 0)


region_map <- left_join(shp, region_df, by = "region") %>% 
  rename(Coefficient = est) %>% 
  ggplot() + 
  geom_sf(aes(fill = Coefficient), color = NA) + 
  ggtitle("Regional predictors of localism") + 
  #scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(  ),
        legend.position = "left",
        plot.title = element_text(hjust = 0),
        plot.margin = margin(0.1, 0, 0.1, 0, "cm")
  )  + 
  scale_fill_gradient2(low = ("blue"), mid = "white", 
                     high = ("red"), midpoint = 0) +
  labs(caption = "Coefficients on region fixed effects in multilevel linear model regressing 
  local belonging on demographic predictors. Coefficients on Scotland, 
                  Wales, and London FEs are significant. Baseline is East Midlands.")
ggsave("drafts/paper1/figures/region_map.pdf", region_map)

region_party_plot <- region_map + party_plot + plot_annotation(
  title = "Regional and partisan correlates of local belonging"
  # caption = "Results from regressions with fixed effects by region and party identification respectively.
  # Both regressions include demographic covariates. The baseline region is the
  # East Midlands; the baseline party is Conservative. Each point represents the estimated
  # propensity of members of the region or party to identify with the local community, above the baseline.
  # Errors bars represent 95 percent confidence intervals."
)
ggsave("drafts/paper1/figures/region_party_plot.pdf", region_party_plot)



#RUN EFFICACY MODELS 
mod_effic1 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade + as.factor(partyIdName) +
  efficacyPolCare + (1 | pcon), weights = wt_full_)
mod_effic2 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade + as.factor(partyIdName) +
  efficacyNoMatter + (1 | pcon), weights = wt_full_)
mod_effic3 <- lmer(data = raw11, belongLocal ~ p_gross_household +
  p_edlevel + p_socgrade + white_british + as.factor(partyIdName) +
  efficacyVoteEffort + (1 | pcon), weights = wt_full_)
mod_effic4 <- lmer(data = raw11, belongLocal ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade + as.factor(partyIdName) +
  satDemUK + (1 | pcon), weights = wt_full_)
class(mod_effic1) <- "lmerMod"
class(mod_effic2) <- "lmerMod"
class(mod_effic3) <- "lmerMod"
class(mod_effic4) <- "lmerMod"

#stargaze efficacy models 
stargazer(mod_effic1, mod_effic2, mod_effic3, mod_effic4,
  type = "text",
  omit = c(
    "p_edlevel", "age", "male", "p_socgrade", "p_gross_household",
    "white_british", "Constant",
    "as.factor\\(partyIdName\\)Green", "as.factor\\(partyIdName\\)Labour",
    "as.factor\\(partyIdName\\)Lib Dem", "as.factor\\(partyIdName\\)None",
    "as.factor\\(partyIdName\\)Other", "as.factor\\(partyIdName\\)PC",
    "as.factor\\(partyIdName\\)UKIP", "as.factor\\(partyIdName\\)SNP"
  ),
  dep.var.caption = "Local belonging (0/1)",
  dep.var.labels.include = FALSE,
  label = "tab:effic_mods",
  covariate.labels = c(
    "Voting is a lot of effort",
    "Politicians don't care about people like me",
    "Doesn't matter which party is in power",
    "Satifaction with UK democracy"
  ),
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  header = FALSE,
  model.numbers = TRUE, column.sep.width = "3pt"
)



# make these models into coefficient plots
# create function for this purpose
get_effic_est <- function(mod_name, var_name) {
  cbind(
    names(fixef(mod_name)),
    summary(mod_name)$coef[, 1],
    summary(mod_name)$coef[, 2]
  ) %>%
    as_tibble() %>%
    filter(V1 == var_name)
}
# apply to each model
effic1 <- get_effic_est(mod_name = mod_effic1, var_name = "efficacyPolCare")
effic2 <- get_effic_est(mod_name = mod_effic2, var_name = "efficacyNoMatter")
effic3 <- get_effic_est(mod_name = mod_effic3, var_name = "efficacyVoteEffort")
effic4 <- get_effic_est(mod_name = mod_effic4, var_name = "satDemUK")

# create tibble for graphing
effic <- rbind(effic1, effic2, effic3, effic4) %>%
  as_tibble() %>%
  rename(variable = V1, est = V2, sd = V3)

# graph the tibble
#CREATE EFFICACY PLOTS 
efficacy_plot <- effic %>%
  mutate(
    est = as.numeric(est),
    sd = as.numeric(sd),
    ymin = est - 1.96 * sd,
    ymax = est + 1.96 * sd,
    var = case_when(
      variable == "efficacyPolCare" ~ "Politicians don't care", 
      variable == "efficacyNoMatter" ~ "Vote doesn't matter",
      variable == "efficacyVoteEffort" ~ "Voting too much effort",
      variable == "satDemUK" ~ "Satisfied with UK democracy"
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = reorder(var, est), y = est)) +
  geom_pointrange(aes(
    x = reorder(var, est), y = est,
    ymin = ymin, ymax = ymax
  )) +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    y = "Estimated relationship with local belonging",
    x = "",
    title = "Local belonging and attitudes towards political process",
    caption = "Coefficient of local belonging variable in regression of efficacy variable on demographics and
    local belonging. Models include constituency random effects and party fixed effects."
  )
ggsave(
  "drafts/paper1/figures/efficacy_plot.pdf",
  efficacy_plot
)


######################################
#### FEELING THERMOMETERS
##########################################
############################################################################
#### COSMOPOLITANISM / FEELING THERMOMETER ANALYSES
################################################################### 3

# same as above, including variables for attitudes towards britishness and towards immigrants
mod_warm_syrian <- lmer(data = raw11, warmSyrians ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade +
  ethnicity_white_british + rate2017 + belongLocal + (1 | pcon), weights = wt_full_)
mod_warm_indian <- lmer(data = raw11, warmIndian ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade +
  belongLocal + ethnicity_white_british + rate2017 + belongLocal + (1 | pcon), weights = wt_full_)
mod_warm_eastern <- lmer(data = raw11, warmEastern ~ p_gross_household + p_edlevel +
  age + male + white_british + p_socgrade +
  belongLocal + ethnicity_white_british + rate2017 + belongLocal + (1 | pcon), weights = wt_full_)
mod_customs <- lmer(data = raw11, britCustoms ~ p_gross_household +
  p_edlevel + age + male + white_british + p_socgrade +
  belongLocal + ethnicity_white_british + rate2017 + belongLocal + (1 | pcon))
mod_christian <- lmer(data = raw11, britChristian ~ p_gross_household +
  p_edlevel + age + male + white_british + p_socgrade +
  belongLocal + ethnicity_white_british + rate2017 + belongLocal + (1 | pcon))

class(mod_warm_syrian) <- "lmerMod"
class(mod_warm_indian) <- "lmerMod"
class(mod_warm_eastern) <- "lmerMod"
class(mod_customs) <- "lmerMod"
class(mod_christian) <- "lmerMod"

#use efficacy estimate function
coef1 <- get_effic_est(mod_name = mod_warm_indian, var_name = "belongLocal")
coef2 <- get_effic_est(mod_name = mod_warm_syrian, var_name = "belongLocal")
coef3 <- get_effic_est(mod_name = mod_warm_eastern, var_name = "belongLocal")
coef4 <- get_effic_est(mod_name = mod_customs, var_name = "belongLocal")
coef5 <- get_effic_est(mod_name = mod_christian, var_name = "belongLocal")


# create tibble for graphing
coefs <- rbind(coef1, coef2, coef3) %>%
  as_tibble() %>%
  rename(var = V1, est = V2, sd = V3)
#set model names and create plot 
mod_names <- c("Syrians", "Indians", "Eastern Europeans")
warm_plot <- coefs %>%
  mutate(
    est = as.numeric(est),
    sd = as.numeric(sd),
    ymin = est - 1.96 * sd,
    ymax = est + 1.96 * sd,
    mod_name = mod_names
  ) %>%
  ggplot() +
  geom_point(aes(x = reorder(mod_name, est), y = est)) +
  geom_pointrange(aes(
    x = reorder(mod_name, est), y = est,
    ymin = ymin, ymax = ymax
  )) +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "Estimate", x = element_blank(), title = "Feeling thermometers towards immigrants")

#set model names 
mod_names <- c("Customs", "Christianity")
#create plot 
brit_plot <- rbind(coef4, coef5) %>%
  as_tibble() %>%
  rename(var = V1, est = V2, sd = V3) %>%
  mutate(
    est = as.numeric(est),
    sd = as.numeric(sd),
    ymin = est - 1.96 * sd,
    ymax = est + 1.96 * sd,
    mod_name = mod_names
  ) %>%
  ggplot() +
  geom_point(aes(x = reorder(mod_name, est), y = est)) +
  geom_pointrange(aes(
    x = reorder(mod_name, est), y = est,
    ymin = ymin, ymax = ymax
  )) +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "Estimate", x = "", title = "Attitudes towards Britishness")

#patchwork the plots together 
cosmo_plot <- brit_plot + warm_plot +
  theme(plot.title = element_text(hjust = 0.5)) +
  plot_annotation(
    title = "Local Belonging and Cosmopolitan Attitudes",
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Coefficient on binary local belonging term in linear random-effects regression.
    Coefficients can be interpreted as difference between localist and non-localists respondents.
    Scale on Britishness models is 1-4; scale on feeling thermometers is 0-100.
    Models control for respondent characteristics and local unemployment and % white."
  )
ggsave("drafts/paper1/figures/cosmo_plot.pdf", cosmo_plot)

# stargaze the results as well
stargazer(mod_warm_syrian, mod_warm_indian, mod_warm_eastern, mod_customs, mod_christian,
  type = "text",
  keep = c("belongLocal", "ethnicity_white_british", "rate2017"),
  dep.var.labels.include = FALSE,
  covariate.labels = c("% White British", "Unemployment Rate", "Local belonging"),
  column.labels = c("Syrians", "Indians", "East Europeans", "Customs", "Christianity"),
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  label = "tab:mod_cosmopolitan",
  header = FALSE
)



#################################################################
## VOTE CHOICE
################################################# 3

#### basic local Econ analyses, plus interaction with belongLoacl
mod1 <- lmer(data = raw11, vote_con ~ localEcon + p_gross_household +
  p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod2 <- lmer(data = raw15, voteCon ~ localEcon + p_gross_household +
  p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod3 <- lmer(data = raw11, vote_con ~ localEcon + p_gross_household +
  econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod4 <- lmer(data = raw15, voteCon ~ localEcon + p_gross_household + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
mod5 <- lmer(data = raw11, vote_con ~ localEcon + belongLocal + econGenRetro +
  econPersonalRetro + p_edlevel + age + male + p_socgrade + p_gross_household +
  white_british + (1 | pcon))
mod6 <- lmer(data = raw11, vote_con ~ localEcon * belongLocal + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + p_gross_household + white_british + (1 | pcon))

#set model classes
class(mod1) <- "lmerMod"
class(mod2) <- "lmerMod"
class(mod3) <- "lmerMod"
class(mod4) <- "lmerMod"
class(mod5) <- "lmerMod"
class(mod6) <- "lmerMod"


#stargazer vote models 
stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
  type = "latex",
  omit = c(
    "p_edlevel", "age", "male", "p_socgrade", "p_gross_household",
    "white_british", "Constant"
  ),
  dep.var.caption = "Vote Conservative (0/1)",
  dep.var.labels.include = FALSE,
  label = "tab:vote_mods",
  covariate.labels = c(
    "Local econ", "Local belong",
    "General econ", "Personal econ",
    "Local econ * belong"
  ),
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  header = FALSE,
  font.size = "small",
  model.numbers = TRUE, column.sep.width = "3pt",
  column.labels = c(
    "2017", "2019", "2017", "2019",
    "2017", "2017"
  )
)


# Table on asset-based localism and vote choice
raw11 <- raw11 %>% mutate(`Own House` = own_house)
raw15 <- raw15 %>% mutate(`Own House` = own_house)

mod_home_2017_1 <- lmer(data = raw11, vote_con ~  own_house + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + p_gross_household + white_british + (1 | pcon))
mod_home_2017_2 <- lmer(data = raw11, vote_con ~ localEcon * own_house + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + p_gross_household + white_british + (1 | pcon))
mod_home_2019_1 <- lmer(data = raw15, voteCon ~ own_house + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + p_gross_household + white_british + (1 | pcon))
mod_home_2019_2 <- lmer(data = raw15, voteCon ~ localEcon * own_house + econGenRetro + econPersonalRetro +
  p_edlevel + age + male + p_socgrade + p_gross_household + white_british + (1 | pcon))
class(mod_home_2017_1) <- "lmerMod"
class(mod_home_2019_1) <- "lmerMod"
class(mod_home_2017_2) <- "lmerMod"
class(mod_home_2019_2) <- "lmerMod"

#stargazer homeownership models 
stargazer(mod_home_2017_2, mod_home_2019_2,
  type = "text",
  omit = c(
    "p_edlevel", "age", "male", "p_socgrade", "p_gross_household",
    "white_british", "Constant"
  ),
  dep.var.caption = "Vote Conservative (0/1)",
  dep.var.labels.include = FALSE,
  label = "tab:house_mods",
  # covariate.labels = c("Local econ","Own house",
  #                    "General econ","Personal econ",
  #                   "Local econ * own house"),
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  header = FALSE,
  model.numbers = TRUE, column.sep.width = "3pt",
  column.labels = c("2017", "2019")
)

#plot homeownership models 
plot_mod_home_2017 <- plot(ggpredict(mod_home_2017_2, terms = c("localEcon", "own_house", "econGenRetro [3]", "white_british [1]")),
                           colors = "bw") +
  labs(x = "Local economic evaluation (low - high)",
       title = "2017 Sample") +
  theme(axis.title.y = element_blank(), 
        legend.position = "none") 


plot_mod_home_2019 <- plot(ggpredict(mod_home_2019_2, terms = c("localEcon", "own_house", "
                                               econGenRetro [3]", "white_british [1]")),
                           colors = "bw") +
  labs(x = "Local economic evaluation (low - high)",
       title = "2019 Sample") +
  theme(axis.title.y = element_blank()) +
  guides(linetype = guide_legend(title = str_wrap("Own house", 7)))

plot_mod_home <- plot_mod_home_2017 + 
  plot_mod_home_2019 + 
  plot_annotation(
    title = "Predicted probability of Conservative vote, by homeownership",
    caption = "Probabilities predicted from random-effects linear regression.
    Probabilities are for white British, male respondent with neutral general
    economic evaluation and mean values of all other covariates."
  )
ggsave("drafts/paper1/figures/plot_mod_home.pdf", plot_mod_home)





# make predicted values plots from the models
plot_mod3 <- plot(ggpredict(mod3, terms = c(
  "localEcon", "econGenRetro [3]",
  "male [1]", "white_british [1]"
))) +
  labs(
    x = "Local economic evaluation (low - high)",
    y = "Predicted probability"
  ) +
  theme(title = element_blank())
plot_mod5 <- plot(ggpredict(mod5, terms = c("localEcon", "belongLocal", "
                                               econGenRetro [3]", "white_british [1]"))) +
  labs(x = "Local economic evaluation (low - high)") +
  theme(
    axis.title.y = element_blank(),
    title = element_blank()
  ) +
  guides(colour = guide_legend(title = str_wrap("Local belonging", 7)))

plot_local_vote_logit <- plot_mod3 + plot_mod5 +
  plot_annotation(
    title = "Predicted probability of Conservative vote",
    caption = "Probabilities predicted from random-effects linear regression.
    Probabilities are for white British, male respondent with neutral general
    economic evaluation and mean values of all other covariates."
  )
ggsave("drafts/paper1/figures/plot_local_vote.pdf")








# make plot of group belonging by category
raw11 <- raw11 %>%
  rename(belongEthnicity = belongGroup_5)
belongs <- raw11 %>% dplyr::select(starts_with("belong"), -starts_with("belongGroup"))
names <- c("Region", "Locality", "Middle Class", "Working Class", "Ethnicity")


belong_plot <- belongs %>%
  pivot_longer(
    cols = belongRegion:belongEthnicity,
    names_to = "variable", values_to = "value"
  ) %>%
  mutate(
    variable = str_remove(variable, "belong"),
    variable = str_replace(variable, "Class", " class"),
    variable = str_replace(variable, "Local", "Local community")
  ) %>%
  group_by(variable) %>%
  summarize(prop = sum(value, na.rm = TRUE) / n()) %>%
  arrange(desc(prop)) %>%
  ggplot() +
  geom_col(aes(x = prop, y = variable)) +
  theme_minimal() +
  ggtitle("Proportion of respondents with sense of group belonging") +
  xlab("Proportion") +
  ylab("Group")

ggsave("prospectus/figures/belonging_plot.pdf", belong_plot)


# read in data from Understandng Society wave 8




#produce table of respondent sample size by region
raw11 %>%
  filter(!is.na(belongLocal) & !is.na(p_edlevel) &
    !is.na(age) & !is.na(male) & !is.na(p_socgrade) &
    !is.na(white_british) & !is.na(p_gross_household) & !is.na(pcon)) %>%
  group_by(region) %>%
  summarize(n = n()) %>%
  filter(!is.na(region)) %>%
  rename(Region = region, N = n) %>%
  adorn_totals("row") %>%
  xtable(
    include.rownames = FALSE,
    caption = "Respondents by region in basic specification",
    label = "respondents_by_region"
  )




#make map of local belonging by region

#function to make map of anything by region 
region_plot <- function(input_var, var_name){
  
  plot_df <- raw11 %>%
    group_by(region) %>% 
    summarize(var_name = mean({{input_var}}, na.rm = TRUE)) 
  
  plot_df <- left_join(shp, plot_df, by = "region")
  mean <- raw11 %>% summarise(mean = mean({{input_var}}, na.rm = TRUE)) %>% 
    pull()
  plot <- plot_df %>%
    ggplot() + 
    geom_sf(aes(fill = var_name)) + 
    theme(rect = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "right") + 
    labs(fill = var_name) +
    scale_fill_gradient2(low = c("blue"), mid = "white",
                         high = "red", midpoint = mean)
  return(plot)
}

belong_region_plot <- region_plot(input_var = belongRegion, 
                                  var_name = "Regional belonging")
belong_local_plot <- region_plot(input_var = belongLocal,
                                 var_name = "Local belonging")
belong_region_plot + belong_local_plot



#look at relationship between strongest connection and localism

mod_local <- lmer(data = raw11, strongestConnectionLocal ~ p_edlevel +
                    age + male + p_socgrade + white_british +
                    p_gross_household + (1 | pcon))
mod_local_house <- lmer(data = raw11, strongestConnectionLocal ~ p_edlevel +
                          age + male + p_socgrade + white_british +
                          p_gross_household + own_house +
                          (1 | pcon))
mod_local_children <- lmer(data = raw11, strongestConnectionLocal ~ p_edlevel +
                             age + male + p_socgrade + white_british +
                             p_gross_household + children_in_household +
                             (1 | pcon))

#set model classes for stargazer
class(mod_local) <- "lmerMod"
class(mod_local_house) <- "lmerMod"
class(mod_local_children) <- "lmerMod"

#stargaze these to produce table 2 
stargazer(mod_local, mod_local_house, mod_local_children,
          type = "text", header = FALSE,
          dep.var.labels.include = FALSE,
          dep.var.caption = "Local belonging (0/1)",
          no.space = TRUE,
          model.numbers = TRUE,
          label = "belong_local_mod",
          title = "Local belonging (0/1)",
          omit.stat = c("aic", "bic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "3pt"
          # covariate.labels = c(
          #   "Education", "Age", "Male", "Social grade",
          #   "White British", "Household income",
          #   "Owns house", "Children at home"
          # )
)



