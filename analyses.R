library(tidyverse)
library(lmerTest)
library(stargazer)
library(ggeffects)
library(ordinal)
library(effects)
library(brms)
library(sjPlot)
library(expss)
library(parallel)
library(clubSandwich)
library(modelsummary)
library(texreg)
library(sf)
library(lmtest)
library(patchwork)
library(lfe)
#Clear workspace - BEWARE
rm(list = ls())

# call this once to distribute chains across cpu cores:
options(mc.cores=parallel::detectCores())

#save figures to filepath: drafts/paper1/figures
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

#read in data, transform the five-point vote variable
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
  ))
raw15 <- raw15 %>% 
  mutate(voteSpectrum = case_when(
    voteSpectrum == 1 ~ 5, 
    voteSpectrum == 2 ~ 4, 
    voteSpectrum == 4 ~ 2, 
    voteSpectrum == 5 ~ 1, 
    TRUE ~ voteSpectrum
  ))
#make factorized versions of the voteSpectrum variable, for brms
raw15$voteSpectrumFactor <- as.factor(raw15$voteSpectrum)
raw11$voteSpectrumFactor <- as.factor(raw11$voteSpectrum)

#make geographic subsets of the data
raw <- raw15 %>% 
  filter(country.y %in% c("England","Wales"))
raw11 <- raw11 %>% filter(country.y %in% c("England","Wales"))
eng11 <- raw11 %>% filter(country.y == "England")

num_in_oslaua_region <- raw11 %>% 
  group_by(region_name, oslaua_name) %>% 
  summarise(num = n()) 

#filter out people whose oslauas and regions don't match - 
raw11 <- raw11 %>% 
  group_by(region_name, oslaua_name) %>% 
  mutate(num = n()) %>% 
  filter(num > 2) 


#histogram of londonLocalEcon variable
hist_londonlocal <- ggplot(raw11) + 
  geom_bar(aes(x = londonLocalEcon,
               y = stat(count)/ sum(stat(count))),
           width = 0.5) + 
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4)) + 
  theme_minimal() + 
  labs(x = "Difference between London and local economic perceptions", 
       y = "Density")
ggsave("drafts/paper1/figures/hist_londonlocal.pdf")

#show map of londonLocalEcon by region
shp <- st_read("data/uk_geography/shapefiles/NUTS_Level_1_(January_2018)_Boundaries.shp")
shp <- shp %>% 
  mutate(region_name = str_replace(nuts118nm, " \\(England\\)",""),
         region_name = ifelse(region_name == 'Yorkshire and The Humber',
                              "Yorkshire and the Humber", region_name))
london_region_diff <- raw11 %>% 
  group_by(region_name) %>% 
  summarise(diff = mean(londonRegionEcon, na.rm = TRUE)) 
diffs_shp <- left_join(shp, london_region_diff, by = "region_name") %>%
  filter(region_name != "Northern Ireland")
diffs_shp <- st_transform(diffs_shp, "+proj=longlat +datum=WGS84")

region_map <- diffs_shp %>% 
  rename(`London-region` = diff) %>% 
  ggplot() + 
  geom_sf(aes(fill = `London-region`), color = NA) +
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank())

#make the same map by local authority instead
london_local_diff <- raw11 %>% 
  filter(region_name != "London") %>% 
  group_by(LAD17CD) %>% 
  summarise(diff = mean(londonLocalEcon, na.rm = TRUE)) 
oslauas <- st_read("data/uk_geography/shapefiles/lad17.shp")
diffs_shp <- left_join(oslauas, london_local_diff, by = c("lad17cd" = "LAD17CD"))

lad_map <- diffs_shp %>% 
  rename(`London-local` = diff) %>% 
  ggplot() +
  geom_sf(aes(fill = `London-local`), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 



map <- region_map + lad_map + 
  plot_annotation(caption = "Difference between evaluations of London's and regional/local economic 
                  performance over the last 12 months. 9-point scale, from -4 to 4.")
ggsave("drafts/paper1/figures/map.pdf", map)

#make plot of local belonging by region
local_belong <- raw11 %>% 
  filter(oslaua_name != "Orkney Islands" & 
           oslaua_name != "Shetland Islands") %>% 
  group_by(LAD17CD) %>% 
  summarise(belong = mean(belongLocal, na.rm = TRUE)) 
belong_shp <- left_join(oslauas, local_belong, by = c("lad17cd" = "LAD17CD")) 
belong_shp <- st_transform(belong_shp, "+proj=longlat +datum=WGS84") %>%
  filter(!(lad17nm %in% c("Orkney Islands","Shetland Islands")))
belong_map <- belong_shp %>% 
  rename(`Local belonging` = belong) %>% 
  ggplot() +
  geom_sf(aes(fill = `Local belonging`), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave("drafts/paper1/figures/local_belonging_map.pdf", belong_map)

#map regional belonging by region
region_belong <- raw11 %>% 
  group_by(region_name) %>% 
  summarise(belong = mean(belongRegion, na.rm = TRUE)) 
belong_shp <- left_join(shp, region_belong, by = c("region_name")) 
belong_shp <- st_transform(belong_shp, "+proj=longlat +datum=WGS84")
belong_map <- belong_shp %>% 
  rename(`Region belonging` = belong) %>% 
  filter(region_name != "Northern Ireland") %>% 
  ggplot() +
  geom_sf(aes(fill = `Region belonging`), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

#map local belonging by oslaua
local_belong <- raw11 %>% 
  group_by(LAD17CD) %>% 
  summarise(belong = mean(belongLocal, na.rm = TRUE)) 
belong_shp <- left_join(oslauas, local_belong, by = c("lad17cd" = "LAD17CD")) 
belong_shp <- st_transform(belong_shp, "+proj=longlat +datum=WGS84")
local_belong_map <- belong_shp %>% 
  rename(`Local belonging` = belong) %>% 
  ggplot() +
  geom_sf(aes(fill = `Local belonging`), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme(rect = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
belong_map <- belong_map + local_belong_map + 
  plot_annotation(caption = "Proportion of respondents reporting regional/local belonging,
                  by region/local authority")
ggsave("drafts/paper1/figures/belong_map.pdf", belong_map)


#############################################
## MODELS USING LOCALISM AS A DEPENDNET VARIABLE
####################################################
mod_local <- lmer(data = raw11, belongLocal ~ p_edlevel + 
                   age + male + p_socgrade + white_british + 
                   p_gross_household + (1 | pcon))
mod_local_house <- lmer(data = raw11, belongLocal ~ p_edlevel + 
                     age + male + p_socgrade + white_british + 
                     p_gross_household + own_house + 
                       (1 | pcon))
mod_local_econ <- lmer(data = raw11, belongLocal ~ p_edlevel + 
                           age + male + p_socgrade + white_british + 
                           p_gross_household  + localEcon + 
                           (1 | pcon))

class(mod_local) <- "lmerMod"
class(mod_local_house) <- "lmerMod"
class(mod_local_econ) <- "lmerMod"


stargazer(mod_local, mod_local_house, mod_local_econ,
          type = "latex", header = FALSE,
          dep.var.labels.include = FALSE,
          dep.var.caption = "Local belonging (0/1)", 
          no.space = TRUE, 
          model.numbers = TRUE,
          title = "belong_local_mod",
          omit.stat = c("aic","bic"), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "3pt",
          covariate.labels = c("Education","Age","Male","Social grade", 
                               "White British","Household income", 
                               "Owns house", "Local econ"))








#make plots of belonging by party and by region
plot_party_belong <- raw11 %>% 
  group_by(partyIdName) %>% 
  filter(partyIdName %in% c("Labour","Conservative", "Lib Dem",
                            "Green","SNP","UKIP","None","PC")) %>% 
  mutate(partyIdName = ifelse(partyIdName == "PC", "Plaid Cymru", partyIdName)) %>% 
  summarise(belong = mean(belongLocal, na.rm = TRUE)) %>% 
  ggplot() + geom_col(aes(x = partyIdName, y = belong)) + 
  theme_minimal() +  labs(x = "Party", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 30))
ggsave("drafts/paper1/figures/plot_party_belong.pdf", plot_party_belong)

mod_local_region <- glm(data = raw11, belongLocal ~ p_edlevel + 
                            age + male + p_socgrade + white_british + 
                            p_gross_household + as.factor(region_name),
                        family = binomial(link = "logit"))

mod_local_party <- glm(data = raw11, belongLocal ~ p_edlevel + 
                           age + male + p_socgrade + white_british + 
                           p_gross_household + as.factor(partyIdName) + 
                         as.factor(region_name),
                       family = binomial(link = "logit"))

stargazer(mod_local_party, type = "latex",
          omit = c("p_edlevel","age","male","p_socgrade",
                   "white_british","p_gross_household","Constant"),
          covariate.labels = c("Green","Labour","Lib Dem",
                             "None","Other",
                               "Plaid Cymru","SNP","UKIP",
                               "East of England","London",
                            "North East","North West","Scotland",
                            "South East","South West",
                             "Wales","West Midlands","Yorkshire"),
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01,0.001), 
          dep.var.labels = "Local belonging (0/1)",
          label = "belong_appendix")



#plot of regional belonging, by region
plot_region_belong <- raw11 %>% 
  group_by(region_name) %>% 
  summarise(belong = mean(belongRegion, na.rm = TRUE)) %>% 
  mutate(region_name = 
           ifelse(region_name == "Yorkshire and the Humber","Yorkshire", region_name)) %>%
  ggplot() + geom_col(aes(x = region_name, y = belong)) + 
  theme_minimal() +  labs(x = "Region", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 30))
ggsave("drafts/paper1/figures/plot_region_belong.pdf", plot_region_belong)

plot_local_belong_by_region <- raw11 %>% 
  group_by(region_name) %>% 
  summarise(belong = mean(belongLocal, na.rm = TRUE)) %>% 
  mutate(region_name = 
           ifelse(region_name == "Yorkshire and the Humber","Yorkshire", region_name)) %>%
  ggplot() + geom_col(aes(x = region_name, y = belong)) + 
  theme_minimal() +  labs(x = "Region", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 30))
ggsave('drafts/paper1/figures/plot_local_belong_by_region.pdf', plot_local_belong_by_region)


plot_belong <- plot_region_belong + 
  plot_party_belong + 
  plot_annotation(title = "Sense of local belonging, by party and region")

#make plots of londonLocalEcon by party and by region
plot_party_econ <- raw11 %>% 
  group_by(partyIdName) %>% 
  filter(partyIdName %in% c("Labour","Conservative",
                            "Green","SNP","UKIP","None","PC")) %>% 
  summarise(econ = mean(londonLocalEcon, na.rm = TRUE)) %>% 
  ggplot() + geom_col(aes(x = partyIdName, y = econ)) + 
  theme_minimal() +  labs(x = "Party",
                          y = "Mean perceived London-local economic difference") +
  theme(axis.text.x = element_text(angle = 30))







mod_local_region <- glm(data = raw11, belongLocal ~ p_edlevel + 
                          age + male + p_socgrade + white_british + 
                          p_gross_household + factor(region_name), 
                        family = binomial(link = "logit"))


mod_london <- lm(data = raw11, londonLocalEcon ~ p_edlevel + 
                   age + male + p_socgrade + white_british + 
                   p_gross_household)
mod_london_region <- lm(data = raw11, londonLocalEcon ~ p_edlevel + 
                          age + male + p_socgrade + white_british + 
                          p_gross_household + factor(region_name))


#########################################################
#### VOTE CHOICE MODELS
#################################################3
#london-Local - logit - twoparty 
local_logit_random_twoparty <- glmer(data = raw11, voteConTwoParty ~ londonLocalEcon + 
               p_edlevel + age + male + p_socgrade + white_british + 
                 p_gross_household + 
                 (1 | pcon_name), family = binomial(link = "logit"))

#belong - logit - twoparty
belong_logit_random_twoparty <- glmer(data = raw11, voteConTwoParty ~ belongLocal + 
                                p_edlevel + age + male + p_socgrade + white_british + 
                                p_gross_household + 
                                (1 | pcon_name), family = binomial(link = "logit"))

#london-Local - logit - twoparty 
local_logit_twoparty <- glm(data = raw11, voteConTwoParty ~ londonLocalEcon + 
                                       p_edlevel + age + male + p_socgrade + white_british + 
                                       p_gross_household, family = binomial(link = "logit"))

#belong - logit - twoparty
belong_logit_twoparty <- glm(data = raw11, voteConTwoParty ~ belongLocal + 
                                        p_edlevel + age + male + p_socgrade + white_british + 
                                        p_gross_household, family = binomial(link = "logit"))


#london-local linear, twoparty
local_linear_random_twoparty <- lmer(data = raw11, voteConTwoParty ~ londonLocalEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                              p_gross_household + (1 | pcon_name))

#london-Local - linear random - spectrum 
local_random_spectrum <- lmer(data = raw11, voteSpectrum ~ londonLocalEcon + 
                                p_edlevel + age + male + p_socgrade + white_british + 
                                p_gross_household + (1 | pcon_name))

#london-local - fixed - spectrum
local_fixed_spectrum <- felm(data = raw11, londonLocalEcon ~ p_edlevel + age + male + 
                               p_socgrade + white_british + p_gross_household | 
                               pcon_name | 0 | pcon_name)

#london-local * belong - logit - twoparty - random
local_belong_logit_random_twoparty <- glmer(data = raw11, voteConTwoParty ~ 
                                       londonLocalEcon*belongLocal + 
                                       p_edlevel + age + male + p_socgrade + 
                                       white_british + p_gross_household + 
                                       (1 | pcon_name), family = binomial(link = "logit"))

#london-local * belong - logit - twoparty 
local_belong_logit_twoparty <- glm(data = raw11, voteConTwoParty ~ 
                                              londonLocalEcon*belongLocal + 
                                              p_edlevel + age + male + p_socgrade + 
                                              white_british + p_gross_household, 
                                   family = binomial(link = "logit"))

#london-local * belong - linear - twoparty
local_belong_linear_twoparty <- lmer(data = raw11, voteConTwoParty ~ londonLocalEcon*belongLocal + 
                                       p_edlevel + age + male + p_socgrade + white_british + 
                                       p_gross_household + (1 | pcon_name))

#london-local * belong - linear random - spectrum 
local_belong_random_spectrum <- lmer(data = raw11, voteSpectrum ~ londonLocalEcon*belongLocal + 
                                       p_edlevel + age + male + p_socgrade + white_british + 
                                       p_gross_household + (1 | pcon_name))

#london-local * belong - fixed linear - spectrum
local_belong_fixed_spectrum <- felm(data = raw11, voteSpectrum ~ londonLocalEcon*belongLocal + 
                                      p_edlevel + age + male + p_socgrade + 
                                      white_british + p_gross_household | pcon_name | 0 | pcon_name)



#predicted values, random logit twoparty, no interaction
plot_local_logit_random_twoparty <- plot(
  ggpredict(local_logit_random_twoparty, terms = c("londonLocalEcon","male [1]",
                                                   "white_british [1]", "p_socgrade [3]"))
) + labs(title = "Predicted probability of voting Conservative",
         x = "Perceived difference between London and local economies",
         y = "Predicted probability")
ggsave("drafts/paper1/figures/plot_local_logit_random_twoparty.pdf",
       plot_local_logit_random_twoparty)

plot_local_linear_random_twoparty <- plot(
  ggpredict(local_linear_random_twoparty, terms = c("londonLocalEcon","male [1]",
                                                   "white_british [1]", "p_socgrade [3]"))
) + labs(title = "Predicted probability of voting Conservative",
         x = "Perceived difference between London and local economies",
         y = "Predicted probability")

plot_local_belong_logit_random_twoparty <- plot(
  ggpredict(local_belong_logit_random_twoparty, terms = c("londonLocalEcon","belongLocal",
                                                          "white_british [1]", "male [1]"))
) + labs(title = "Predicted probability of voting Conservative",
         x = "Perceived difference between London and local economies",
         y = "Predicted probability")  + 
  guides(colour=guide_legend(title=str_wrap("Local belonging", 7)))
ggsave("drafts/paper1/figures/plot_local_belong_logit_random_twoparty.pdf",
       plot_local_belong_logit_random_twoparty)


plot_local_belong_linear_twoparty <- plot(
  ggpredict(local_belong_linear_twoparty, terms = c("londonLocalEcon","belongLocal",
                                                          "white_british [1]", "male [1]"))
) + labs(title = "Predicted probability of voting Conservative",
         x = "Perceived difference between London and local economies",
         y = "Predicted probability") + 
  guides(colour=guide_legend(title=str_wrap("Local belonging", 7)))


stargazer(local_logit_random_twoparty, 
          belong_logit_random_twoparty, 
          local_belong_logit_random_twoparty,
          type = "latex", header = FALSE, 
          model.numbers = TRUE, 
          dep.var.labels.include = FALSE, 
          dep.var.caption = "Vote choice (0/1, Labour/Conservative)", 
          no.space = TRUE, 
          title = "vote_mods",
          omit = c("p_edlevel", "age","male","p_socgrade","white_british","p_gross_household"),
          covariate.labels = c("Peripheralness", "Local belonging", "Peripheralness * Local belonging"),
          omit.stat = c("aic","bic"),
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.sep.width = "3pt",
          add.lines = list(c("Covariates","Yes","Yes","Yes")))


################################
#REDISTRIBUTION MODELS 
###################################

#london-local - random linear 
redist_local_random <- lmer(data = raw11, redistSelf ~ londonLocalEcon + 
                        p_edlevel + age + male + p_socgrade + white_british + 
                        p_gross_household + (1 | pcon_name))

#london-local - fixed linear
redist_fixed <- felm(data = raw11, redistSelf ~ londonLocalEcon + 
                       p_edlevel + age + male + p_socgrade + 
                       white_british + p_gross_houshold | pcon_name | 0 | pcon_name)

#london-local * belong - random linear 
redist_local_belong_random <- lmer(data = raw11, redistSelf ~ londonLocalEcon*belongLocal + 
                               p_edlevel + age + male + p_socgrade + white_british + 
                               p_gross_household + (1 | pcon_name))

redist_belong_random <- lmer(
  data = raw11, redistSelf ~ belongLocal + p_edlevel + age + male + p_socgrade + 
    white_british + p_gross_household + (1 | pcon_name)
)


#london-local * belong - fixed linear 
redist_fixed <- felm(data = raw11, redistSelf ~ londonLocalEcon*belongLocal + 
                       p_edlevel + age + male + p_socgrade + 
                       white_british + p_gross_houshold | pcon_name | 0 | pcon_name)

#linear cluster-robust models 
clust <- sandwich::vcovCL(linear, raw11$pcon_name)
redist_local_cluster <- lm(data = raw11, redistSelf ~ londonLocalEcon + 
               p_edlevel + age + male + p_socgrade + white_british + 
               p_gross_household)
#cluster by pcon
redist_local_cluster <- coeftest( redist_local_cluster, vcov. = clust )

redist_belong_cluster <- lm(data = raw11, redistSelf ~ belongLocal + 
                                p_edlevel + age + male + p_socgrade + 
                                white_british + p_gross_household)
redist_belong_cluster <- coeftest(redist_belong_cluster, vcov. = clust)

redist_local_belong_cluster <- lm(data = raw11, redistSelf ~ londonLocalEcon*belongLocal + 
                                    p_edlevel + age + male + p_socgrade + 
                                    white_british + p_gross_household)
redist_local_belong_cluster <- coeftest(redist_local_belong_cluster, vcov. = clust)





####################################################
### IMMIGRATION MODELS 
############################################
#london-local - random linear 
immig_local_random <- lmer(data = raw11, immigSelf ~ londonLocalEcon + 
                        p_edlevel + age + male + p_socgrade + white_british + 
                        p_gross_household + (1 | pcon_name))

#london-local - fixed linear
immig_fixed <- felm(data = raw11, immigSelf ~ londonLocalEcon + 
                       p_edlevel + age + male + p_socgrade + 
                       white_british + p_gross_household | pcon_name | 0 | pcon_name)

#london-local * belong - random linear 
immig_local_belong_random <- lmer(data = raw11, immigSelf ~ londonLocalEcon*belongLocal + 
                               p_edlevel + age + male + p_socgrade + white_british + 
                               p_gross_household + (1 | pcon_name))
immig_belong_random <- lmer(
  data = raw11, immigSelf ~ belongLocal + p_edlevel + age + male + 
    p_socgrade + white_british + p_gross_household + (1 | pcon_name)
)



#london-local * belong - fixed linear 
immig_fixed <- felm(data = raw11, immigSelf ~ londonLocalEcon*belongLocal + 
                       p_edlevel + age + male + p_socgrade + 
                       white_british + p_gross_household | pcon_name | 0 | pcon_name)

#belong - random linear 
immig_localbelong_random <- lmer(data = raw11, immigSelf ~ belongLocal + 
                              p_edlevel + age + male + p_socgrade + white_british + 
                              p_gross_household + (1 | pcon_name))

#london-local * belong - fixed linear 
immig_localbelong_fixed <- felm(data = raw11, immigSelf ~ belongLocal + 
                      p_edlevel + age + male + p_socgrade + 
                      white_british + p_gross_houshold | pcon_name | 0 | pcon_name)


immig_belong_cluster <- lm(data = raw11, immigSelf ~ londonLocalEcon*belongLocal + 
                              p_edlevel + age + male + p_socgrade + 
                              white_british + p_gross_household)
immig_belong_cluster <- coeftest(immig_belong_cluster, vcov. = clust)
plot(ggpredict(immig_belong_cluster, terms = c("londonLocalEcon","belongLocal")))

#make predicted values plots 

#stargaze these ones
class(redist_local_random) <- 'lmerMod'
class(redist_belong_random) <- 'lmerMod'
class(redist_local_belong_random) <- 'lmerMod'
class(immig_local_random) <- 'lmerMod'
class(immig_belong_random) <- 'lmerMod'
class(immig_local_belong_random) <- 'lmerMod'

stargazer(redist_local_random, redist_belong_random, 
          redist_local_belong_random, 
          immig_local_random, immig_belong_random, 
          immig_local_belong_random,
          type = "text", header = FALSE, 
          model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.caption = "", 
          no.space = TRUE, 
          omit = c("p_edlevel", "age","male","p_socgrade","white_british","p_gross_household"),
          covariate.labels = c("Peripheralness", "Local belonging", "Peripheralness * Local belonging"),
          omit.stat = c("aic","bic"),
          star.cutoffs = c(0.05, 0.01, 0.001), 
          column.sep.width = "1pt",
          add.lines = list(c("Covariates","Yes","Yes","Yes")),
          column.labels = c("Redist","Redist","Redist","Immig","Immig","Immig"),
          font.size = "small")










plot(ggpredict(immig_local_belong_random, terms = c("londonLocalEcon","belongLocal")))








#Produce region and local lines on the same plot 

mod1_local <- glm(data = raw11, 
                 voteConTwoParty ~ londonLocalEcon*belongLocal + 
                   p_edlevel + age + male + p_socgrade + p_gross_household + 
                   white_british + as.factor(pcon_name), 
                 family = "binomial")

localpred <- ggpredict(mod1_local, terms = c("londonLocalEcon", "belongLocal",
                                             "p_edlevel [3]",
                                             "male [1]", 
                                             "p_socgrade [3]"), 
                       vcov.fun = "vcovCL", 
                       vcov.type = "HC1",
                       vcov.args = list(cluster = raw11$pcon_name))
localpred$group <- "Local"
regionpred <- ggpredict(mod1_region,
                        terms = c("londonRegionEcon", "p_edlevel [3]",
                                  "male [1]",
                                  "p_socgrade [3]"), 
                        type = "re")
regionpred$group <- "Region"
pred <- rbind(localpred, regionpred)
ggplot(pred) + 
  geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, ymax  = conf.high, 
                  fill = group), alpha = 0.3) + 
  geom_line(aes(x = x, y = predicted, color = group)) + 
  theme_minimal()


#################################################
##### BREXIT MODELS
##########################################

#note that there are convergence problems in all of these 
mod_brexit_local_random_logit <- glmer(data = raw11, p_eurefvote ~ londonLocalEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            (1 | pcon_name), family = binomial(link = "logit"))
mod_brexit_belong_random_logit <- glmer(data = raw11, p_eurefvote ~ belongLocal + 
                                 p_edlevel + age + male + p_socgrade + white_british + 
                                 (1 | pcon_name), family = binomial(link = "logit"))
mod_brexit_interact_random_logit <- glmer(data = raw11, p_eurefvote ~ londonLocalEcon*belongLocal + 
                                            p_edlevel + age + male + p_socgrade + white_british + 
                                            (1 | pcon_name), family = binomial(link = "logit"))







plot(ggpredict(mod_brexit_local, terms = "londonLocalEcon")) + 
  ggtitle("Probability of voting leave, by perception of London-local econ diff") + 
  xlab("London-local diff") + ylab('Predicted probability of voting Leave')


plot(ggpredict(mod_brexit_local_belong, 
               terms = c("londonLocalEcon","belongLocal", "male [1]", "white_british [1]"),
               vcov.fun = "vcovCL", vcov.type = "HC1",
               vcov.args = list(cluster = raw11$pcon_name))) + 
  ggtitle("Probability of voting leave, by perception of London-local econ diff") + 
  xlab("London-local diff") + ylab('Predicted probability of voting Leave')

mod_brexit_region <- glmer(data = raw11, p_eurefvote ~ londonRegionEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            p_gross_household + (1 | pcon_name),
                           family = binomial(link = "logit"))
plot(ggpredict(mod_brexit_region, terms = "londonRegionEcon")) + 
  ggtitle("Probability of voting leave, by perception of London-region econ diff") + 
  xlab("London-region diff") + ylab('Predicted probability of voting Leave')

mod_brexit_region_belong <- glmer(data = raw11, p_eurefvote ~ londonRegionEcon*belongRegion + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            (1 | pcon_name), family = binomial(link = "logit"))
brexit_region_belong_plot <- plot(ggpredict(mod_brexit_region_belong, terms = c("londonRegionEcon","belongRegion"))) + 
  ggtitle("Probability of voting leave, by perception of London-region econ diff") + 
  xlab("London-region diff") + ylab('Predicted probability of voting Leave')




###############################################################3##
#### REDISTRIBUTION / IMMIGRATION MODELS
#########################################################3
mod_redist_local <- lmer(data = raw11, 
                         redistSelf ~ londonLocalEcon + p_edlevel + 
                           age + male + p_socgrade + white_british + 
                           (1 | pcon_name))
mod_redist_region <- lmer(data = raw11, redistSelf ~ londonRegionEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            (1 | pcon_name))

#make predicted values plots
plot(ggpredict(mod_redist_local, 
               terms = c("londonLocalEcon", "male [1]",
                         "p_socgrade [3]")))
plot(ggpredict(mod_redist_region, 
               terms = c("londonRegionEcon", "male [1]", 
                         "p_socgrade [3]")))

mod_immig_local <- lmer(data = raw11, 
                         immigSelf ~ londonLocalEcon*belongLocal + p_edlevel + 
                           age + male + p_socgrade + white_british +
                           (1 | pcon_name))
plot(ggpredict(mod_immig_local, 
               terms = c("londonLocalEcon","belongLocal",
                         "male [1]","white_british [1]")))

mod_immig_region <- lmer(data = raw11, immigSelf ~ londonRegionEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            (1 | pcon_name))


###############################################################3##
#### SATDEM MODELS
#########################################################3
mod_satdem_local <- lmer(data = raw11, 
                         satDemUK ~ londonLocalEcon*belongLocal + p_edlevel + 
                           age + male + p_socgrade + white_british + 
                           (1 | pcon_name))
mod_satdem_region <- lmer(data = raw11, satDemUK ~ londonRegionEcon + 
                            p_edlevel + age + male + p_socgrade + white_british + 
                            (1 | pcon_name))


