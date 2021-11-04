library(tidyverse)
library(stargazer)
library(lmerTest)
library(xtable)
library(reshape2)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
df <- read_csv("data/bes/internet_panel/clean_data/wave11_clean.csv")

#create subsets of data 
df <- df %>%
  mutate(localist_only = ifelse(
    belongLocal == 1 & belongRegion == 0 &
      belongMiddleClass == 0 & belongWorkingClass == 0 & 
      belongGroup_5 == 0, 1, 0
  )) 

sum(df$localist_only, na.rm = TRUE) 
#1614 people are localist only 

df_localists_only_and_non_localists <- df %>%
  filter(localist_only == 1 | belongLocal == 0)

df_all_localists <- df %>% filter(belongLocal == 1)

df_localists_vs_everyone_else <- df

demo_models <- function(dat) { 
  
  mod_local <- lmer(data = dat, localist_only ~ p_edlevel +
                               age + male + p_socgrade + white_british +
                               p_gross_household + (1 | pcon))
  mod_local_house <- lmer(data = dat, localist_only ~ p_edlevel +
                                     age + male + p_socgrade + white_british +
                                     p_gross_household +  own_house +
                                     (1 | pcon))
  mod_local_children <- lmer(data = dat, localist_only ~ p_edlevel +
                                        age + male + p_socgrade + white_british +
                                        p_gross_household + children_in_household +
                                        (1 |  pcon))
  class(mod_local) <- "lmerMod"
  class(mod_local_house) <- "lmerMod"
  class(mod_local_children) <- "lmerMod"
  
  stargazer(mod_local, 
            mod_local_house, mod_local_children,
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
  
}

#model for only-localists vs. everyone else
demo_models(dat = df) #results unchanged from main paper

#model for only-localists vs. only-non-localists
demo_models(dat = df_localists_only_and_non_localists) 
#results mostly unchanged from main paper - child effect quite strong

#model for only-localists vs. multiple-belongers
demo_models(dat = df_all_localists)
#only gender, homeowneship, and children remain significant

#print correlation matrix of belonging
cormat <- df %>%
  dplyr::select(belongLocal, belongRegion, 
                belongWorkingClass, belongMiddleClass, belongGroup_5) %>%
  cor(., use = "complete.obs") %>% 
  as_tibble() %>%
  rename(Local = belongLocal, Region = belongRegion, 
         `Middle class` = belongMiddleClass, `Working class` = belongWorkingClass,
         Ethnicity = `belongGroup_5`) %>%
  mutate(variable = c("Local","Region","Working Class","Middle Class","Ethnicity")) %>% 
  dplyr::select(variable, everything()) 

cormat <- df %>% 
  dplyr::select(belongLocal, belongRegion,
                belongWorkingClass, belongMiddleClass, belongGroup_5) %>% 
  rename(Local = belongLocal, Region = belongRegion, 
         `Middle class` = belongMiddleClass, `Working class` = belongWorkingClass,
         Ethnicity = `belongGroup_5` ) %>% 
  as_tibble()
cormat <- cormat %>% 
  cor(., use = 'complete.obs')

melted_cormat <- melt(cormat)
names(melted_cormat) <- c("Var1","Var2","value")


get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_tile(aes(Var2, Var1, fill = round(value, 2)), color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + 
  labs(x = "Variable 1", y = "Variable 2 ") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
  
corrplot <- ggcorrplot(cormat) + 
  ggtitle("Correlation Matrix of Group Belonging")
ggsave( "drafts/paper1/figures/group_corrplot.pdf", corrplot)

#run model on regional belonging
region_mod <- function(dat){
  
  mod_local <- lmer(data = dat, belongRegion ~ p_edlevel +
                    age + male + p_socgrade + white_british +
                    p_gross_household + (1 | pcon))
mod_local_house <- lmer(data = dat, belongRegion ~ p_edlevel +
                          age + male + p_socgrade + white_british +
                          p_gross_household +  own_house +
                          (1 | pcon))
mod_local_children <- lmer(data = dat, belongRegion ~ p_edlevel +
                             age + male + p_socgrade + white_british +
                             p_gross_household + children_in_household +
                             (1 |  pcon))
class(mod_local) <- "lmerMod"
class(mod_local_house) <- "lmerMod"
class(mod_local_children) <- "lmerMod"

stargazer(mod_local, 
          mod_local_house, mod_local_children,
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

}
region_mod(dat = df)
#education not significant, ethnicity is now significant 
#homeownership not signifcant

mod_local_region <- lmer(data = df, belongLocal ~ p_edlevel + belongRegion + 
                           belongWorkingClass + belongMiddleClass + belongGroup_5 + 
                    age + male + p_socgrade + white_british +
                    p_gross_household + (1 | pcon))
class(mod_local_region) <- "lmerMod"
stargazer(mod_local_region, type = "text")

#make belonging plot from understandign soceity data
us <- read_dta("data/understanding_society/bhps/stata/stata13_se/ukhls_w8/h_indresp.dta")

us <- us %>%
  dplyr::select(starts_with("h_ethid"))

us <- us %>%
  rename(
    language_home = h_ethid2,
    language_english = h_ethid3,
    religion_own = h_ethid4a, religion_raised = h_ethid4b,
    region_live = h_ethid5, country_born = h_ethid6,
    region_raised = h_ethid7, color = h_ethid14
  )
us[us < 0] <- NA


# make the belonging plot again, but for the understanding society data
self <- us %>%
  select(-starts_with("h_")) %>%
  pivot_longer(cols = language_home:color, names_to = "id", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(id) %>%
  summarize(
    n = n(), very_fairly = sum(value == 1 | value == 2),
    very = sum(value == 1)
  ) %>%
  mutate(
    prop_important = very_fairly / n,
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
    )
  ) %>%
  filter(id != "religion_raised") %>%
  ggplot() +
  geom_col(aes(x = prop_important, y = var)) +
  xlab("Proportion very/fairly important") +
  ylab("Category") +
  ggtitle("Importance of social groupings to sense of self") +
  theme_minimal()

ggsave("prospectus/figures/ukhls_senseofself.pdf", self)



####################################################################
########## REDISTRIBUTION MODELS
########################################################## 3

red1 <- lmer(data = raw11, redistSelf ~ localEcon + p_gross_household +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red2 <- lmer(data = raw15, redistSelf ~ localEcon + p_gross_household +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red3 <- lmer(data = raw11, redistSelf ~ localEcon + econGenRetro + econPersonalRetro +
               p_gross_household +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red4 <- lmer(data = raw15, redistSelf ~ localEcon + econGenRetro + econPersonalRetro +
               p_gross_household +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
red5 <- lmer(data = raw11, redistSelf ~ localEcon * belongLocal + econGenRetro + econPersonalRetro + p_gross_household +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
class(red1) <- "lmerMod"
class(red2) <- "lmerMod"
class(red3) <- "lmerMod"
class(red4) <- "lmerMod"
class(red5) <- "lmerMod"

stargazer(red1, red2, red3, red4, red5,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c(
            "p_gross_household", "p_edlevel", "age",
            "male", "p_socgrade", "white_british", "Constant"
          ),
          label = "redist_mods",
          omit.stat = c("aic", "bic"),
          no.space = TRUE,
          covariate.labels = c(
            "Local econ", "Local belong", "General econ", "Personal econ",
            "Econ * belong"
          ),
          dep.var.labels = "Redistribution attitudes (0-10, left-right)",
          column.labels = c("2017", "2019", "2017", "2019", "2017")
)

########################################### 3
#### immigration models
######################################################## 33
immig1 <- lmer(data = raw11, immigSelf ~ localEcon + p_gross_household +
                 p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig2 <- lmer(data = raw15, immigSelf ~ localEcon + p_gross_household +
                 p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig3 <- lmer(data = raw11, immigSelf ~ localEcon + econGenRetro + econPersonalRetro +
                 p_gross_household +
                 p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig4 <- lmer(data = raw15, immigSelf ~ localEcon + econGenRetro + econPersonalRetro +
                 p_gross_household +
                 p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
immig5 <- lmer(data = raw11, immigSelf ~ localEcon * belongLocal + econGenRetro + econPersonalRetro + p_gross_household +
                 p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
class(immig1) <- "lmerMod"
class(immig2) <- "lmerMod"
class(immig3) <- "lmerMod"
class(immig4) <- "lmerMod"
class(immig5) <- "lmerMod"

#stargazer
stargazer(immig1, immig2, immig3, immig4, immig5,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c(
            "p_gross_household", "p_edlevel", "age",
            "male", "p_socgrade", "white_british"
          ),
          omit.stat = c("aic", "bic"),
          no.space = TRUE,
          label = "tab:immig_mods",
          covariate.labels = c(
            "Local econ", "Local belong", " General econ", "Personal econ",
            "Local econ * belong"
          ),
          dep.var.labels = "Immigration attitudes (0-10, higher-lower)",
          column.labels = c("2017", "2019", "2017", "2019", "2017")
)
# plot the interaction from the last model
immig_plot <- ggpredict(immig5, terms = c(
  "localEcon", "belongLocal",
  "econGenRetro [3]", "econPersonalRetro [3]"
)) %>%
  plot() +
  labs(
    x = "Local economic evaluation",
    y = "Immigration preferences",
    title = "Predicted immigration attitudes, by local economic evaluations"
  ) +
  guides(colour = guide_legend(title = str_wrap("Local belonging", 7)))
ggsave("drafts/paper1/figures/immig_plot.pdf", immig_plot)

# for people who belong local, better econ = more anti-immigrant
# for people who don't belong local, better econ = less anti-immigrant


#############################
## Context models: Are voters responsive to objective local context?
# mod1_context: includes unemployment, annual change in median income, 20 year house price change
# mod2_context: adds general and personal retrospective evaluations
# unemployment and price change stay significant in both
#############################
mod1_context <- lmer(data = raw11, voteConTwoParty ~ median_per_change_2017 + rate2017 +
                       price_change20yr_scale +
                       p_edlevel + age + male + p_socgrade + white_british + p_gross_household)
mod2_context <- glmer(data = raw11, voteConTwoParty ~ price_change20yr_scale + econGenRetro +
                        econPersonalRetro + rate2017 + median_per_change_2017 +
                        p_edlevel + age + male + p_socgrade + white_british + p_gross_household +
                        (1 | pcon), family = binomial(link = "logit"))

class(mod1_context) <- "lmerMod"
class(mod2_context) <- "lmerMod"
stargazer(mod1_context, mod2_context,
          type = "text",
          omit = c(
            "p_edlevel", "age", "male", "p_socgrade",
            "white_british", "p_gross_household"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels = c(
            "Pct. annual change median income",
            "Unemployment rate",
            "House price 20-year pct. change",
            "General econ eval",
            "Personal econ eval"
          ),
          omit.stat = c("aic", "bic"),
          dep.var.labels = "Vote choice (1-5, left-right)"
)


# LOGIT MODELS, CLUSTERED STANDARD ERRORS AT PCON
mod1glm <- glm(
  data = raw11, voteConTwoParty ~ localEcon + p_gross_household +
    p_edlevel + age + male + p_socgrade + white_british,
  family = binomial(link = "logit")
)
mod2glm <- glm(
  data = raw15, voteConTwoParty ~ localEcon + p_gross_household +
    p_edlevel + age + male + p_socgrade + white_british,
  family = binomial(link = "logit")
)
mod3glm <- glm(
  data = raw11, voteConTwoParty ~ localEcon + p_gross_household +
    econGenRetro + econPersonalRetro +
    p_edlevel + age + male + p_socgrade + white_british,
  family = binomial(link = "logit")
)
mod4glm <- glm(
  data = raw15, voteConTwoParty ~ localEcon + p_gross_household + econGenRetro + econPersonalRetro +
    p_edlevel + age + male + p_socgrade + white_british,
  family = binomial(link = "logit")
)
mod5glm <- glm(
  data = raw11, voteConTwoParty ~ localEcon * belongLocal + econGenRetro + econPersonalRetro +
    p_edlevel + age + male + p_socgrade + p_gross_household + white_british,
  family = binomial(link = "logit")
)
clust1 <- sandwich::vcovCL(mod1glm, raw11$pcon)
mod1glm <- coeftest(mod1glm, vcov. = clust1)
clust2 <- sandwich::vcovCL(mod2glm, raw15$pcon)
mod2glm <- coeftest(mod2glm, vcov. = clust2)
clust3 <- sandwich::vcovCL(mod3glm, raw11$pcon)
mod3glm <- coeftest(mod3glm, vcov. = clust3)
clust4 <- sandwich::vcovCL(mod4glm, raw15$pcon)
mod4glm <- coeftest(mod4glm, vcov. = clust4)
clust5 <- sandwich::vcovCL(mod5glm, raw11$pcon)
mod5glm <- coeftest(mod5glm, vcov. = clust5)




#DIFFERENT METHODS FOR LOGISITIC MODELS 

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
clogit5 <- clogit(data = raw11, vote_con ~ localEcon * belongLocal + econGenRetro + econPersonalRetro +
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british +
                    strata(pcon))
clogit6 <- clogit(data = raw11, vote_con ~ localEcon + belongLocal + econGenRetro + econPersonalRetro +
                    p_edlevel + age + male + p_socgrade + p_gross_household + white_british +
                    strata(pcon))

#stargazer
stargazer(clogit1, clogit3, clogit6, clogit5,
          type = "text",
          omit = c(
            "p_edlevel", "age", "male", "p_socgrade", "p_gross_household",
            "white_british", "Constant"
          ),
          dep.var.caption = "Vote Labour/Con (0/1)",
          dep.var.labels.include = FALSE,
          column.labels = c("2017", "2019", "2017", "2019", "2017", "2017"),
          covariate.labels = c(
            "Local econ", "Local belonging",
            "General econ", "Personal econ", "Local econ * belong"
          ),
          no.space = TRUE,
          label = "tab:vote_mods",
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE,
          model.numbers = TRUE, column.sep.width = "3pt",
          omit.stat = c("wald", "lr", "logrank")
)

