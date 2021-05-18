library(tidyverse)
library(lfe)
library(lmerTest)
library(stargazer)

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

raw11 <- read_csv("data/bes/internet_panel/clean_data/wave11_clean.csv")
raw15 <- read_csv("data/bes/internet_panel/clean_data/wave15_clean.csv")

dat15 <- raw15 %>% 
  filter(country.y %in% c("England","Wales"))
dat11 <- raw11 %>% filter(country %in% c("England","Wales"))
eng11 <- raw11 %>% filter(country == "England")


####basic local Econ analyses, plus interaction with belongLoacl
mod1 <- lmer(data = raw11, vote_spectrum ~ localEcon + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon_name))
mod2 <- lmer(data = raw15, voteSpectrum ~ localEcon +
               p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon_name))
mod3 <- lmer(data = raw11, vote_spectrum ~ localEcon + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon_name))
mod4 <- lmer(data = raw15, voteSpectrum ~ localEcon + econGenRetro + econPersonalRetro + 
             p_edlevel + age + male + p_socgrade +  white_british + (1 | pcon_name))
mod5 <- lmer(data = raw11, vote_spectrum ~ localEcon*belongLocal + econGenRetro + econPersonalRetro + 
               p_edlevel + age + male + p_socgrade +  white_british +  (1 | pcon_name))
class(mod1) <- "lmerMod" ; class(mod2) <- "lmerMod" ; class(mod3) <- "lmerMod" 
class(mod4) <- "lmerMod" ; class(mod5) <- "lmerMod"


stargazer(mod1, mod2, mod3, mod4, mod5, type  = "latex",
          dep.var.caption = "Vote choice (1-5, right-left)", 
          dep.var.labels.include = FALSE,
          column.labels = c("2017","2019","2017","2019","2017"),
          no.space = TRUE, 
          omit = c("p_edlevel","age","male","p_socgrade","white_british"),
          covariate.labels = c("Local econ eval", "Local belonging",
                               "General econ retro", "Personal econ retro",
                               "Local econ * Local belonging"),
          title = "local_econ_mod",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("aic","bic"),
          header = FALSE,
          model.numbers = TRUE,
          column.sep.width = "3pt")


########################33
#### LONDON ECON MODELS
mod6 <- lmer(data = raw11, vote_spectrum ~ londonLocalEcon + 
             p_edlevel + age + male + p_socgrade + white_british + (1 | pcon_name))
mod7 <- lmer(data = raw15, voteSpectrum ~ londonLocalEcon +
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon_name))
mod10 <- lmer(data = raw11, vote_spectrum ~ londonLocalEcon*belongLocal + 
               p_edlevel + age + male + p_socgrade + white_british + (1 | pcon_name))
mod11 <- lmer(data = eng11, vote_spectrum ~ londonLocalEcon*belongLocal + 
                p_edlevel + age + male + p_socgrade + white_british + (1 | pcon_name))

class(mod6) <- "lmerMod" ; class(mod7) <- "lmerMod" ; class(mod8) <- "lmerMod" 
class(mod11) <- "lmerMod" ; class(mod10) <- "lmerMod"

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





