#Noah Daponte-Smith 
#Code submission for EDU S043 final project
#Takes in cleaned dataset and produces all tables and figures in the paper 

#Not sure I need all these packages but here we go! 
library(tidyverse)
library(lmerTest)
library(broom)
library(arm)
library(stargazer)
library(ordinal)
library(brms)
library(parallel)
#Clear workspace - BEWARE
rm(list = ls())

# call this once to distribute chains across cpu cores:

options(mc.cores=parallel::detectCores())

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
#read in data and select relevant variables 
dat <- read_csv("bes/internet_panel/wave_20_clean.csv")
dat <- dat %>% 
  mutate(vote_choice = case_when(
    vote == 1 ~ "Brexit/Ukip",
    vote == 2 ~ "Tory", 
    vote == 3 ~ "Lib Dem / Change UK",
    vote == 4 ~ "Labour", 
    vote == 5 ~ "Greens / Plaid Cymru"
  )) 

names(dat) <- str_remove(names(dat), "%")

#brms wants this to be a factor
#dat$vote <- as.factor(dat$vote)

#scale gross_household_mid
dat$gross_household_mid_scale <- scale(dat$gross_household_mid)

#try a linear model 
linear_edlevel_region <- lmer(data = dat,
               vote ~ p_edlevel + is_white + 
                 age  + gender + gross_household_mid_scale + 
                 (1 + p_edlevel | region))
ndat_educ <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                         region =  unique(dat$region),
                         is_white = 1, #white  
                         age = 40, #this is median age in Britain
                         gender = 2, 
                         gross_household_mid_scale = median(dat$gross_household_mid_scale, na.rm = T))
#omit NAs
ndat_educ <- na.omit(ndat_educ)
#predict
probs <- predict(linear_edlevel_region, newdata = ndat_educ)
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




#try a logistic model
logit_edlevel_region <- glmer(data = dat, 
                    family = binomial(link = "logit"), 
                    vote_con ~ p_edlevel + age + gender + 
                    p_gross_household + PopWhiteConst + 
                      ConstPercentChangeFiveYr + 
                    (1 + p_edlevel | region))

#get median percent change in house prices by region 
#get median popwhite by region 
medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE))

#create new dat for predicting - we hold a bunch at median here 
ndat_edlevel <- expand.grid(p_edlevel = unique(dat$p_edlevel),
                    region =  unique(dat$region),
                    #is_white = 1, #white  
                    age = 40, #this is median age in Britain
                    gender = 1, #male
                    p_gross_household = median(dat$p_gross_household, na.rm = T))
#omit NAs
ndat_edlevel <- na.omit(ndat_edlevel) %>% as_tibble() %>% 
  left_join(., medians, by = "region")
#predict
ndat_edlevel$predicted <- predict(logit_edlevel_region, newdata = ndat_edlevel,type = "response")


#create plot 
educ_plot <- ggplot(ndat_edlevel) + 
  geom_line(aes(x = p_edlevel, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()

ggsave(educ_plot, filename = "educ_plot.jpeg")

#do this again with income 

#try a logistic model
logit_income_region <- glmer(data = dat, 
                              family = binomial(link = "logit"), 
                              vote_con ~ p_edlevel + age + gender + 
                                p_gross_household + PopWhiteConst + 
                                ConstPercentChangeFiveYr + 
                                (1 + p_gross_household | region))

ndat_income <- expand.grid(p_edlevel = 3, #A-level, but not undergraduate degree
                         region =  unique(dat$region),
                         is_white = 1, #white  
                         age = 40, #this is median age in Britain
                         gender = 1, #male 
                         p_gross_household = unique(dat$p_gross_household))

medians <- dat %>% 
  group_by(region) %>% 
  summarize(ConstPercentChangeFiveYr = median(ConstPercentChangeFiveYr, na.rm = TRUE),
            PopWhiteConst = median(PopWhiteConst, na.rm = TRUE))

ndat_income <- na.omit(ndat_income) %>% as_tibble() %>% 
  left_join(.,medians, by = "region")
ndat_income$predicted <- predict(logit_income_region, newdata = ndat_income, type = "response")

#create plot 
income_plot <- ggplot(ndat_income) + 
  geom_line(aes(x = p_gross_household, y = predicted, color = region)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Household income') + 
  ylab("Predicted probability of voting Conservative") + 
  theme_minimal()


########################################
# CREATE RANDOM SLOPE PLOT
###########################
#create newdata - all local authorities this time 
testdat <- expand.grid(median = median(dat$median, na.rm = T), 
                       p_edlevel = unique(dat$p_edlevel),
                       oslaua =  unique(dat$oslaua),
                       is_white = 1,
                       age = median(dat$age, na.rm = T), 
                       gender = 2, 
                       p_gross_personal = median(dat$p_gross_personal, na.rm = T)) %>% 
  na.omit() 
#predict 
probs <- predict(brms_fit, newdata = testdat, type = "response", allow_new_levels = TRUE)
probs <- probs %>% as_tibble() 

#this is weird rearranging for facet_Wrap purposes
newdatTory <- testdat
#get conservatives 
newdatTory$prob <- probs $`P(Y = 2)`
newdatTory$party <- "Tory"
newdatLabour <- testdat
#get labour 
newdatLabour$prob <- probs $`P(Y = 4)`
newdatLabour$party <- "Labour"

#make plot 
slope_plot <- rbind(newdatTory, newdatLabour) %>% 
  as_tibble() %>% 
  ggplot(.) + 
  geom_line(aes(x = p_edlevel, y = prob, group = oslaua), alpha = 0.35, size = 0.01) + 
  facet_wrap(~party) +
  xlab("Education level") + 
  ylab("Predicted probability of vote") + 
  ggtitle("Education vs. vote choice, by local authority") +
  theme_minimal()















#run model - this takes 6.5 hours - so BEWARE BEFORE RUNNING 
brms_fit <- brm(
  formula = vote ~ unemp*p_edlevel + median*p_gross_personal + 
    is_white + age + gender + 
    (1 + p_edlevel | oslaua), 
  data = dat, 
  family = cumulative("logit"),
  cores = 4, 
  iter = 10000
)
#save the model so we don't have to run it again later
save(brms_fit, file = "C:/Users/dapon/Dropbox/Harvard/G3/stat151/final_project/data/brms_fit_big.rda")


#load model 
load("data/brms_fit_big.rda")



#make summary table of covariates 
tab <- dat %>% 
  ungroup() %>% 
  #add list of relevant variables here 
  dplyr::select(median, is_white, p_edlevel, p_gross_personal, age, gender, vote) %>% 
  #rename for nice formatting 
  rename(`Is white` = is_white, 
         `Median income` = median,
         Education = p_edlevel, 
         Income = p_gross_personal, 
         Age = age,
         `Gender (1 = male)` = gender) %>% 
  #get rid of NAs, since this is what the regression does 
  na.omit() %>% 
  as.data.frame()
stargazer(tab, 
          title = "Summary table of covariates")  


###############################################
# CREATE EDUCATION PLOT 
########
#get median random intercept oslaua
ranefs <- ranef(brms_fit)$oslaua %>% as_tibble()
ints <- ranefs$Estimate.Intercept
median_int <- median(ints)
ranefs$dist_Int_median <- abs(ranefs$Estimate.Intercept - median_int)
ranefs$oslaua <- rownames(ranef(brms_fit)$oslaua)
rownummed <- which(ranefs$dist_Int_median == min(ranefs$dist_Int_median))
median_oslaua <- rownames(ranef(brms_fit)$oslaua)[rownummed][1]

#create new dat for predicting - we hold a bunch at median here 
ndat <- expand.grid(median = median(dat$median, na.rm = T), 
                    p_edlevel = unique(dat$p_edlevel),
                    oslaua =  median_oslaua,
                    is_white = (unique(dat$is_white)), 
                    age = median(dat$age, na.rm = T), 
                    gender = 2, 
                    p_gross_personal = median(dat$p_gross_personal, na.rm = T))
#omit NAs
ndat <- na.omit(ndat)
#predict
probs <- predict(brms_fit, newdata = ndat, type = "response", 
                 probs = c(0.025, 0.975), se.fit = TRUE)
#rearrange data for graphing - this is a bit roundabout
ndatBrexit <- ndat
ndatTory <- ndat
ndatCenter <- ndat
ndatLabour <- ndat
ndatLeft <- ndat
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

#change variable type for graphing
test$White <- as.factor(test$is_white)

#create plot 
educ_plot <- ggplot(test) + 
  geom_line(aes(x = p_edlevel, y = prob, color = Party, linetype = White)) + 
  ggtitle("Predicted probabilities of vote choice") + 
  xlab('Education level') + 
  ylab("Predicted probability") + 
  theme_minimal()

ggsave(educ_plot, filename = "educ_plot.jpeg")

########################################
# CREATE RANDOM SLOPE PLOT
###########################
#create newdata - all local authorities this time 
testdat <- expand.grid(median = median(dat$median, na.rm = T), 
                       p_edlevel = unique(dat$p_edlevel),
                       oslaua =  unique(dat$oslaua),
                       is_white = 1, 
                       age = median(dat$age, na.rm = T), 
                       gender = 2, 
                       p_gross_personal = median(dat$p_gross_personal, na.rm = T)) %>% 
  na.omit() 
#predict 
probs <- predict(brms_fit, newdata = testdat, type = "response", allow_new_levels = TRUE)
probs <- probs %>% as_tibble() 

#this is weird rearranging for facet_Wrap purposes
newdatTory <- testdat
#get conservatives 
newdatTory$prob <- probs $`P(Y = 2)`
newdatTory$party <- "Tory"
newdatLabour <- testdat
#get labour 
newdatLabour$prob <- probs $`P(Y = 4)`
newdatLabour$party <- "Labour"

#make plot 
slope_plot <- rbind(newdatTory, newdatLabour) %>% 
  as_tibble() %>% 
  ggplot(.) + 
  geom_line(aes(x = p_edlevel, y = prob, group = oslaua), alpha = 0.35, size = 0.01) + 
  facet_wrap(~party) +
  xlab("Education level") + 
  ylab("Predicted probability of vote") + 
  ggtitle("Education vs. vote choice, by local authority") +
  theme_minimal()
ggsave(slope_plot, filename = "slope_plot.jpeg")


##################################3
#COEFFICIENT PLOT
############

#get fixed effects from model 
fixed <- fixef(brms_fit) %>% as_tibble()
#assign terms 
fixed$term <- rownames(fixef(brms_fit))
#make variable for significance 
fixed$is_sig <- ifelse(
  (fixed$Q2.5 > 0 & fixed$Q97.5 > 0) | 
    (fixed$Q2.5 < 0 & fixed$Q97.5 < 0), 1, 0)

#filter out intercept terms 
fixed <- fixed %>% 
  filter(str_detect(term, "Intercept") == FALSE) %>% 
  #change variable names 
  mutate(term = case_when(
    term == "median" ~ "Median income", 
    term == "p_edlevel" ~ "Education", 
    term == "p_gross_personal" ~ "Income",
    term == "is_white" ~ "White", 
    term == "age" ~ "Age",
    term == "gender" ~ "Gender",
    term == "median:p_edlevel" ~ "Median income * Education", 
    term == "median:p_gross_personal" ~ "Median income * Income"
  )) %>% 
  mutate(Signif = as.factor(is_sig))

#create plot 
coef_plot <- fixed %>% 
  ggplot(.) + 
  geom_point(aes(x = term, y = Estimate, color = Signif)) + 
  geom_linerange(aes(x = term, ymin = Q2.5, ymax = Q97.5, color = Signif)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  coord_flip() + 
  ggtitle("Coefficients from multilevel estimation") +
  xlab("Variable") + ylab("Estimate")


ggsave(coef_plot, filename = "coef_plot.jpeg")


#create crosstab 
cro(dat$p_edlevel, dat$vote_choice) %>% 
  xtable(digits = 0,
         label = "tab:educ_vote_crosstab")

#####
#get out ICC 
tau2 <- brms::VarCorr(brms_fit)[[1]]$sd[1]^2

#level 1 variance is equal to pi^2 / 3, variance of the standard logistic distribution 

#we can estimate icc using the tau2 / (tau2 / level 1 variance) formula
level1var <- (pi^2)/3
icc <- tau2 / (tau2 + level1var)


#produce trace and posterior plots 
trace1 <- plot(brms_fit, plot = F, fixed = TRUE)[[1]] 
trace2 <- plot(brms_fit, plot = F, fixed = TRUE)[[2]] 
trace3 <- plot(brms_fit, plot = F, fixed = TRUE)[[3]] 

#save plots 
ggsave(trace1, file = "trace1.jpeg")
ggsave(trace2, file = "trace2.jpeg")
ggsave(trace3, file = "trace3.jpeg")


################################33
##produce appendix figure - number of observations per group
n_obs <- dat %>% 
  dplyr::select(median, oslaua, p_edlevel, p_gross_personal, 
                gender, age, is_white, vote) %>% 
  na.omit() %>%
  group_by(oslaua) %>% 
  summarize(nobs = n()) %>% 
  as_tibble() %>%
  ggplot() + 
  geom_histogram(aes(x = nobs), bins = 40) + 
  ggtitle("Distribution of observations per group") + 
  xlab("Number of observations") + ylab("Count") + 
  theme_minimal()

ggsave(n_obs, filename = 'n_obs.jpeg')



######################33
#run cluster-robust model
linear <- lm(data = dat, as.numeric(vote) ~ p_edlevel*median + median*p_gross_personal + 
               age + gender + is_white)
#cluster by oslaua
vcov_clust <- sandwich::vcovCL(linear, dat$oslaua)
logit_clust <- coeftest( linear, vcov. = vcov_clust )
stargazer(logit_clust, 
          title = "Linear cluster-robust model",
          label = "tab:cluster_robust",
          no.space = TRUE,
          covariate.labels = c("Education","Median income",
                               "Personal income","Age",
                               "Gender","Is White","Education * Median",
                               "Personal income * Median"),
          dep.var.labels = "Vote choice (1 = far right, 5 = far left)")
