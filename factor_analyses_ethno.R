setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

test <- read_csv("data/bes/internet_panel/clean_data/wave11_clean.csv") 

test <- test %>% 
  mutate(paper = case_when(
    p_paper_read == 1 ~ "The Express", 
    p_paper_read == 2 ~ "Daily Mail", 
    p_paper_read == 3 ~ "Mirror / Daily Record", 
    p_paper_read == 4 ~ "Daily Star", 
    p_paper_read == 5 ~ "The Sun", 
    p_paper_read == 6 ~ "Telegraph", 
    p_paper_read == 7 ~ "FT", 
    p_paper_read == 8 ~ "Guardian", 
    p_paper_read == 9 ~ "Independent", 
    p_paper_read == 10 ~ "Times", 
    p_paper_read == 11 ~ "Scotsman", 
    p_paper_read == 12 ~ "Herald", 
    p_paper_read == 13 ~ "Western Mail", 
    p_paper_read == 14 ~ "Other local", 
    p_paper_read == 15 ~ "Other", 
    p_paper_read == 16 ~ "None" 
    
  )) %>% 
  filter(p_paper_read != 16) %>% 
  mutate(tabloid = case_when(
    p_paper_read %in% c(1, 2, 3, 4, 5) ~ 1, 
    TRUE ~ 0
  ))




pca_dat <- test %>%
  dplyr::select(ethno1, ethno2, ethno3, ethno4, ethno5, ethno6)


#THIS CODE PRODUCES SCORES FROM FACTOR ANALYSIS
#NEED TO JOIN TO DATA FRAME THAT HAS OMITTED NAS ON ETHNO VARIABLES 
analysis <- factanal(x = na.omit(pca_dat), factors = 1,
                     scores = c('regression'))
print(analysis, cutoff = 0.3)


to_join <- test %>%
  filter(!is.na(ethno1) & !is.na(ethno2) &
           !is.na(ethno3) &!is.na(ethno4) &
           !is.na(ethno5) &!is.na(ethno6) )

to_join$cosmo_factor <- analysis$scores

cosmo_mod <- lmer(data = to_join, cosmo_factor ~ belongLocal +
                 p_gross_household + p_edlevel + age + male + p_socgrade + white_british + (1 | pcon))
class(cosmo_mod) <- "lmerMod"
stargazer(cosmo_mod, type = "text")


fit <- princomp(na.omit(pca_dat), cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)





