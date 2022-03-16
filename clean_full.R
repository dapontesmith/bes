library(data.table)
library(tidyverse)
df <- fread("data/bes/internet_panel/bes_wave1_to_wave20.csv")
geo <- fread("data/bes/internet_panel/bes_wave11_msoa_geo.csv")
df <- df %>% 
  filter(wave11 == 1) 
geo <- geo %>% 
  filter(wave11 == 1)
df11 <- df %>%
  dplyr::select(id, ends_with("W11"), gender, starts_with("soc2010"), 
                houseBuildW12, localEconNowW12, localEcon1520YrW12, 
                cutsTooFarNationalW12, cutsTooFarNHSW12, 
                cutsTooFarLocalW12)


names(df11) <- gsub(pattern = "W11", replacement = "", x = names(df11)) 
names(df11) <- gsub(pattern = "W12", replacement = "", x = names(df11)) 

df11 <- df11 %>%
  dplyr::select(id:p_msoa11, gender, soc2010W6W7W8W9, 
                houseBuild, localEconNow, localEcon1520Yr, 
                cutsTooFarNational, cutsTooFarNHS, cutsTooFarLocal, 
                -oslaua, -gor, -p_msoa11, -pcon,
                -soc2010W6W7W8W9, -pano, -country)
df11 <- left_join(df11, geo, by = "id")

names(df11) <- gsub(pattern = "W11", replacement = "", x = names(df11)) 
names(df11) <- gsub(pattern = "W12", replacement = "", x = names(df11)) 
write.csv(df11, "data/bes/internet_panel/bes_wave11_msoa_clean.csv")
