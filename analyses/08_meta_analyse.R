#install.packages("metafor")

###libraries
library(dplyr)
library(metafor)
library(tibble)
#library(lattice)  
#library(lme4)
#library(ggplot2)
#library(mgcv)
#library(maptools)
#library(sp)
#library(ggmap)
#library(rgdal)
#library(doBy)
#library(raster)
#library(ClimClass)
#library(gdata)


###Import data
dat <- read.csv("data/raw-data/quanti_data.csv", 
                sep = ",", 
                header = T)
  
dat <- select(
  dat,
  Article_ID, 
  Study_ID,
  Comparative_study_code,
  Population_studied,
  Population_homogenized,
  Intervention_R2,
  Intervention_R3,
  Intervention_copy_paste,
  Comparator,
  Outcome_indicator,
  Outcome_metric,
  Trait_set,
  Mod_country,
  Latitude,
  Longitude,
  Mod_soil_type,
  Mod_time_period_of_sampling,
  Mod_crop_species,
  Mod_practice,
  Overall.score,
  Mean_intervention,
  Type_variation_intervention,
  sd_intervention,
  N_intervention,
  Mean_comparator,
  Type_variation_comparator,
  sd_comparator,
  N_comparator
  ) 

###Analyse 1 : effet de l'agriculture biologique sur le CWM de la faune du sol####
###1.A 
#data base
data_ma_1A <- dat %>%
  filter(Intervention_R2 == "Organic agriculture",
         Trait_set == "Body size",
         !Article_ID %in% c("s_77")) #test sans les articles qui vont dans l'autre sens

#### calculate effect sizes
dat_es <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_intervention,
  sd1i = sd_intervention,
  n1i = N_intervention,
  m2i = Mean_comparator,
  sd2i = sd_comparator,
  n2i = N_comparator,
  data = data_ma_1A
)

res <- rma(
  yi,
  vi,
  data = dat_es,
  method = "REML"
)

summary(res)
exp(res$b)
forest(res)

funnel(res)
regtest(res)


#Ajout effets aléatoires et modérateurs
#rma(yi, vi,
#    random = ~ 1 | Article_ID,
#    mods = ~ Climate)

#summary(res_mod)




#1.B : 
data_ma_1B <- dat %>%
  filter(Intervention_R2 == "Organic agriculture",
         Trait_set == "Diet")

#1.C : 
data_ma_1C <- dat %>%
  filter(Intervention_R2 == "Organic agriculture",
         Trait_set == "Dispersal ability")

#1.D : 
data_ma_1D <- dat %>%
  filter(Intervention_R2 == "Organic agriculture",
         Trait_set == "Hunting strategy")





###Analyse 2 : effet du labour sur la biomasse des vers de terre################
#data base
data_ma_2 <- dat %>%
  filter(Intervention_R2 == "Tillage management",
         !Intervention_R3 %in% c("mulch sowing"))

#### calculate effect sizes
dat_es <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_intervention,
  sd1i = sd_intervention,
  n1i = N_intervention,
  m2i = Mean_comparator,
  sd2i = sd_comparator,
  n2i = N_comparator,
  data = data_ma_2
)

res <- rma(
  yi,
  vi,
  data = dat_es,
  method = "REML"
)

summary(res)
exp(res$b)
forest(res)

funnel(res)
regtest(res)


#Ajout effets aléatoires et modérateurs
#rma(yi, vi,
#    random = ~ 1 | Article_ID,
#    mods = ~ Climate)

#summary(res_mod)


