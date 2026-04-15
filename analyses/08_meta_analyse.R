#install.packages("metafor")
library(metafor)

dat <- read.csv("data/raw-data/quanti_data.csv", sep = ",", header = T)

#Analyse 1 : effet de l'agriculture biologique sur le CWM de la faune du sol####
#1.A : 

data_MA <- dat %>%
  select(Article_ID, 
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
         N_comparator) %>%
  filter(Intervention_R2 == "Organic agriculture")

dat_es <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_intervention,
  sd1i = sd_intervention,
  n1i = N_intervention,
  m2i = Mean_comparator,
  sd2i = sd_comparator,
  n2i = N_comparator,
  data = data_MA
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




#rma(yi, vi,
#    random = ~ 1 | Article_ID,
#    mods = ~ Climate)

#summary(res_mod)

#funnel(res)
#regtest(res)

