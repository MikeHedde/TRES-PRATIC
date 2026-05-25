##Analyse 1 : effet de l'agriculture biologique par rapport à l'agriculture conventionnelle sur la body size

#install.packages("metafor")

install.packages("remotes")
remotes::install_github("daniel1noble/orchaRd")

###libraries
library(dplyr)
library(metafor)
library(tibble)
#library(lattice)  
#library(lme4)
library(ggplot2)
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
data_bio <- read.csv("data/raw-data/quanti_data.csv", 
                sep = ",", 
                header = T)

dat_bio <- select(
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
  Earth_inversion,
  Depth_between_int_and_comp,
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
data_bio <- dat %>%
  filter(Intervention_R2 == "Organic agriculture",
         Trait_set == "Body size",
         !Article_ID %in% c("s_77")) #on retire car comparateur différent

#### calculate effect sizes
lnRR_bio <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_intervention,
  sd1i = sd_intervention,
  n1i = N_intervention,
  m2i = Mean_comparator,
  sd2i = sd_comparator,
  n2i = N_comparator,
  data = data_bio
)

res <- rma(
  yi,
  vi,
  data = lnRR_bio,
  method = "REML"
)

summary(res)
exp(res$b)
forest(res,
       slab = lnRR_bio$Comparative_study_code
       )

#Ajout effets aléatoires et modérateurs
mod_ML_lnRR_bio <- rma.mv(yi, 
                          vi,
                          random = ~ 1 | Article_ID/Study_ID/Comparative_study_code,
                          method = "REML",
                          data = lnRR_bio
                          )

forest(mod_ML_lnRR_bio,
       slab = lnRR_bio$Comparative_study_code
       )


#model.RS.CTE <- rma.mv(yi, vi,
#                       mods=~Population_homogenized, 
#                       method="REML",
#                       random=~1 | Study_ID, 
#                       data=dat_es)

#rma(yi, vi,
#    random = ~ 1 | Study_ID,
#    mods = ~ Population_homogenized)

#summary(model.RS.CTE)


#funnel(model.RS.CTE)

#regtest(model.RS.CTE)
forest(model.RS.CTE,
       xlab = "Effect size lnRR"
       )

orchard_plot(mod_multilevel_SMD, 
             mod = "1", 
             xlab = "Standardised mean difference (SMD)", 
             group = "Study_ID",  k = TRUE, g = TRUE,
             data = dat2_Midolo_2019) + 
  scale_x_discrete(labels = c("Overall effect (meta-analytic lnRR)"))

#funnel(res)

#pour visualiser les résultats d'une méta-régression 
orchard_plot(mod_MLMR_lnRR_trait, 
             mod = "trait", 
             xlab = "Effect size lnRR", 
             group = "Study_ID",  k = TRUE, g = TRUE, trunk.size = 1.5, 
             data = dat2_Midolo_2019) + 
  scale_x_discrete(labels = c("SLA","Pmass","Nmass","Narea","LMA","LA","dC13"))





#1.B : 
#data_ma_1B <- dat %>%
#  filter(Intervention_R2 == "Organic agriculture",
#         Trait_set == "Diet")

#1.C : 
#data_ma_1C <- dat %>%
#  filter(Intervention_R2 == "Organic agriculture",
#         Trait_set == "Dispersal ability")

#1.D : 
#data_ma_1D <- dat %>%
#  filter(Intervention_R2 == "Organic agriculture",
#         Trait_set == "Hunting strategy")

#on met tout ensemble et on sépare dans le orchard
p <- orchaRd::orchard_plot(
  res_2,
  group = Trait_set,
  xlab = "Log response ratio",
  transfm = "none",
  twig.size = 0.5,
  trunk.size = 1
)
