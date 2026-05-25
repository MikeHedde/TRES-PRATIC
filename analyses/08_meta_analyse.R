###Analyse 2 : effet du labour sur la biomasse des vers de terre################

###Libraries
library(metafor)
library(orchaRd)

###Import data
data_labour <- read.csv("data/raw-data/quanti_data.csv", 
                sep = ",", 
                header = T)

###Create data base
data_labour <- select(
  data_labour,
  Article_ID, 
  Study_ID,
  Comparative_study_code,
  Population_studied,
  Intervention_R2,
  Intervention_R3,
  Comparator,
  Outcome_indicator,
  Outcome_metric,
  Trait_set,
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
) %>%
  filter(Intervention_R2 == "Tillage management",
         !Intervention_R3 %in% c("Mulch sowing"),
         Population_studied == "Earthworms")

####Calculate effect sizes
lnRR_labour <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_comparator, #intervention et comparateur inversés
  sd1i = sd_comparator,
  n1i = N_comparator,
  m2i = Mean_intervention,
  sd2i = sd_intervention,
  n2i = N_intervention,
  data = data_labour
  )

###Fixed effects model (ne prend pas en compte l'hétérogénéité due aux différents niveaux)
mod_FE_lnRR_labour <- rma(
  yi,
  vi,
  method = "EE",
  data = lnRR_labour
  )

forest(mod_FE_lnRR_labour,
       slab = lnRR_labour$Comparative_study_code)

#funnel(res_2)
#regtest(res_2)


###Multi level meta analytic model
mod_ML_lnRR_labour <- rma.mv(yi, 
                             vi,
                             random = list(
                               ~ 1 | Article_ID,
                               ~ 1 | Study_ID,
                               ~ 1 | Comparative_study_code
                             ),
                             method = "REML",
                             data = lnRR_labour
                             )

mod_ML_lnRR_labour_bis <- rma.mv(yi, 
                             vi,
                             random = ~ 1 | Article_ID/Study_ID/Comparative_study_code,
                             method = "REML",
                             data = lnRR_labour
                             )

forest(mod_ML_lnRR_labour_bis,
      slab = lnRR_labour$Comparative_study_code)

###Explaining variance with meta-regression
mod_MLMR_lnRR_depth <- rma.mv(yi, 
                              vi,
                              mods = ~ Depth_between_int_and_comp,
                              random = ~ 1 | Article_ID/Study_ID/Comparative_study_code,
                              method = "REML",
                              test = "t",
                              data = lnRR_labour,
                              sparse = TRUE
                              )
                                 
forest(mod_MLMR_lnRR_depth,
       slab = lnRR_labour$Comparative_study_code)

regplot(mod_MLMR_lnRR_depth) #affiche la droite de régression


#ggplot(dat, aes(x = yi, y = study)) +
#  geom_vline(xintercept = 0, linetype = "dashed") +
#  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0) +
#  geom_point() +
#  facet_grid(population_homogenized ~ ., scales = "free_y", space = "free_y") +
#  labs(x = "Effect size", y = NULL) +
#  theme_bw() +
#  theme(
#    strip.text.y = element_text(angle = 0, face = "bold"),
#    panel.spacing = unit(0.8, "lines")
 # )


#Visualisation des figures

p <- orchaRd::orchard_plot(
  
  model.2.mod.log,
  
  mod = "population_homogenized",
  
  group = "article_id",
  
  xlab = "Log response ratio",
  
  transfm = "none",
  
  twig.size = 0.5,
  
  trunk.size = 1)

p