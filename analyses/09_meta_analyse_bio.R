##Analyse 1 : effet de l'agriculture biologique par rapport à l'agriculture conventionnelle sur la CWM body size

###Import data
data_bio <- read.csv("data/raw-data/Data base - TRES PRATIC - quanti_data.csv", 
                sep = ",", 
                header = T)

###Create data base
data_bio <- select(
  data_bio,
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

###Fixed effects model (ne prend pas en compte l'hétérogénéité due aux différents niveaux)
mod_FE_lnRR_bio <- rma(
  yi,
  vi,
  method = "EE",
  data = lnRR_bio
)



png(
  "Figures/MA_bio/forest_FE_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

forest(
  mod_FE_lnRR_bio,
  slab = lnRR_bio$Comparative_study_code
)

dev.off() #pour fermer le png


png(
  "Figures/MA_bio/funnel_FE_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

funnel(mod_FE_lnRR_bio)

dev.off()

regtest(mod_FE_lnRR_bio)

###Multi level meta analytic model
mod_ML_lnRR_bio <- rma.mv(yi, 
                          vi,
                          random = ~ 1 | Article_ID/Study_ID/Comparative_study_code,
                          method = "REML",
                          data = lnRR_bio
                          )
png(
  "Figures/MA_bio/forest_ML_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

forest(mod_ML_lnRR_bio,
       slab = lnRR_bio$Comparative_study_code
       )

dev.off()

png(
  "Figures/MA_bio/funnel_ML_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

funnel(mod_ML_lnRR_bio)

dev.off()

###Critical appraisal : Testing bias with meta-regression
###Dans le modèle à effets fixes
mod_FEMR_lnRR_bias_bio <- rma(yi, 
                              vi,
                              mods = ~ Overall.score,
                              method = "EE",
                              data = lnRR_bio
                              )

png(
  "Figures/MA_bio/forest_FEMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

forest(mod_FEMR_lnRR_bias_bio,
       slab = lnRR_bio$Comparative_study_code)

dev.off()

png(
  "Figures/MA_bio/regplot_FEMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

regplot(mod_FEMR_lnRR_bias_bio) #affiche la droite de régression

dev.off()

png(
  "Figures/MA_bio/funnel_FEMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

funnel(mod_FEMR_lnRR_bias_bio)

dev.off()


###Dans le modèle multi-niveaux (avec effets aléatoires)
mod_MLMR_lnRR_bias_bio <- rma.mv(yi, 
                             vi,
                             mods = ~ Overall.score,
                             random = ~ 1 | Article_ID/Study_ID/Comparative_study_code,
                             method = "REML",
                             test = "t",
                             data = lnRR_bio,
                             sparse = TRUE
                             )

png(
  "Figures/MA_bio/forest_MLMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

forest(mod_MLMR_lnRR_bias_bio,
       slab = lnRR_bio$Comparative_study_code)

dev.off()

png(
  "Figures/MA_bio/regplot_MLMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

regplot(mod_MLMR_lnRR_bias_bio) #affiche la droite de régression

dev.off()

png(
  "Figures/MA_bio/funnel_MLMR_bio.png",
  width = 9.2,
  height = 6.6,
  units = "in",
  res = 450,
  bg = "white"
)

funnel(mod_MLMR_lnRR_bias_bio)

dev.off()



#pour visualiser les résultats d'une méta-régression 
#orchard(mod_MLMR_lnRR_trait, 
#             mod = "trait", 
#             xlab = "Effect size lnRR", 
#             group = "Study_ID",  k = TRUE, g = TRUE, trunk.size = 1.5, 
#             data = dat2_Midolo_2019) + 
#  scale_x_discrete(labels = c("SLA","Pmass","Nmass","Narea","LMA","LA","dC13"))

#on met tout ensemble et on sépare dans le orchard
#p <- orchaRd::orchard_plot(
#  res_2,
#  group = Trait_set,
#  xlab = "Log response ratio",
#  transfm = "none",
#  twig.size = 0.5,
#  trunk.size = 1
#)
