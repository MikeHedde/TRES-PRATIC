# Dataset
db <- read.csv("data/raw-data/raw_db.csv", h = T, sep = ",")

db <- db %>%
  mutate(across(where(is.character), str_squish)) %>% #plus d'espaces avant ou après les textes
  filter(Study_ID != "s_127",  #à enlever quand on aura traité l'art 127
         Outcome_type_R1 == "Functional",
         Outcome_type_R3 == "Univariate") 

#Hypothèse 1 : simplification écologique et diversité fonctionnelle
#tableau qui regroupe toutes les lignes avec intervention simplification et outcome mesure de div fonctionnelle
hyp1 <- db %>%
  filter(Intervention_R2 %in% c("GMO",
                               "Combined practices", 
                               "Landscape complexity", 
                               "Land-use change", 
                               "Crop diversification", 
                               "Agroforestry",
                               "Conservation agriculture"),
         
         Outcome_indicator %in% c("Rao's quadratic diversity",
                                 "Functional richness (FRic)",
                                 "Functional evenness (FEve)",
                                 "Functional Diversity variance (FDvar)",
                                 "Functional diversity (FDis)",
                                 "Multi-trait diversity (scaled)",
                                 "FDmovement",
                                 "FDlocation",
                                 "FDsize",
                                 "Rao Stirling index",
                                 "Functional group richness",
                                 "Functional dispersion (FDis)",
                                 "Functional redundancy",
                                 "Functional divergence (FDiv)"))

length(unique(hyp1$Article_ID))
nrow(hyp1)
#hyp1 rassemble 40 études comparatives et 7 sites, on peut faire une méta-analyse multi-niveaux

#Hypothèse 2 : travail du sol et taille corporelle
hyp2 <- db %>%
  filter(Intervention_R2 %in% c("Tillage management",
                                "Combined practices",
                                "Conservation agriculture",
                                "Residues management"),
         
         Outcome_indicator %in% c("CWM",
                                  "Dry weight", 
                                  "Body size",
                                  "Biomass",   
                                  "Body mass",
                                  "Log of biomass"), 
         
         Outcome_indicator != "CWM" | Trait_set == "Body size")

length(unique(hyp2$Article_ID))
nrow(hyp2)
#hyp2 rassemble 90 études comparatives et 14 sites, on peut faire une méta-analyse multi-niveaux

#hyp 3 : agriculture organique et CWM
hyp3a <- db %>%
  filter(Intervention_R2 == "Organic agriculture",
         Outcome_indicator == "CWM" & Trait_set == "Body size")

length(unique(hyp3a$Study_ID))
nrow(hyp3a)
#hyp3a rassemble 16 études comparatives et 5 sites et papiers, on peut faire une méta-analyse multi-niveaux

hyp3b <- db %>%
  filter(Intervention_R2 == "Organic agriculture",
         Outcome_indicator == "CWM" & Trait_set == "Diet")

length(unique(hyp3b$Article_ID))
nrow(hyp3b)
#hyp3b rassemble 7 études comparatives et 3 sites, on peut faire une méta-analyse multi-niveaux

hyp3c <- db %>%
  filter(Intervention_R2 == "Organic agriculture",
         Outcome_indicator == "CWM" & Trait_set == "Dispersal ability")

length(unique(hyp3c$Article_ID))
nrow(hyp3c)
#hyp3c rassemble 13 études comparatives et 4 sites, on peut faire une méta-analyse multi-niveaux

hyp3d <- db %>%
  filter(Intervention_R2 == "Organic agriculture",
         Outcome_indicator == "CWM" & Trait_set == "Hunting strategy")

length(unique(hyp3d$Article_ID))
nrow(hyp3d)
#hyp3d rassemble 3 études comparatives et 2 sites, on peut faire une méta-analyse multi-niveaux


#hyp 4 : labour et biomasse
hyp4a <- db %>%
  filter(Intervention_R2 == "Tillage management",
         Outcome_indicator %in% c("Biomass", "Log of biomass"),
         Population_homogenized == "Earthworms")

length(unique(hyp4a$Article_ID))
length(unique(hyp4a$Study_ID))
nrow(hyp4a)

hyp4b <- db %>%
  filter(Intervention_R2 == "Tillage management",
         Outcome_indicator %in% c("Biomass", "Log of biomass"))

length(unique(hyp4b$Article_ID))
length(unique(hyp4b$Study_ID))
nrow(hyp4b)

#hyp 5 : diversification de cultures et biomasse
hyp5a <- db %>%
  filter(Intervention_R2 == "Crop diversification",
         Outcome_indicator %in% c("Biomass", "Log of biomass"),
         Population_homogenized == "Earthworms")

length(unique(hyp5a$Article_ID))
length(unique(hyp5a$Study_ID))
nrow(hyp5a)

hyp5b <- db %>%
  filter(Intervention_R2 == "Crop diversification",
         Outcome_indicator %in% c("Biomass", "Log of biomass"))

length(unique(hyp5b$Article_ID))
length(unique(hyp5b$Study_ID))
nrow(hyp5b)


#hyp 6 : intrants
hyp6a <- db %>%
  filter(Intervention_R2 %in% c("Combined practices",
                                "Fertilisers and amendments",
                                "Pest and disease management"),
         
         Outcome_indicator %in% c("Rao's quadratic diversity",
                                  "Functional richness (FRic)",
                                  "Functional evenness (FEve)",
                                  "Functional Diversity variance (FDvar)",
                                  "Functional diversity (FDis)",
                                  "Multi-trait diversity (scaled)",
                                  "FDmovement",
                                  "FDlocation",
                                  "FDsize",
                                  "Rao Stirling index",
                                  "Functional group richness",
                                  "Functional dispersion (FDis)",
                                  "Functional redundancy",
                                  "Functional divergence (FDiv)"))

length(unique(hyp6a$Article_ID))
length(unique(hyp6a$Study_ID))
nrow(hyp6a)

hyp6b <- db %>%
  filter(Intervention_R2 %in% c("Combined practices",
                                "Fertilisers and amendments",
                                "Pest and disease management"),
         
         Outcome_indicator %in% c("CWM",
                                  "Dry weight", 
                                  "Body size",
                                  "Biomass",   
                                  "Body mass",
                                  "Log of biomass"), 
         
         Outcome_indicator != "CWM" | Trait_set == "Body size")

length(unique(hyp6b$Article_ID))
length(unique(hyp6b$Study_ID))
nrow(hyp6b)
