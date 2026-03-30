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
  filter(Intervention_R2 %in% c("Tillage management", 
                               "GMO",
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
                                 "Rao Stirling index",
                                 "Functional group richness",
                                 "Functional dispersion (FDis)",
                                 "Functional redundancy",
                                 "Functional divergence (FDiv)"))
#hyp1 rassemble 44 études comparatives et 11 sites, on peut faire une méta-analyse multi-niveaux