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

length(unique(hyp1$Study_ID))
nrow(hyp1)
#hyp1 rassemble 40 études comparatives et 7 sites, on peut faire une méta-analyse multi-niveaux

#Hypothèse 2 : travail du sol et traits corporels
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

length(unique(hyp2$Study_ID))
nrow(hyp2)
#hyp2 rassemble 90 études comparatives et 14 sites, on peut faire une méta-analyse multi-niveaux

#hyp 3 : agriculture organique et CWM
hyp3 <- db %>%
  filter(Intervention_R2 == "Organic agriculture",
         
         Outcome_indicator == "CWM" & Trait_set == "Body size")


