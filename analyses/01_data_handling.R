# Dataset
db <- read.csv("data/raw-data/Data base - TRES PRATIC - To export.csv", h = T, sep = ",")

#Base de données finale revue systématique, 1133 lignes
db <- db %>%
  mutate(across(where(is.character), str_squish)) %>% #plus d'espaces avant ou après les textes
  filter(Study_ID != "s_127",                         #on retire l'art 127
         Outcome_type_R1 == "Functional",
         Outcome_type_R2 %in% c("Trait value", "MultiTrait value"),
         Outcome_type_R3 == "Univariate"
         )

#Sous jeu de données synthétique au niveau des articles, 37 lignes
paper_db <- db %>%
  select(Article_ID, Publication_Year, Newspaper) %>%
  distinct()

write.csv(x = paper_db, file = "data/derived-data/paper_db.csv")

#Sous jeu de données synthétique au niveau des expérimentations, 46 lignes
exp_db <- db %>%
  select(Study_ID, Publication_Year, Latitude, Longitude, Newspaper, Study_country) %>%
  distinct() 
  
write.csv(x = exp_db, file = "data/derived-data/exp_db.csv")

#Sous jeu de données synthétique croisant Intervention et Population
PI_db <- db %>%
  select(Study_ID, Intervention_R2, Population_homogenized) %>%
  distinct()

write.csv(x = PI_db, file = "data/derived-data/PI_db.csv")

#Sous jeu de données synthétique croisant Intervention, Population et groupe de traits
PIT_db <- db %>%
  select(Study_ID, Intervention_R2, Population_homogenized, Trait_group) %>%
  distinct()

write.csv(x = PIT_db, file = "data/derived-data/PIT_db.csv")

#Sous jeu de données pour le camembert des indicateurs
div_db <- db %>%
  select(Study_ID, Outcome_indicator, Outcome_type_R2) %>%
  distinct()

write.csv(x = div_db, file = "data/derived-data/div_db.csv")

#Sous jeu de données pour l'hist des groupes de trait
trait_grp_db <- db %>%
  select(Study_ID, Trait_group) %>%
  separate_rows(Trait_group, sep = ",\\s*") %>%
  distinct()

write.csv(x = trait_grp_db, file = "data/derived-data/trait_grp_db.csv")

