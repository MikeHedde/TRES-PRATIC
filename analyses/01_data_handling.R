# Dataset
db <- read.csv("data/raw-data/raw_db.csv", h = T, sep = ",")

#Sous jeu de données synthétique au niveau des articles
paper_level_db <- db %>%
  select(Study_ID, Publication_Year, Newspaper, Study_country) %>%
  distinct() %>%
  filter(Study_ID != "s_127") #à enlever quand on aura traité l'art 127
  
write.csv(x = paper_level_db, file = "data/derived-data/paper_level_db.csv")

#Sous jeu de données synthétique croisant Intervention et Population
PI_db <- db %>%
  select(Study_ID, Intervention_R2, Population_homogenized, Trait_group) %>%
  distinct() %>%
  filter(Study_ID != "s_127")%>% #à enlever quand on aura traité l'art 127
  mutate(Intervention_R2 = recode(Intervention_R2,
                                  "Organic agriculture " = "Organic agriculture",
                                  "Agroforestry " = "Agroforestry"))

write.csv(x = PI_db, file = "data/derived-data/PI_db.csv")
