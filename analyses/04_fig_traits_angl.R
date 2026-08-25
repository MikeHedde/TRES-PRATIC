library(scales)

#camembert trait value vs multitrait value en nombre d'occurences
div_db <- read.csv("data/derived-data/div_db.csv") 

pie_data <- div_db %>%
  count(Outcome_type_R2, name = "n") %>%
  mutate(
    prop = n / sum(n),
    label = percent(prop)
   # Outcome_type_R2 = recode(
   #   Outcome_type_R2,
  #    "Trait value" = "Mesure de trait",
   #   "MultiTrait value" = "Indicateur de diversité\nfonctionnelle"
 #   )
  )

ggplot(pie_data, aes(x = "", y = n, fill = Outcome_type_R2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 8
  ) +
  scale_fill_manual(
    values = c(
      "#9ECAE1",
      "#4292C6")) +
  labs(fill = "Type of indicator") +
  theme_void() +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

# camembert trait value vs multitrait value par expérimentation
div_db <- read.csv("data/derived-data/div_db.csv") 

categories <- div_db %>%
  group_by(Study_ID) %>%
  summarise(
    has_single = any(Outcome_type_R2 == "Trait value"),
    has_multiple = any(Outcome_type_R2 == "MultiTrait value"),
    .groups = "drop"
  ) %>%
  mutate(
    categorie_exclusive = case_when(
      has_single & has_multiple ~ "Both",
      has_single ~ "Single trait only",
      has_multiple ~ "Multiple traits only"
    )
  )

counts_pie <- categories %>%
  count(categorie_exclusive) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(round(percentage, 1), "%")
  )

ggplot(counts_pie, aes(x = "", y = n, fill = categorie_exclusive)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 8
  ) +
  scale_fill_manual(
    values = c(
      "#4292C6",
      "#C6DBEF",
      "#9ECAE1"
    )
  ) +
  labs(fill = "Type of indicator") +
  theme_void() +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

#pourcentage d'experimentations utilisant le CWM
(sum(div_db$Outcome_indicator == "CWM"))/(n_distinct(div_db$Study_ID))*100

a <- div_db %>%
  filter(Outcome_type_R2=="Trait value")%>%
  distinct(Study_ID, Outcome_type_R2)
nrow(a) #40 expérimentations ont au moins une mesure de trait (basée sur un seul trait)

b=sum(div_db$Outcome_indicator == "CWM") #11 expérimentations présentent un CWM

b/nrow(a)*100 # 27,5% des expérimentations présentant au moins une mesure de trait utilisent un CWM

c<-div_db%>%
  filter(Outcome_type_R2=="Trait value")

d<-div_db%>%
  filter(Outcome_type_R2=="MultiTrait value")


percents <- db %>%
  select(Study_ID, Outcome_indicator, Trait_set) %>%
  filter(Outcome_indicator == "CWM") %>%
  distinct()

(sum(percents$Trait_set == "Body size"))/(nrow(percents))*100
(sum(percents$Trait_set == "Body size"))/(nrow(percents))*100


#histogramme groupes de traits
trait_grp_db <- read.csv("data/derived-data/trait_grp_db.csv") 

trait_counts <- trait_grp_db %>%
  count(Trait_group, name = "n") %>%
  mutate(
    prop = n / sum(n),
    label = percent(prop)
#    Trait_group = recode(
 #     Trait_group,
  #    "Morphological" = "Morphologique",
   #   "Physiological" = "Physiologique",
    #  "Ecological preference" = "Préférence\nécologique",
     # "Behavioral" = "Comportemental",
      #"Phenological" = "Phénologique"
 #   )
  )

ggplot(trait_counts, aes(x = reorder(Trait_group, -n), y = n)) +
  geom_col(fill = "blue", width = 0.3) +
  geom_text(aes(label = label),
            vjust = -0.3,
            size = 7) +
  labs(
    x = "Traits groups",
    y = "Number of occurrences"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18))

comptes <- db %>%
  filter(Outcome_type_R2 == "MultiTrait value") %>%
  select(Study_ID, Outcome_indicator) %>%
  distinct() 
