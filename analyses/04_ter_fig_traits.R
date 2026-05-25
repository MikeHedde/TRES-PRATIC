library(scales)

#camembert trait value vs multitrait value
div_db <- read.csv("data/derived-data/div_db.csv") 

pie_data <- div_db %>%
  count(Outcome_type_R2, name = "n") %>%
  mutate(
    prop = n / sum(n),
    label = percent(prop),
    Outcome_type_R2 = recode(
      Outcome_type_R2,
      "Trait value" = "Mesure de trait",
      "MultiTrait value" = "Indicateur de diversité\nfonctionnelle"
    )
  )
    
ggplot(pie_data, aes(x = "", y = n, fill = Outcome_type_R2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "#9ECAE1",
      "#4292C6")) +
  labs(fill = "Type d'indicateur") +
  theme_void()


#histogramme groupes de traits
trait_grp_db <- read.csv("data/derived-data/trait_grp_db.csv") 

trait_counts <- trait_grp_db %>%
  count(Trait_group, name = "n") %>%
  mutate(
    prop = n / sum(n),
    label = percent(prop),
    Trait_group = recode(
      Trait_group,
      "Morphological" = "Morphologique",
      "Physiological" = "Physiologique",
      "Ecological preference" = "Préférence écologique",
      "Behavioral" = "Comportemental",
      "Phenological" = "Phénologique"
    )
  )

ggplot(trait_counts, aes(x = reorder(Trait_group, -n), y = n)) +
  geom_col(fill = "blue", width = 0.3) +
  geom_text(aes(label = label),
            vjust = -0.3,
            size = 4) +
  labs(
    x = "Groupe de traits",
    y = "Nombre d’occurrences"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))
