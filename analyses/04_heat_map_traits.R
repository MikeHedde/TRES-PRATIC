#############################################################
# Figure C. Trait-group composition by intervention and fauna
#############################################################
CS_db <- read.csv("data/derived-data/CS_db.csv") 

######################################################
#Ordre
######################################################
intervention_order <- c(
  "Landscape complexity",
  "Agroforestry",
  "Organic agriculture",
  "Combined practices",
  "Tillage management",
  "Land-use change",
  "Crop diversification",
  "Pest and disease management",
  "GMO",
  "Fertilisers and amendments",
  "Water management",
  "Residues management",
  "Conservation agriculture"
)

taxa_order <- c(
  "Collembola",
  "Earthworms",
  "Acari",
  "Beetles",
  "Spiders",
  "Macroinvertebrates",
  "Invertebrates",
  "Termites",
  "Millipedes",
  "Other insects",
  "Ants",
  "Microinvertebrates",
  "Woodlice",
  "Mollusks"
)

#################################################################
# Données pour la figure "camemberts dans la matrice"
trait_pie_data <- CS_db %>%
  #distinct(Study_ID, Intervention_R2, Population_homogenized, Trait_group) %>%
  count(Intervention_R2, Population_homogenized, Trait_group, name = "n")

# Totaux par case (pour éventuellement contrôler la taille)
trait_totals <- trait_pie_data %>%
  group_by(Intervention_R2, Population_homogenized) %>%
  summarise(total = sum(n), .groups = "drop")


# ordre des interventions = ordre d'apparition dans les données
#intervention_order <- trait_pie_data %>%
#  filter(!is.na(Intervention_R2)) %>%
#  pull(Intervention_R2) %>%
#  unique()

# ordre des taxons : on garde seulement ceux présents dans trait_pie_data
#taxa_order <- trait_pie_data %>%
#  filter(!is.na(Population_homogenized)) %>%
#  pull(Population_homogenized) %>%
#  unique()

#############################################################
# Données pies
#############################################################
trait_pie_wide <- trait_pie_data %>%
  filter(
    !is.na(Intervention_R2),
    !is.na(Population_homogenized),
    !is.na(Trait_group)
  ) %>%
  group_by(Intervention_R2, Population_homogenized, Trait_group) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(
    names_from = Trait_group,
    values_from = n,
    values_fill = 0
  )

# s'assurer que toutes les colonnes existent
for (col in c("Morphological", "Physiological", "Ecological preference", "Behavioral", "Phenological")) {
  if (!col %in% names(trait_pie_wide)) trait_pie_wide[[col]] <- 0
}

# recalcul du total directement à partir des colonnes pie
trait_pie_wide <- trait_pie_wide %>%
  mutate(
    total = Morphological + Physiological + `Ecological preference` + Behavioral + Phenological
  ) %>%
  mutate(
    Intervention_R2 = factor(Intervention_R2, levels = intervention_order),
    Population_homogenized = factor(Population_homogenized, levels = rev(taxa_order))
  ) %>%
  mutate(
    x = as.integer(Intervention_R2),
    y = as.integer(Population_homogenized),
    r = 0.22
    # ou, si tu veux faire varier la taille :
    # r = 0.12 + 0.02 * sqrt(total)
  ) %>%
  filter(!is.na(x), !is.na(y), total > 0)

#############################################################
# Grille de fond complète
#############################################################
grid_df <- expand.grid(
  Intervention_R2 = intervention_order,
  Population_homogenized = rev(taxa_order),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Intervention_R2 = factor(Intervention_R2, levels = intervention_order),
    Population_homogenized = factor(Population_homogenized, levels = rev(taxa_order)),
    x = as.integer(Intervention_R2),
    y = as.integer(Population_homogenized)
  )

#############################################################
# Figure
#############################################################

fig_heatmap_traits <- ggplot() +
  geom_tile(
    data = grid_df,
    aes(x = x, y = y),
    fill = "grey95",
    color = "white",
    linewidth = 0.5,
    width = 1,
    height = 1
  ) +
  scatterpie::geom_scatterpie(
    data = trait_pie_wide,
    aes(x = x, y = y, r = r),
    cols = c("Morphological", "Physiological", "Ecological preference", "Behavioral", "Phenological"),
    color = "black",
    linewidth = 0.5
  ) +
  scale_x_continuous(
    breaks = seq_along(intervention_order),
    labels = c(
      "Complexité du paysage",
      "Agroforesterie",
      "Agriculture biologique",
      "Pratiques multiples",
      "Travail du sol",
      "Reclassification des terres",
      "Diversification des cultures",
      "Gestion des ravageurs et maladies",
      "OGM",
      "Fertilisation",
      "Gestion de l'eau",
      "Gestion des déchets",
      "Agriculture de conservation"
    ),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    breaks = seq_along(rev(taxa_order)),
    labels = rev(
      c(
        "Collemboles",
        "Vers de terre",
        "Acariens",
        "Carabidés",
        "Araignées",
        "Macroinvertébrés",
        "Invertébrés",
        "Termites",
        "Mille-pattes",
        "Autres insectes",
        "Fourmis",
        "Microinvertébrés",
        "Cloportes",
        "Mollusques"
      )
    ),
    expand = expansion(add = 0.5)
  ) +
  coord_equal() +
  scale_fill_manual(
    values = c(
      "Morphological" = "#F8766D",
      "Physiological" = "#7CAE00",
      "Ecological preference" = "#00BFC4",
      "Behavioral" = "#C77CFF",
      "Phenological" = "#C88"
    ),
    breaks = c(
      "Morphological", 
      "Physiological", 
      "Ecological preference", 
      "Behavioral", 
      "Phenological"
    ),
    labels = c(
      "Morphologique",
      "Physiologique",
      "Préférence écologique",
      "Comportemental",
      "Phénologique"
    ),
    name = "Type de trait"
  ) +
  labs(
    x = "Type d'intervention",
    y = "Groupe faunistique"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold")
  )

fig_heatmap_traits

ggsave(
  "Figures/02_PI/heatmap_traits.png",
  plot = fig_heatmap_traits,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)