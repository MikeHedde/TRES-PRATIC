
# Dataset
PI_db <- read.csv("data/derived-data/PI_db.csv") 

    # A enlever plus tard quand les colonne seront clean
    PI_db <- PI_db %>%
      mutate(Study_ID = str_extract(Study_ID, "s_[0-9]+"))%>%
      mutate(
        Population_homogenized = case_when(
          Population_homogenized %in% c("Earthworms", "Earthworm") ~ "Earthworms",
          Population_homogenized %in% c("Termite", "Termites") ~ "Termites",
          Population_homogenized %in% c("Invertebrate", "Invertebrates", "Arthropods") ~ "Invertebrates",
          Population_homogenized %in% c("Other insects", "Insects", "Arthropods") ~ "Other insects",
          TRUE ~ Population_homogenized
        )
      )

# Ordre logique pour la représentation
order_taxa <- c(
  "Invertebrates",
  "Macroinvertebrates",
  "Earthworms",
  "Termites",
  "Ants",
  "Beetles",
  "Other insects",
  "Spiders",
  "Other arachnids",
  "Woodlice",
  "Millipedes",
  "Mollusks",
  "Microinvertebrates",
  "Acari",
  "Collembola",
  "Tardigrada"
)

PI_db <- PI_db %>%
  mutate(
    Population_homogenized =
      factor(Population_homogenized, levels = order_taxa)
  )

#############################################################
# 2. Agrégation
#############################################################

heatmap_data <- PI_db %>%
  distinct(Study_ID, Intervention_R2, Population_homogenized) %>%
  count(Intervention_R2, Population_homogenized, name = "n")

intervention_counts <- PI_db %>%
  distinct(Study_ID, Intervention_R2) %>%
  count(Intervention_R2, name = "n")

pop_counts <- PI_db %>%
  distinct(Study_ID, Population_homogenized) %>%
  count(Population_homogenized, name = "n")

#############################################################
# 2bis. Préparation pour figures complémentaires
#############################################################

# Nettoyage éventuel de la colonne Trait_group
PI_db <- PI_db %>%
  mutate(
    Trait_group = case_when(
      Trait_group %in% c("Morphological", "Morphology") ~ "Morphological",
      Trait_group %in% c("Physiological", "Physiology") ~ "Physiological",
      Trait_group %in% c("Ecological preferences", "Ecological", "Ecology") ~ "Ecological preferences",
      Trait_group %in% c("Behavioral", "Behavioural", "Behavior") ~ "Behavioral",
      TRUE ~ Trait_group
    )
  )

# Comptage du nombre d'études par intervention
intervention_counts <- PI_db %>%
  distinct(Study_ID, Intervention_R2) %>%
  count(Intervention_R2, name = "n") %>%
  arrange(n)

# Comptage du nombre d'études par groupe de faune
pop_counts <- PI_db %>%
  distinct(Study_ID, Population_homogenized) %>%
  count(Population_homogenized, name = "n") %>%
  arrange(n)

# Données pour la figure "camemberts dans la matrice"
trait_pie_data <- PI_db %>%
  distinct(Study_ID, Intervention_R2, Population_homogenized, Trait_group) %>%
  count(Intervention_R2, Population_homogenized, Trait_group, name = "n")

# Totaux par case (pour éventuellement contrôler la taille)
trait_totals <- trait_pie_data %>%
  group_by(Intervention_R2, Population_homogenized) %>%
  summarise(total = sum(n), .groups = "drop")

#############################################################
# 3. Thème commun
#############################################################

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

#############################################################
# 4. Heatmap
#############################################################

p_heat <- ggplot(
  heatmap_data,
  aes(x = Intervention_R2, y = Population_homogenized, fill = n)
) +
  geom_tile(color = "white", linewidth = 0.45) +
  scale_fill_viridis_c(
    name = "Number \nof papers",
    option = "D",
    guide = guide_colorbar(
      barheight = unit(35, "mm"),
      barwidth  = unit(3.5, "mm"),
      title.position = "top",
      ticks = FALSE
    )
  ) +
  labs(
    x = "Intervention type",
    y = "Soil fauna group"
  ) +
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 2, 0, 0)
  )

#############################################################
# 5. Marginale haute
#############################################################

p_top <- ggplot(
  intervention_counts,
  aes(x = Intervention_R2, y = n)
) +
  geom_col(
    width = 0.9,
    fill = "grey35",
    color = "black",
    linewidth = 0.25
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_pub +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

#############################################################
# 6. Marginale droite
#############################################################

p_right <- ggplot(
  pop_counts,
  aes(x = n, y = Population_homogenized)
) +
  geom_col(
    width = 0.9,
    fill = "grey35",
    color = "black",
    linewidth = 0.25
  ) +
  theme_pub +
  scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

#############################################################
# 7. Assemblage final
#############################################################

# 1. Heatmap avec légende
p_heat_leg <- p_heat +
  theme(
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  ) +
  scale_fill_viridis_c(
    name = "Number \nof papers",
    option = "D",
    guide = guide_colorbar(
      barheight = unit(28, "mm"),
      barwidth  = unit(3, "mm"),
      title.position = "top",
      ticks = FALSE
    )
  )

# 2. Extraction de la légende
leg <- cowplot::get_legend(p_heat_leg)

# 3. Heatmap sans légende
p_heat_noleg <- p_heat_leg +
  theme(legend.position = "none")

# 4. Bloc principal compact (sans légende)
main_panel <- (p_top + patchwork::plot_spacer()) /
  (p_heat_noleg + p_right) +
  plot_layout(
    widths  = c(6, .9),
    heights = c(.9, 6)
  )

# 5. Assemblage final avec légende collée
final_plot <- cowplot::plot_grid(
  main_panel,
  leg,
  nrow = 1,
  rel_widths = c(1, 0.09),
  align = "h",
  axis = "tb"
)

final_plot

#Pour exporter :
  
  ggsave(
    "Figures/02_PI/heatmap_marginals_final.png",
    plot = final_plot,
    width = 9.2,
    height = 6.6,
    units = "in",
    dpi = 450,
    bg = "white"
  )
  
  
  #############################################################
  # Figure A. Distribution of interventions
  #############################################################
  
  fig_intervention_counts <- ggplot(
    intervention_counts,
    aes(x = n, y = reorder(Intervention_R2, n))
  ) +
    geom_segment(
      aes(x = 0, xend = n,
          y = reorder(Intervention_R2, n),
          yend = reorder(Intervention_R2, n)),
      color = "grey70",
      linewidth = 1
    ) +
    geom_point(size = 8, color = "#2C7FB8") +
    labs(
      title = "Distribution of interventions",
      x = "Number of studies",
      y = NULL
    ) +
    theme_classic(base_size = 16) +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold")
    )
  
  ggsave(
    "Figures/02_PI/intervention_counts.png",
    plot = fig_intervention_counts,
    width = 8,
    height = 8,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  #############################################################
  # Figure B. Distribution of soil fauna groups
  #############################################################
  
  fig_pop_counts <- ggplot(
    pop_counts,
    aes(x = n, y = reorder(Population_homogenized, n))
  ) +
    geom_segment(
      aes(x = 0, xend = n,
          y = reorder(Population_homogenized, n),
          yend = reorder(Population_homogenized, n)),
      color = "grey70",
      linewidth = 1
    ) +
    geom_point(size = 8, color = "#E41A1C") +
    labs(
      x = "Number of studies",
      y = NULL
    ) +
    theme_classic(base_size = 16) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold")
    )
  
  ggsave(
    "Figures/02_PI/pop_counts.png",
    plot = fig_pop_counts,
    width = 8,
    height = 8,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  #############################################################
  # Figure C. Trait-group composition by intervention and fauna
  #############################################################
  
  # Ordre des interventions (tu peux le modifier si besoin)
  intervention_order <- c(
    "Agroforestry",
    "Combined practices",
    "Conservation agriculture",
    "Crop diversification",
    "Fertilisers and amendments",
    "Land-use change",
    "Landscape complexity",
    "Tillage management",
    "Water management"
  )
  
  #############################################################
  # Ordres
  #############################################################
  
  # ordre des interventions = ordre d'apparition dans les données
  intervention_order <- trait_pie_data %>%
    filter(!is.na(Intervention_R2)) %>%
    pull(Intervention_R2) %>%
    unique()
  
  # ordre des taxons : on garde seulement ceux présents dans trait_pie_data
  taxa_order <- trait_pie_data %>%
    filter(!is.na(Population_homogenized)) %>%
    pull(Population_homogenized) %>%
    unique()
  
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
  for (col in c("Morphological", "Physiological", "Ecological preferences", "Behavioral")) {
    if (!col %in% names(trait_pie_wide)) trait_pie_wide[[col]] <- 0
  }
  
  # recalcul du total directement à partir des colonnes pie
  trait_pie_wide <- trait_pie_wide %>%
    mutate(
      total = Morphological + Physiological + `Ecological preferences` + Behavioral
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
      cols = c("Morphological", "Physiological", "Ecological preferences", "Behavioral"),
      color = "black",
      linewidth = 0.5
    ) +
    scale_x_continuous(
      breaks = seq_along(intervention_order),
      labels = intervention_order,
      expand = expansion(add = 0.5)
    ) +
    scale_y_continuous(
      breaks = seq_along(rev(taxa_order)),
      labels = rev(taxa_order),
      expand = expansion(add = 0.5)
    ) +
    coord_equal() +
    scale_fill_manual(
      values = c(
        "Morphological" = "#F8766D",
        "Physiological" = "#7CAE00",
        "Ecological preferences" = "#00BFC4",
        "Behavioral" = "#C77CFF"
      ),
      breaks = c("Morphological", "Physiological", "Ecological preferences", "Behavioral"),
      name = "type"
    ) +
    labs(
      x = "Intervention type",
      y = "Soil fauna group"
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
  
  