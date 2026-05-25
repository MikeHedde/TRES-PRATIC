#############################################################
# Figure heatmap + marginales avec nombres affichés
#############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)

#############################################################
# 1. Ordres
#############################################################

intervention_order <- c(
  "Tillage management",
  "Crop diversification",
  "Organic agriculture",
  "Landscape complexity",
  "Land-use change",
  "Combined practices",
  "Agroforestry",
  "Fertilisers and amendments",
  "Water management",
  "Residues management",
  "Pest and disease management",
  "GMO",
  "Conservation agriculture"
)

order_taxa <- c(
  "Earthworms",
  "Beetles",
  "Spiders",
  "Macroinvertebrates",
  "Microinvertebrates",
  "Collembola",
  "Termites",
  "Other insects",
  "Invertebrates",
  "Millipedes",
  "Acari",
  "Ants",
  "Woodlice",
  "Other arachnids",
  "Mollusks",
  "Tardigrada"
)

#############################################################
# 2. Facteurs + traduction
#############################################################

PI_db <- PI_db %>%
  mutate(
    Population_homogenized = factor(Population_homogenized, levels = rev(order_taxa)),
    Intervention_R2 = factor(Intervention_R2, levels = intervention_order)
  ) %>%
  mutate(
    Intervention_R2 = recode(
      Intervention_R2,
      "Tillage management" = "Travail du sol",
      "Crop diversification" = "Diversification des cultures",
      "Organic agriculture" = "Agriculture biologique",
      "Landscape complexity" = "Complexité du paysage",
      "Land-use change" = "Reclassification des terres",
      "Combined practices" = "Pratiques multiples",
      "Agroforestry" = "Agroforesterie",
      "Fertilisers and amendments" = "Fertilisation",
      "Water management" = "Gestion de l'eau",
      "Residues management" = "Gestion des déchets",
      "Pest and disease management" = "Gestion des ravageurs et maladies",
      "GMO" = "OGM",
      "Conservation agriculture" = "Agriculture de conservation"
    ),
    Population_homogenized = recode(
      Population_homogenized,
      "Earthworms" = "Vers de terre",
      "Beetles" = "Carabidés",
      "Spiders" = "Araignées",
      "Macroinvertebrates" = "Macroinvertébrés",
      "Collembola" = "Collemboles",
      "Other insects" = "Autres insectes",
      "Microinvertebrates" = "Microinvertébrés",
      "Millipedes" = "Mille-pattes",
      "Acari" = "Acariens",
      "Termites" = "Termites",
      "Ants" = "Fourmis",
      "Woodlice" = "Cloportes",
      "Invertebrates" = "Invertébrés",
      "Other arachnids" = "Autres arachnides",
      "Mollusks" = "Mollusques",
      "Tardigrada" = "Tardigrades"
    )
  )

#############################################################
# 3. Agrégation
#############################################################

heatmap_data <- PI_db %>%
  count(Intervention_R2, Population_homogenized, name = "n") %>%
  filter(!is.na(n))

intervention_counts <- PI_db %>%
  distinct(Study_ID, Intervention_R2) %>%
  count(Intervention_R2, name = "n")

pop_counts <- PI_db %>%
  distinct(Study_ID, Population_homogenized) %>%
  count(Population_homogenized, name = "n")

#############################################################
# 4. Theme
#############################################################

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

#############################################################
# 5. HEATMAP (corrigée)
#############################################################

p_heat <- ggplot(heatmap_data,
                 aes(x = Intervention_R2,
                     y = Population_homogenized,
                     fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = n), size = 3, color = "black") +
  scale_fill_gradientn(
    colours = c("#DCE9F9", "#A6C8E8", "#4A90C2", "#1F5A99"),
    name = "Nombre\nd'études",
    guide = guide_colorbar(
      barheight = unit(30, "mm"),
      barwidth = unit(3, "mm"),
      title.position = "top",
      ticks = FALSE
    )
  ) +
  labs(x = "Type d'intervention",
       y = "Groupe faunistique") +
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#############################################################
# 6. BARPLOT HAUT (+ nombres)
#############################################################

p_top <- ggplot(intervention_counts,
                aes(x = Intervention_R2, y = n)) +
  geom_col(fill = "grey40") +
  geom_text(aes(label = n), vjust = -0.3, size = 3) +
  theme_pub +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  )

#############################################################
# 7. BARPLOT DROIT (+ nombres)
#############################################################

p_right <- ggplot(pop_counts,
                  aes(x = n, y = Population_homogenized)) +
  geom_col(fill = "grey40") +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  theme_pub +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  )

#############################################################
# 8. ASSEMBLAGE
#############################################################

main_panel <- (p_top + plot_spacer()) /
  (p_heat + p_right) +
  plot_layout(widths = c(6, 1), heights = c(1, 6))

#############################################################
# 9. EXPORT
#############################################################

ggsave(
  "Figures/02_PI/heatmap_final_corrected.png",
  plot = main_panel,
  width = 10,
  height = 7,
  dpi = 450,
  bg = "white"
)

main_panel