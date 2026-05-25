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
# 2. Facteurs
#############################################################

PI_db <- PI_db %>%
  mutate(
    Population_homogenized = factor(Population_homogenized, levels = rev(order_taxa)),
    Intervention_R2 = factor(Intervention_R2, levels = intervention_order)
  )

#############################################################
# 3. Traduction française
#############################################################

PI_db <- PI_db %>%
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
      "Pest and disease management" = "Gestion des ravageurs\net maladies",
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
# 4. Agrégation
#############################################################

heatmap_data <- PI_db %>%
  count(Intervention_R2, Population_homogenized, name = "n") %>%
  filter(!is.na(Intervention_R2), !is.na(Population_homogenized))

intervention_counts <- PI_db %>%
  distinct(Study_ID, Intervention_R2) %>%
  count(Intervention_R2, name = "n")

pop_counts <- PI_db %>%
  distinct(Study_ID, Population_homogenized) %>%
  count(Population_homogenized, name = "n")

#############################################################
# 5. Thème commun
#############################################################

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

#############################################################
# 6. Heatmap
#############################################################

p_heat <- ggplot(
  heatmap_data,
  aes(x = Intervention_R2,
      y = Population_homogenized,
      fill = n)
) +
  geom_tile(color = "white", linewidth = 0.45) +
  
  geom_text(
    aes(label = n),
    size = 3.2,
    color = "black"
  ) +
  
  scale_fill_gradientn(
    colours = c("#EDF4FB", "#C6DBEF", "#6BAED6", "#2171B5"),
    name = "Nombre\nd'études",
    guide = guide_colorbar(
      barheight = unit(35, "mm"),
      barwidth  = unit(3.5, "mm"),
      title.position = "top",
      ticks = FALSE
    )
  ) +
  
  labs(
    x = "Type d'intervention",
    y = "Groupe faunistique"
  ) +
  
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

#############################################################
# 7. Marginale haute
#############################################################

p_top <- ggplot(intervention_counts, aes(Intervention_R2, n)) +
  geom_col(width = 0.9, fill = "grey35", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_pub +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
  )

#############################################################
# 8. Marginale droite
#############################################################

p_right <- ggplot(pop_counts, aes(n, Population_homogenized)) +
  geom_col(width = 0.9, fill = "grey35", color = "black") +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_pub +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
  )

#############################################################
# 9. Légende heatmap
#############################################################

p_heat_leg <- p_heat +
  theme(legend.position = "right")

leg <- cowplot::get_legend(p_heat_leg)

p_heat_noleg <- p_heat_leg + theme(legend.position = "none")

#############################################################
# 10. Assemblage
#############################################################

main_panel <- (p_top + plot_spacer()) /
  (p_heat_noleg + p_right) +
  plot_layout(widths = c(6, 0.9), heights = c(0.9, 6))

final_plot <- cowplot::plot_grid(
  main_panel,
  leg,
  nrow = 1,
  rel_widths = c(1, 0.09),
  align = "h"
)

#############################################################
# 11. Output
#############################################################

final_plot

ggsave(
  "Figures/02_PI/heatmap_marginals_final.png",
  plot = final_plot,
  width = 9.2,
  height = 6.6,
  dpi = 450,
  bg = "white"
)