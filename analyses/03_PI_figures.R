
# Dataset
PI_db <- read.csv("data/derived-data/PI_db.csv") 

    # A enlever plus tard quand les colonne seront clean
#    PI_db <- PI_db %>%
#      mutate(Study_ID = str_extract(Study_ID, "s_[0-9]+"))%>%
#      mutate(
#        Population_homogenized = case_when(
#          Population_homogenized %in% c("Earthworms", "Earthworm") ~ "Earthworms",
#          Population_homogenized %in% c("Termite", "Termites") ~ "Termites",
#          Population_homogenized %in% c("Invertebrate", "Invertebrates", "Arthropods") ~ "Invertebrates",
#          Population_homogenized %in% c("Other insects", "Insects", "Arthropods") ~ "Other insects",
#          TRUE ~ Population_homogenized
#        )
#      )

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

heatmap_data <- PI_db %>% #j'ai enlevé le 2
  distinct(Study_ID, Intervention_R2, Population_homogenized) %>%
  count(Intervention_R2, Population_homogenized, name = "n")

intervention_counts <- PI_db %>% #j'ai enlevé le 2
  distinct(Study_ID, Intervention_R2) %>%
  count(Intervention_R2, name = "n")

pop_counts <- PI_db %>% #j'ai enlevé le 2
  distinct(Study_ID, Population_homogenized) %>%
  count(Population_homogenized, name = "n")

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
  