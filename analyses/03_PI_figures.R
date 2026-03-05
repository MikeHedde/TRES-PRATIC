
# Dataset
PI_db <- read.csv("data/derived-data/PI_db.csv") 

# Lollipop intervention
intervention_counts <- PI_db %>%
  count(Intervention_R2, sort = TRUE)

ggplot(intervention_counts,
       aes(x = reorder(Intervention_R2, n), y = n)) +
  geom_segment(aes(xend = Intervention_R2, y = 0, yend = n),
               color = "grey70") +
  geom_point(size = 4, color = "#2C7BB6") +
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "Number of studies",
       title = "Distribution of interventions")

ggsave("Figures/02_PI/intervention_counts.png",
       width = 12,
       height = 12,
       units = "cm",
       dpi = 300)

# Lollipop : populations étudiées
pop_counts <- PI_db %>%
  separate_rows(Population_homogenized, sep = ",\\s*") %>%
  count(Population_homogenized, sort = TRUE)

ggplot(pop_counts,
       aes(x = reorder(Population_homogenized, n), y = n)) +
  geom_segment(aes(xend = Population_homogenized, y = 0, yend = n),
               color = "grey70") +
  geom_point(size = 4, color = "#D7191C") +
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "Number of studies")

ggsave("Figures/02_PI/pop_counts.png",
       width = 12,
       height = 12,
       units = "cm",
       dpi = 300)

# Heatmap croisant interventions et populations
heatmap_data <- PI_db %>%
  separate_rows(Population_homogenized, sep = ",\\s*") %>%
  count(Intervention_R2, Population_homogenized)

ggplot(heatmap_data,
       aes(x = Intervention_R2,
           y = Population_homogenized,
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Studies") +
  theme_minimal() +
  labs(x = "Intervention type",
       y = "Soil fauna group",
       title = "Distribution of interventions across soil fauna groups") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Figures/02_PI/PI_heatmap.png",
       width = 12,
       height = 12,
       units = "cm",
       dpi = 300)

# Heatmap avec piechart dans les tiles
db_long <- PI_db %>%
  separate_rows(Population_homogenized, sep = ",\\s*") %>%
  separate_rows(Trait_group, sep = ",\\s*")

trait_counts <- db_long %>%
  filter(!is.na(Trait_group)) %>%
  count(Intervention_R2, Population_homogenized, Trait_group)

trait_wide <- trait_counts %>%
  pivot_wider(
    names_from = Trait_group,
    values_from = n,
    values_fill = 0
  )

trait_wide <- trait_wide %>%
  mutate(
    x = as.numeric(factor(Intervention_R2)),
    y = as.numeric(factor(Population_homogenized))
  )

ggplot() +
  
  geom_tile(
    data = trait_wide,
    aes(x = x, y = y),
    fill = "grey99",
    color = "white"
  ) +
  
  geom_scatterpie(
    data = trait_wide,
    aes(x = x, y = y),
    cols = c(
      "Morphological",
      "Physiological",
      "Ecological preferences",
      "Behavioral"
    ),
    pie_scale = 0.8
  ) +
  
  scale_x_continuous(
    breaks = unique(trait_wide$x),
    labels = unique(trait_wide$Intervention_R2)
  ) +
  
  scale_y_continuous(
    breaks = unique(trait_wide$y),
    labels = unique(trait_wide$Population_homogenized)
  ) +
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Intervention type",
    y = "Soil fauna group"
  )

ggsave("Figures/02_PI/heatmap_traits.png",
       width = 18,
       height = 14,
       units = "cm",
       dpi = 300)
