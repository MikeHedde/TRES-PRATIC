
# Dataset
paper_level_db <- read.csv("data/derived-data/paper_level_db.csv") 

# Figure showing the number of articles published per country
articles_per_country <- paper_level_db %>%
  count(Study_country) %>%
  rename(Country = Study_country)
  
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(articles_per_country, by = c("name" = "Country"))

fig_chloropeth <- ggplot(map_data) +
  geom_sf(aes(fill = n), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey95",
    name = "Number of \narticles"
  ) +
  theme_minimal()

ggsave("Figures/01_paper_level/map_articles_country.png", 
       plot = fig_chloropeth,
       width = 20, height = 8, dpi = 300)

# Figure showing the cummulated number of articles published per year
year_counts <- paper_level_db %>%
  count(Publication_Year) %>%
  arrange(Publication_Year)%>%
  mutate(cum_articles = cumsum(n))

ggplot(year_counts, aes(Publication_Year, cum_articles)) +
  geom_step(size = 1.2) +
  geom_point(size = 2) +
  
  # flèche Violle
  geom_segment(aes(x = 2007, xend = 2007,
                   y = max(cum_articles) + 5,
                   yend = cum_articles[Publication_Year == 2007]),
               arrow = arrow(length = unit(0.2, "cm"))) +
  
  geom_text(aes(x = 2007,
                y = max(cum_articles) + 6,
                label = "Violle et al. 2007"),
            hjust = 0.5,
            size = 4) +
  
  # flèche Pey
  geom_segment(aes(x = 2014, xend = 2014,
                   y = max(cum_articles) + 5,
                   yend = cum_articles[Publication_Year == 2014]),
               arrow = arrow(length = unit(0.2, "cm"))) +
  
  geom_text(aes(x = 2014,
                y = max(cum_articles) + 6,
                label = "Pey et al. 2014"),
            hjust = 0.5,
            size = 4) +
  
  theme_classic() +
  labs(
    x = "Publication year",
    y = "Cumulative number of studies"
  ) +
  expand_limits(y = max(year_counts$cum_articles) + 8)

ggsave("Figures/01_paper_level/cumulative_articles.png",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300)

# Treemap des journaux
  ##nombre minimal d'article pour considérer la revue
  threshold = 2

journal_counts <- paper_level_db %>%
  distinct(Study_ID, Newspaper) %>%
  count(Newspaper, sort = TRUE) %>%
  mutate(Newspaper = ifelse(n < threshold, "Other journals", Newspaper))%>%
  group_by(Newspaper) %>%
  summarise(n = sum(n))

journal_treemap <- ggplot(journal_counts,
       aes(area = n,
           fill = Newspaper,
           label = paste(Newspaper, "\n", n))) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Distribution of articles by journal")

ggsave("Figures/01_paper_level/journal_treemap.png",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300)
