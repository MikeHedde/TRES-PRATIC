
# Dataset
paper_level_db <- read.csv("data/derived-data/paper_level_db.csv") 


#############################################################
# Figure showing the number of studies published per country
#############################################################
articles_per_country <- paper_level_db %>%
  count(Study_country) %>%
  rename(Country = Study_country)
  
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(articles_per_country, by = c("name" = "Country"))

# manque-t-il des pays dans le df joint?
missing_countries <- setdiff(
  articles_per_country$Country,
  world$name
)

missing_countries

# Comment est nommée Czech Republic dans la base Natural Earth
#world %>%
#  filter(grepl("Czech", name))

# on change le nom dans articles_per_country -> fait sur la bdd
#articles_per_country <- articles_per_country %>%
# mutate(
#    Country = ifelse(Country == "Czech Republic",
#                     "Czechia",
#                     Country)
# )

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

#############################################################
# Figure showing the cumulated number of articles published per year
#############################################################
pub_year <- data.frame(
  Publication_Year = seq(1995, 2022)) 

year_counts <- paper_db %>%  #nrow = 40 : 41 articles moins le 127 pas encore traité exclu script 01
  count(Publication_Year) 

year_counts <- pub_year %>%
  left_join(year_counts, by = "Publication_Year") 
  
year_counts <- year_counts %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(Publication_Year) %>%
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
    y = "Cumulative number of papers"
  ) +
  expand_limits(y = max(year_counts$cum_articles) + 8)

ggsave("Figures/01_paper_level/cumulative_articles.png",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300)

########################################################
#Avec le ratio
########################################################
publications_trend <- read.csv("data/raw-data/total_publications_trend.csv", header = T, sep = ";")

years_count_trend <- publications_trend %>%
  arrange(Publication.Years) %>%
  filter(Publication.Years >= 1996, Publication.Years <= 2022) %>%
  rename(Publication_Year = Publication.Years) %>%
  mutate(cum_articles = cumsum(Count)) %>%
  left_join(year_counts, by = "Publication_Year") %>%
  select(Publication_Year, cum_articles.x, cum_articles.y) %>%
  mutate(Ratio = cum_articles.y/cum_articles.x)
 
#ggplot(years_count_trend, 
#       aes(x = Publication_Year)) +
#  geom_step(aes(y = cum_articles.x), size = 1.2, color = "blue") +
#  geom_point(aes(y = cum_articles.x), color = "blue") +
#  geom_step(aes(y = cum_articles.y), size = 1.2, color = "red") +
#  geom_point(aes(y = cum_articles.y), color = "red")

ggplot(years_count_trend,
       aes(x = Publication_Year, y = Ratio)) +
  geom_step(size = 1.2, color = "blue") + #croissant : les études sur les traits augmentent plus vite que la tendance habituelle
  geom_point(size = 2, color = "blue") + #décroissant : les études sur les traits augmentent moins vite que la tendance habituelle
  
# flèche Violle
geom_segment(aes(x = 2007, xend = 2007,
                 y = max(Ratio)-0.0001,
                 yend = Ratio[Publication_Year == 2007]),
             arrow = arrow(length = unit(0.2, "cm"))) +
  
  geom_text(aes(x = 2007,
                y = max(Ratio),
                label = "Violle et al. 2007"),
            hjust = 0.5,
            size = 3.5) +
  
  # flèche Pey
  geom_segment(aes(x = 2014, xend = 2014,
                   y = max(Ratio)-0.0001,
                   yend = Ratio[Publication_Year == 2014]),
               arrow = arrow(length = unit(0.2, "cm"))) +
  
  geom_text(aes(x = 2014,
                y = max(Ratio),
                label = "Pey et al. 2014"),
            hjust = 0.5,
            size = 3.5) +
  
  theme_classic() +
  labs(
    x = "Année de publication",
    y = "Ratio atricles du corpus/ tendance générale"
  ) +
  expand_limits(y = max(year_counts$Ratio))

ggsave("Figures/01_paper_level/cumulative_articles_ratio.png",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300)

########################################################
# Treemap des journaux
########################################################
  ##nombre minimal d'articles pour considérer la revue
  threshold = 2

journal_counts <- paper_db %>%
  distinct(Article_ID, Newspaper) %>%
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
