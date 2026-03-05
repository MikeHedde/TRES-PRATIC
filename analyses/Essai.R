library(dplyr)
library(ggplot2)

db <- read.csv("data/raw-data/raw_db.csv", h= T, sep = ";")

db2 <- db %>%
  select(Study_ID, Publication_Year, Newspaper, Study_country) %>%
  distinct()

ggplot(db2, aes(x = Study_country))+
  geom_bar()+
  labs(x = "Publication year", y = "Article number")
