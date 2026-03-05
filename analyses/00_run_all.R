# ============================================================
# 00_run_all.R — Full pipeline runner
# ============================================================

# Libraries
librarian::shelf(tidyverse, ggplot2, sf, rnaturalearth, scatterpie, treemapify)

# Run all scripts
# Order matters !
source(file.path("analyses/01_data_handling.R"))
source(file.path("analyses/02_paper_level_figures.R"))
source(file.path("analyses/03_PI_figures.R"))