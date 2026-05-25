# Installation of packages
if(!require("metafor"))install.packages("metafor")
if(!require("clubSandwich"))install.packages("clubSandwich")
if(!require("MuMIn"))install.packages("MuMIn")
if(!require("glmulti"))install.packages("glmulti")
if(!require("mice"))install.packages("mice")
if(!require("metagear"))install.packages("metagear")
if(!require("tidyverse"))install.packages("tidyverse")
if(!require("here"))install.packages("here")
if(!require("DT"))install.packages("DT")
if(!require("readxl"))install.packages("readxl")
if(!require("stringr"))install.packages("stringr")
if(!require("GoodmanKruskal"))install.packages("GoodmanKruskal")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("plotly"))install.packages("plotly")
if(!require("cowplot"))install.packages("cowplot")
if(!require("grDevices"))install.packages("grDevices")
if(!require("grid"))install.packages("grid")
if(!require("gridGraphics"))install.packages("gridGraphics")
if(!require("pander"))install.packages("pander")
if(!require("formatR"))install.packages("formatR")

devtools::install_github("daniel1noble/orchaRd", force = TRUE)
devtools::install_github("daniel1noble/ggthemr", force = TRUE)

# Libraries
library(metafor)

dat <- read.csv("data/raw-data/quanti_data.csv", 
                sep = ",", 
                header = T)

dat_es <- escalc(
  measure = "ROM",   # Ratio of Means (log response ratio)
  m1i = Mean_intervention,
  sd1i = sd_intervention,
  n1i = N_intervention,
  m2i = Mean_comparator,
  sd2i = sd_comparator,
  n2i = N_comparator,
  data = data_ma_1A
)

_lnRR <- rma.mv(yi = lnRR, 
                V = lnRRV, 
                random = list(~1 | Study_ID, # allows true effect sizes to vary among different primary studies - account for the between-study effect and quantify between-study heterogeneity;
                              ~1 | ES_ID), # allows true effect sizes to vary within primary studies - account for the with-study effect and quantify with-study heterogeneity;
                method = "REML", # REML is assigned as the estimator for variance components as suggested;
                data = dat2_Midolo_2019 # our dataset
)
