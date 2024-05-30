# Code to create curves of proportional loss of native vegetation in Brazil through presidential terms
# Vinicius Tonetti  - vrtonetti@gmail.com
# ------------------------------------------------------------------------------

rm(list = ls())

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(readxl)


# Loading data -----------------------------------------------------------------

MB <- readxl::read_excel(path = "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/data/TABELA-GERAL-MAPBIOMAS-COL8.0-BIOMASxESTADOS-1.xlsx", sheet = "COBERTURA_COL8.0", )


# Filtering data ---------------------------------------------------------------

# Checking unique biome classes
unique(MB$biome)

# Checking unique veg classes
unique(MB$level_0)
unique(MB$level_1)
unique(MB$level_2)

# Filtering, summarizing, and summing values for each year

MB %>% 
  filter(level_0 == "Natural") %>% 
  group_by(biome) %>%
  summarise(across(`1985`:`2022`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}"))





