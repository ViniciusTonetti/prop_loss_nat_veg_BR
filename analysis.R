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

# Filtering, summarizing, and summing values for each year

# First, checking classes considered natural vegetation

# Checking level_1 class
MB %>% 
  filter(level_0 == "Natural") %>% 
  select(level_1) %>% 
  distinct()

# excluding from level_1 class: "Water" 
MB <- MB %>% 
  filter(level_0 == "Natural") %>% 
  filter(level_1 != "5. Water")


# Checking level_2 class
MB %>% 
  select(level_2) %>% 
  distinct()

# excluding from level_2 class: "Rocky outcrop", "Salt flat", "Beach and Dune"  
MB <- MB %>% 
  filter(level_2 != "Rocky outcrop") %>% 
  filter(level_2 != "Salt flat") %>% 
  filter(level_2 != "Beach and Dune")


# Checking level_3 class
MB %>% 
  select(level_3) %>% 
  distinct()

# Checking level_4 class
MB %>% 
  select(level_4) %>% 
  distinct()

# Classes level_2, 3, and 4 only varies for artificial formations, e.g., agriculture


# Summing area values for the selected values above

MB_sum <- MB %>% 
  filter(level_0 == "Natural") %>% 
  group_by(biome) %>%
  summarise(across(`1985`:`2022`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}"))





