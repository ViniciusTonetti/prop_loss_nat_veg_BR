# Code to create curves of proportional deforestation of native vegetation in Brazil through presidential terms
# Vinicius Tonetti  - vrtonetti@gmail.com
# ------------------------------------------------------------------------------

# set R messages in English
Sys.setenv(LANG = "en_US.UTF-8")


# Cleaning directory
rm(list = ls())


# Packages ---------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(cowplot)
library(scales)
library(sf)
library(ggspatial)


# Directories ------------------------------------------------------------------

input <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/data"
output <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/outputs/prop_deforestation"


# Loading data -----------------------------------------------------------------
# Data downloaded on 27/06/2024 from MapBiomas collection 8 from the tab "estatísticas" > DESMATAMENTO E VEGETAÇÃO SECUNDÁRIA (COLEÇÃO 8) –  dados de área (ha) de desmatamento e vegetação secundária por classe de cobertura para os recortes de bioma e estado de 1986 e 2022 (Versão 2, atualizada em 20/02/2024 com filtro espacial de 1ha)

# https://brasil.mapbiomas.org/estatisticas/

MB <- readxl::read_excel(path = paste(input, "/TABELA-DESMATAMENTO-E-VEGETACAO-SECUNDARIA-BIOMA-x-ESTADO-MAPBIOMAS-COL8.0-v2.xlsx", sep = ""), sheet = "STATE_BIOME")


# Checking and filtering data --------------------------------------------------
################################################################################

# Checking cols and rows
colnames(MB)
row.names(MB)


# Checking if values in the tab "STATE_BIOME" are the same in the tab "PIVOT_STATE_BIOME"
# Filtering below and comparing to the data in the spreadsheet, tab "PIVOT_STATE_BIOME"
# Data is matching

MB %>% 
  group_by(BIOME) %>% 
  filter(dr_class_name == "Supressão Veg. Primária") %>% 
  summarise(across(`1986`:`2021`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}"))


# Filtering tibble to show primary forest cover only

Biomes_area <- MB %>% 
  group_by(BIOME) %>% 
  filter(dr_class_name == "Veg. Primária") %>% 
  summarise(across(`1986`:`2021`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}")) %>% 
  select(c(BIOME, sum_2021))


# Checking each class

unique(MB$dr_class_id)
unique(MB$dr_class_name)
unique(MB$class_id)
unique(MB$level_1_id)
unique(MB$level_2_id)
unique(MB$level_3_id)
unique(MB$level_4_id)
unique(MB$level_0)    # "Natural", "Anthropic", "Not applied"

# Filtering to only consider level_0 = "Natural"

MB <- MB %>% 
  filter(level_0 == "Natural")


# Checking each class

unique(MB$level_1) # "1. Forest", "2. Non Forest Natural Formation"

unique(MB$level_2) # "Forest Formation", "Savanna Formation", "Flooded Forest"                 
                   # "Wetland", "Grassland", "Rocky outcrop"                     
                   # "Magrove", "Tidal Flat", "Wooded Restinga"                   
                   # "Shrub Restinga", "Other Non Forest Natural Formation"


# Excluding classes - "Rocky outcrop", "Magrove", "Tidal Flat", "Other Non Forest Natural Formation"
# According to MapBiomas legend - "Other Non Forest Natural Formation" correponds to "Outras Formações Naturais não florestais que não puderam ser categorizadas" IBGE classes - "Herbácea (Planícies fluviomarinhas)", "Vegetação com influência marinha (Restinga) Pm - Arbustiva (das dunas)", "Vegetação com influência marinha (Restinga) Pm - Herbácea (das praias)"


MB <- MB %>% 
  filter(!level_2 %in% c("Rocky outcrop", "Magrove",
                         "Tidal Flat", "Other Non Forest Natural Formation"))


# Checking if it was excluded and checking other classes

unique(MB$level_2)
unique(MB$level_3)
unique(MB$level_4)


# Land cover classes excluded:

#"Rocky outcrop",
#"Magrove"
#"Tidal Flat"
#"Other Non Forest Natural Formation"


# Land cover classes considered in this analysis (Primary cover only):

#"Forest Formation"
#"Savanna Formation"
#"Flooded Forest"
#"Wetland"
#"Grassland"
#"Wooded Restinga"  
#"Shrub Restinga"   


# Summing area values for the selected values above ----------------------------
# Considering values from 1990 as suggested by Marcos Rosa, because first years of mapping are not reliable
# Including year 1989 to consider the proportional increase/decrease of deforestation related to the first year,
# this approach is similar to the calculation of the proportional loss/gain of native vegetation in the first year of analysis (1985)

MB_sum <- MB %>% 
  group_by(BIOME) %>% 
  filter(dr_class_name == "Supressão Veg. Primária") %>% 
  summarise(across(`1986`:`2021`, ~sum(.x, na.rm = TRUE), .names = "defo_{.col}"))


# converting from tibble to data frame to change row names

MB_sum <- as.data.frame(MB_sum)
row.names(MB_sum) <- MB_sum[[1]]
MB_sum <- MB_sum[,-1]


# Preparing data to plot ------------------------------------------------------
################################################################################

# First, creating an empty dataframe to be filled with the prop deforestation

mtx <- matrix(nrow = nrow(MB_sum), ncol = ncol(MB_sum)-1)
mtx <- as.data.frame(mtx)

row.names(mtx) <- row.names(MB_sum)
colnames(mtx) <- paste("prop_defo_", 1987:2021, sep = "")


# loop to fill the matrix with the prop deforestation of natural vegetation

for (i in 2:ncol(MB_sum)) {
  mtx[,i-1] <- (MB_sum[,i] - MB_sum[,i-1])/MB_sum[,i-1]
}


# Replacing Portuguese names to English names

row.names(mtx)[which(row.names(mtx) == "Amazônia")] <- "Amazon"
row.names(mtx)[which(row.names(mtx) == "Mata Atlântica")] <- "Atlantic Forest"


# Filling the matrix with the change of deforestation

mtx_rate <- mtx
colnames(mtx_rate) <- paste("rate_change_", 1987:2021, sep = "")
mtx_rate[,] <- NA # filling the data frame with NA just to erase previous values


for (i in 2:ncol(mtx_rate)) {
  mtx_rate[,i] <- mtx[,i] - mtx[,i-1]
}

# Creating long data frames ----------------------------------------------------

mtx$biome <- row.names(mtx)

mtx_defo_long <- mtx %>%
   filter(biome != "Pantanal", biome != "Pampa") %>% # Excluding Pantanal and Pampa
   select(-c("prop_defo_1987", "prop_defo_1988", "prop_defo_1989")) %>% 
   pivot_longer(cols = starts_with("prop_defo_"),  
               names_to = "year",                 
               names_prefix = "prop_defo_",      
               values_to = "prop_defo")
  


mtx_rate$biome <- row.names(mtx_rate)
  
mtx_defo_rate_long <- mtx_rate %>%
  filter(biome != "Pantanal", biome != "Pampa") %>% # Excluding Pantanal and Pampa
  select(-c("rate_change_1987", "rate_change_1988", "rate_change_1989")) %>%  
  pivot_longer(cols = starts_with("rate_change_"),  
               names_to = "year",                 
               names_prefix = "rate_change_",      
               values_to = "rate_change") 
  
  
################################################################################
# Plotting the proportion of deforestation -------------------------------------

biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "gray50", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F", "Pampa" = "blue", "Pantanal" = "black")


# plot -------------------------------------------------------------------------

# path to biomes shapefile
# Biome delimitation used by MapBiomas downloaded from https://www.ibge.gov.br/geociencias/informacoes-ambientais/estudos-ambientais/15842-biomas.html?=&t=acesso-ao-produto (2019 version)

biomes_shapefile <- st_read("D:/_Vinicius/Mapas/Biomas brasileiros/IBGE 2019/lm_bioma_250.shp")
biomes_shapefile$Bioma <- c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal")

# Map 

#ggplot() +
#  geom_sf(data = biomes_shapefile, aes(fill = Bioma), color = "black") +
#  scale_fill_manual(values = biome_colors) +
#  theme_void() +
#  theme(legend.position = "none")+
#  annotation_scale(location = "bl", width_hint = 0.5)


# Define the longitude and latitude limits for your data
min_longitude <- -75
max_longitude <- -35
min_latitude <- -35
max_latitude <- 5

ggplot() +
  geom_sf(data = biomes_shapefile, aes(fill = Bioma), color = "black") +
  scale_fill_manual(values = biome_colors) +
  scale_x_continuous(breaks = seq(min_longitude, max_longitude, by = 10)) +
  scale_y_continuous(breaks = seq(min_latitude, max_latitude, by = 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "lightgray", size = 1),
    axis.text = element_text(color = "gray50", margin = margin(t = 15, r = 15, b = 15, l = 15), size = 20),
    axis.ticks = element_line(color = "gray50"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(min_longitude, max_longitude), ylim = c(min_latitude, max_latitude), expand = FALSE)

#ggsave(paste(output, "/map_biomas.jpg", sep = ""), width = 8, height = 7, dpi = 300)


# Line chart

(plot_prop_deforestation <- mtx_defo_long %>%
  ggplot(aes(x  = as.numeric(year), y= prop_defo)) +
  geom_rect(aes(xmin = 1990.2, xmax = 1992.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1993.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2016.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2017.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
  geom_rect(aes(xmin = 2019.2, xmax = 2021.8, ymin = -Inf, ymax = Inf, fill = as.factor("Jair Bolsonaro")), colour = NA) +
  geom_point(aes(color = biome))+
  geom_line(aes(color = biome, group = biome), lwd = 1)+
  scale_color_manual(values = biome_colors, name = "") +
  scale_fill_manual(name = "",
                    values = c("#FFCCCC", "#FFFFE0", 
                               "#F5E5FF", "#FFE5CC", 
                               "gray92",  "#E5FFFF", 
                               "#E5FFE5", "#FFF7E6"),
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1990) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1993) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1995) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2003) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2011) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2016.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1993, 1995, 2003, 2011, 2016.9, 2019, 2022),
    labels = c("1985", "1990", "1993", "1995", "2003", "2011", "2016 (August)", "2019", "2022")) +
  xlab("Year") + 
  ylab("Proportional deforestation of primary vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

#ggsave(paste(output, "/1_prop_defo_excl_Pantanal_Pampa_t-1.png", sep = ""), width = 10, height = 7, dpi = 300)


################################################################################
## Plotting bar graphs per presidential terms ----------------------------------

# Informing percentage of area each biome occupy and creating biome labels

# Original proportion each biome occupy

biome_areas_original_dist <- tibble(biome = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
                                    area = c(49, 13, 24, 10, 2, 2))


# Calculating proportion area biomes currently occupy.

biome_areas_current_dist <- tibble(biome = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
                                   area = Biomes_area$sum_2021)


biome_labels <- c("Amazon", "Atlantic\nForest", "Caatinga", "Cerrado")


# Gray and white plots 

# Mean proportion deforestation

# Collor -----------------------------------------------------------------------

(bar_chart_Collor <- mtx_defo_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


# Itamar Franco ----------------------------------------------------------------

(bar_chart_Itamar <- mtx_defo_long %>% 
   filter(year > 1992 & year < 1995) %>% 
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


# FHC --------------------------------------------------------------------------

(bar_chart_FHC <- mtx_defo_long %>% 
   filter(year >= 1995 & year < 2003) %>%  
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


# Lula -------------------------------------------------------------------------

(bar_chart_Lula <- mtx_defo_long %>% 
   filter(year >= 2003 & year < 2011) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Luiz Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


# Dilma ------------------------------------------------------------------------

(bar_chart_Dilma<- mtx_defo_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


# Temer ------------------------------------------------------------------------

(bar_chart_Temer <- mtx_defo_long %>% 
   filter(year >= 2017 & year < 2019) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 2.5, y = 0.7, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)

# Bolsonaro --------------------------------------------------------------------

(bar_chart_Bolsonaro <- mtx_defo_long %>% 
   filter(year >= 2019 & year <= 2022) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years,
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.7, hjust = 1, label = "Jair Bolsonaro - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.58, 0, 0.7),
                      labels = c(-0.58, 0, 0.7),
                      limits = c(-0.58, 0.7))
)


## Combining plots in a single plot


(all_bar_charts <- plot_grid(plot_grid(bar_chart_Collor, bar_chart_Itamar, 
                                       bar_chart_FHC, bar_chart_Lula, 
                                       nrow = 1, ncol = 4),
                             plot_grid(NULL, bar_chart_Dilma, bar_chart_Temer, 
                                       bar_chart_Bolsonaro, NULL, 
                                       rel_widths = c(0.5, 1, 1, 1, 0.5), nrow = 1),
                             nrow = 2
))

#ggsave(paste(output, "/2_bar_charts_mean_prop_defo_t-1.png", sep = ""), width = 20, height = 7, dpi = 300)


################################################################################
## Plotting bar graphs rate of the prop deforestation --------------------------

# Collor -----------------------------------------------------------------------

(plot_rate_long_Collor <- mtx_defo_rate_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# Itamar Franco ----------------------------------------------------------------

(plot_rate_long_Itamar <- mtx_defo_rate_long %>% 
   filter(year > 1992 & year < 1995) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# FHC --------------------------------------------------------------------------

(plot_rate_long_FHC <- mtx_defo_rate_long %>% 
   filter(year >= 1995 & year < 2003) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# Lula -------------------------------------------------------------------------

(plot_rate_long_Lula <- mtx_defo_rate_long %>% 
   filter(year >= 2003 & year < 2011) %>%  
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Luiz Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# Dilma ------------------------------------------------------------------------

(plot_rate_long_Dilma <- mtx_defo_rate_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# Temer ------------------------------------------------------------------------

(plot_rate_long_Temer <- mtx_defo_rate_long %>% 
   filter(year >= 2017 & year < 2019) %>%    
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                      labels = c(-0.3, 0, 0.33),
                      limits = c(-0.3, 0.33))
)


# Bolsonaro --------------------------------------------------------------------

(plot_rate_long_Bolsonaro <- mtx_defo_rate_long %>% 
   filter(year >= 2019 & year <= 2022) %>%    
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75") +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "red", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = 0.25, hjust = 1, label = "Jair Bolsonaro - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.3, 0, 0.33),
                       labels = c(-0.3, 0, 0.33),
                       limits = c(-0.3, 0.33))
)


## Combining plots in a single plot

(all_bar_charts_rate <- plot_grid(plot_rate_long_Collor, plot_rate_long_Itamar,
                                  plot_rate_long_FHC, plot_rate_long_Lula, plot_rate_long_Dilma,
                                  plot_rate_long_Temer, plot_rate_long_Bolsonaro,
                                  labels = "", ncol = 4, nrow = 2))

#ggsave(paste(output, "/3_bar_charts_rate.png", sep = ""), width = 20, height = 7, dpi = 300)


# Plotting coloured barplots ---------------------------------------------------
################################################################################


colors_presidents <- c("#FFCCCC", "#FFFFE0", 
                       "#F5E5FF", "#FFE5CC", 
                       "gray92",  "#E5FFFF", 
                       "#E5FFE5", "#FFF7E6")



# Mean proportion loss/gains

# Collor -----------------------------------------------------------------------

(bar_chart_Collor <- mtx_loss_gain_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[2], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Itamar Franco ----------------------------------------------------------------

(bar_chart_Itamar <- mtx_loss_gain_long %>% 
   filter(year > 1992 & year < 1995) %>% 
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[3], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# FHC --------------------------------------------------------------------------

(bar_chart_FHC <- mtx_loss_gain_long %>% 
   filter(year >= 1995 & year < 2003) %>%  
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[4], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Lula -------------------------------------------------------------------------

(bar_chart_Lula <- mtx_loss_gain_long %>% 
   filter(year >= 2003 & year < 2011) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[5], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Luiz Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Dilma ------------------------------------------------------------------------

(bar_chart_Dilma<- mtx_loss_gain_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[6], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Temer ------------------------------------------------------------------------

(bar_chart_Temer <- mtx_loss_gain_long %>% 
   filter(year >= 2017 & year < 2019) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[7], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)

# Bolsonaro --------------------------------------------------------------------

(bar_chart_Bolsonaro <- mtx_loss_gain_long %>% 
   filter(year >= 2019 & year <= 2022) %>%   
   group_by(biome) %>%
   mutate(total_prop_defo = sum(prop_defo),
          median_total_prop_defo = median(prop_defo),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_defo/num_years, 
          q1 = quantile(prop_defo, probs = 0.25),
          q3 = quantile(prop_defo, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_defo, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[8], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_defo, yend =  median_total_prop_defo), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Jair Bolsonaro - 4 years", size = 5)+
   scale_y_continuous(breaks = c(-0.017, -0.0085, 0, 0.003),
                      labels = c(-0.017, -0.0085, 0, 0.003),
                      limits = c(-0.017, 0.003))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


## Combining plots in a single plot

(all_bar_charts <- plot_grid(bar_chart_Collor, bar_chart_Itamar, bar_chart_FHC, bar_chart_Lula, bar_chart_Dilma, bar_chart_Temer, bar_chart_Bolsonaro, labels = "", ncol = 4, nrow = 2))

#ggsave(paste(output, "/2_bar_charts_loss_gain_COLOR.png", sep = ""), width = 20, height = 7, dpi = 300)




# Rate of change ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# Collor -----------------------------------------------------------------------

(plot_rate_long_Collor <- mtx_rate_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[2], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Itamar Franco ----------------------------------------------------------------

(plot_rate_long_Itamar <- mtx_rate_long %>% 
   filter(year > 1992 & year < 1995) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[3], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# FHC --------------------------------------------------------------------------

(plot_rate_long_FHC <- mtx_rate_long %>% 
   filter(year >= 1995 & year < 2003) %>% 
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[4], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Lula -------------------------------------------------------------------------

(plot_rate_long_Lula <- mtx_rate_long %>% 
   filter(year >= 2003 & year < 2011) %>%  
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[5], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Luiz Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Dilma ------------------------------------------------------------------------

(plot_rate_long_Dilma <- mtx_rate_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[6], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Temer ------------------------------------------------------------------------

(plot_rate_long_Temer <- mtx_rate_long %>% 
   filter(year >= 2017 & year < 2019) %>%    
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[7], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)


# Bolsonaro --------------------------------------------------------------------

(plot_rate_long_Bolsonaro <- mtx_rate_long %>% 
   filter(year >= 2019 & year <= 2022) %>%    
   group_by(biome) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas_current_dist, by = "biome") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop, fill = biome)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[8], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 15),  
     plot.title = element_text(size = 14)  
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.0055, hjust = 1, label = "Jair Bolsonaro - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      labels = c(-0.0072, -0.0036, 0, 0.0026, 0.0052),
                      limits = c(-0.0072, 0.0052))+
   scale_fill_manual(values = biome_colors)+
   theme(legend.position = "none")
)

(all_bar_charts_rate <- plot_grid(plot_rate_long_Collor, plot_rate_long_Itamar,
                                  plot_rate_long_FHC, plot_rate_long_Lula, plot_rate_long_Dilma,
                                  plot_rate_long_Temer, plot_rate_long_Bolsonaro,
                                  labels = "", ncol = 4, nrow = 2))

#ggsave(paste(output, "/3_bar_charts_rate_COLOR.png", sep = ""), width = 20, height = 7, dpi = 300)








