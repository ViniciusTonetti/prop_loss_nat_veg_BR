# Code to create curves of proportional loss of native vegetation in Brazil through presidential terms since 1985
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

input <- "D:/_Vinicius/artigos/2024.09 loss of habitat presidential terms Brazil/data"
output <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/outputs/prop_loss"


# Loading data -----------------------------------------------------------------
# Data downloaded on 23/08/2024 from MapBiomas collection 9 from the tab "estatísticas" > COBERTURA E TRANSIÇÕES PARA OS BIOMAS (COLEÇÃO 9) – dados de áreas (ha) por classe de cobertura e uso da terra para os biomas para o período de 1985 a 2023 (atualizada em 21/08/2024)

# https://brasil.mapbiomas.org/estatisticas/


MB <- readxl::read_excel(path = paste(input, "/MAPBIOMAS_BRAZIL-COL.9-BIOMES.xlsx", sep = ""), sheet = "COVERAGE_9", )


# Checking and filtering data --------------------------------------------------
################################################################################


colnames(MB)
head(MB)

# Checking unique biome classes
unique(MB$territory)

# Checking unique class 0 values
unique(MB$class_level_0)

# Checking "Natural/Antropic" class 0 - All values correspond to non vegetated areas
MB %>% 
  filter(class_level_0 == "Natural/Antropic") %>% 
  select(class_level_1)


# Filtering, summarizing, and summing values for each year
# First, checking classes considered natural vegetation


# Checking class_level_1 considering natural vegetation only

MB %>% 
  filter(class_level_0 == "Natural") %>% 
  select(class_level_1) %>% 
  distinct()


# excluding from class_level_1: "Water and Marine Environment"

MB <- MB %>% 
  filter(class_level_0 == "Natural") %>% 
  filter(class_level_1 != "5. Water and Marine Environment", class_level_1 != "4. Non vegetated area")


# Checking level_2 class

MB %>% 
  select(class_level_2) %>% 
  distinct()


# excluding from level_2 class: "2.1. Wetland", "2.2. Grassland", "2.4. Rocky Outcrop", "2.3. Hypersaline Tidal Flat", "2.4. Herbaceous Sandbank Vegetation"

MB <- MB %>% 
  filter(class_level_2 != "2.1. Wetland") %>% 
  filter(class_level_2 != "2.2. Grassland") %>% 
  filter(class_level_2 != "2.4. Rocky Outcrop") %>% 
  filter(class_level_2 != "2.3. Hypersaline Tidal Flat") %>%
  filter(class_level_2 != "2.4. Herbaceous Sandbank Vegetation")

# Checking level_3 class

MB %>% 
  select(class_level_3) %>% 
  distinct()


# Checking level_4 class

MB %>% 
  select(class_level_4) %>% 
  distinct()


# class_level_2, 3, and 4 only varies for artificial formations, e.g., agriculture

# Land cover classes excluded:

# "Undefined" (natural, anthropic)
# "Antropic"
# "Natural/Antropic"
# "2.1. Wetland"
# "2.2. Grassland"
# "2.4. Rocky Outcrop"
# "2.3. Hypersaline Tidal Flat"
# "2.4. Herbaceous Sandbank Vegetation"


# Classes considered in this analysis:

# Natural               
# Forest               
# Forest Formation                
# Savanna Formation                           
# Mangrove                  
# Floodable Forest
# Wooded Sandbank Vegetation


# Summing area values for the selected values above ----------------------------

MB_sum <- MB %>% 
  group_by(territory) %>%
  summarise(across(`1985`:`2023`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}"))


# converting from tibble to data frame to change row names

MB_sum <- as.data.frame(MB_sum)
row.names(MB_sum) <- MB_sum[[1]]
MB_sum <- MB_sum[,-1]


# Preparing data to plot ------------------------------------------------------
################################################################################

# First, creating an empty dataframe to be filled with the prop loss/gain of natural vegetation

mtx <- matrix(nrow = nrow(MB_sum), ncol = ncol(MB_sum)-1)
mtx <- as.data.frame(mtx)

row.names(mtx) <- row.names(MB_sum)
colnames(mtx) <- paste("prop_loss_", 1986:2023, sep = "")


# loop to fill the matrix with the prop loss/gain of natural vegetation

for (i in 2:ncol(MB_sum)) {
  mtx[,i-1] <- (MB_sum[,i] - MB_sum[,i-1])/MB_sum[,i-1] # proportional loss in relation to the previous year
}


# Replacing Portuguese names to English names

row.names(mtx)[which(row.names(mtx) == "Amazônia")] <- "Amazon"
row.names(mtx)[which(row.names(mtx) == "Mata Atlântica")] <- "Atlantic Forest"


# Filling the matrix with the change of rate gain/loss native vegetation

mtx_rate <- mtx
colnames(mtx_rate) <- paste("rate_change_", 1986:2023, sep = "")

for (i in 2:ncol(mtx_rate)) {
  mtx_rate[,i] <- mtx[,i] - mtx[,i-1]
}


# Filling the matrix with the change of rate gain/loss native vegetation proportional to the previous year

mtx_rate_prop <- mtx
colnames(mtx_rate_prop) <- paste("rate_change_", 1986:2023, sep = "")

for (i in 2:ncol(mtx_rate_prop)) {
  mtx_rate_prop[,i] <- (mtx[,i] - mtx[,i-1])/mtx[,i]
}



# Creating long data frames ----------------------------------------------------

mtx$territory <- row.names(mtx)

mtx_loss_gain_long <- mtx %>%
  filter(territory != "Pantanal", territory != "Pampa", territory != "Caatinga") %>% 
  pivot_longer(cols = starts_with("prop_loss_"),  
               names_to = "year",                 
               names_prefix = "prop_loss_",      
               values_to = "prop_loss")


mtx_rate$territory <- row.names(mtx_rate)
  
mtx_rate_long <- mtx_rate %>%
  filter(territory != "Pantanal", territory != "Pampa", territory != "Caatinga") %>% 
  pivot_longer(cols = starts_with("rate_change_"),  
               names_to = "year",                 
               names_prefix = "rate_change_",      
               values_to = "rate_change")


mtx_rate_prop$territory <- row.names(mtx_rate_prop)

mtx_rate_prop_long <- mtx_rate_prop %>%
  filter(territory != "Pantanal", territory != "Pampa", territory != "Caatinga") %>% 
  pivot_longer(cols = starts_with("rate_change_"),  
               names_to = "year",                 
               names_prefix = "rate_change_",      
               values_to = "rate_change")


# Excluding data before 1990

mtx_loss_gain_long <- mtx_loss_gain_long %>%
                        filter(!year < 1990)

mtx_rate_long <- mtx_rate_long %>% 
                  filter(!year < 1990)

mtx_rate_prop_long <- mtx_rate_prop_long %>% 
                  filter(!year < 1990)
  

################################################################################
# Plotting the proportion of vegetation loss/gain ------------------------------

biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "white", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F", "Pampa" = "white", "Pantanal" = "white")


# plot -------------------------------------------------------------------------

# path to biomes shapefile
# Biome delimitation used by MapBiomas downloaded from https://www.ibge.gov.br/geociencias/informacoes-ambientais/estudos-ambientais/15842-biomas.html?=&t=acesso-ao-produto (2019 version)

#biomes_shapefile <- st_read("D:/_Vinicius/Mapas/Biomas brasileiros/IBGE 2019/lm_bioma_250.shp")
#biomes_shapefile$Bioma <- c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal")

# Map 

#ggplot() +
#  geom_sf(data = biomes_shapefile, aes(fill = Bioma), color = "black") +
#  scale_fill_manual(values = biome_colors) +
#  theme_void() +
#  theme(legend.position = "none")+
#  annotation_scale(location = "bl", width_hint = 0.5)


# Define the longitude and latitude limits for your data
#min_longitude <- -75
#max_longitude <- -35
#min_latitude <- -35
#max_latitude <- 5

#ggplot() +
#  geom_sf(data = biomes_shapefile, aes(fill = Bioma), color = "black") +
#  scale_fill_manual(values = biome_colors) +
#  scale_x_continuous(breaks = seq(min_longitude, max_longitude, by = 10)) +
#  scale_y_continuous(breaks = seq(min_latitude, max_latitude, by = 10)) +
#  theme_void() +
#  theme(
#    legend.position = "none",
#    panel.grid.major = element_line(color = "lightgray", size = 1),
#    axis.text = element_text(color = "gray50", margin = margin(t = 15, r = 15, b = 15, l = 15), size = 20),
#    axis.ticks = element_line(color = "gray50"),
#    plot.margin = unit(c(1, 1, 1, 1), "cm")
#  ) +
#  annotation_scale(
#    location = "bl", 
#    width_hint = 0.5,
#    text_col = "black",
#    text_cex = 1.5,
    #pad_x = unit(0.5, "cm"),
    #pad_y = unit(0.5, "cm")
#  )+ 
#  coord_sf(xlim = c(min_longitude, max_longitude), ylim = c(min_latitude, max_latitude), expand = FALSE)

#ggsave(paste(output, "/map_biomas_excl_Caatinga.jpg", sep = ""), width = 8, height = 7, dpi = 300)


# Line chart

(plot_prop_loss_gain <- mtx_loss_gain_long %>%
  ggplot(aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1990.2, xmax = 1992.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1993.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2016.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2017.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
  geom_rect(aes(xmin = 2019.2, xmax = 2021.8, ymin = -Inf, ymax = Inf, fill = as.factor("Jair Bolsonaro")), colour = NA) +
  geom_rect(aes(xmin = 2022.2, xmax = 2023.1, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_point(aes(color = territory))+
  geom_line(aes(color = territory, group = territory), lwd = 1)+
  scale_color_manual(values = biome_colors, name = "") +
  scale_fill_manual(name = "",
                    values = c("#E5FFE5", "#FFCCCC",
                               "#F5E5FF", "#FFE5CC", 
                               "gray92",  "#E5FFFF", 
                               "#FFFFE0", "#FFE5CC"),
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
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2023.3) +
  scale_x_continuous(
    breaks = c(1990, 1993, 1995, 2003, 2011, 2016.9, 2019, 2022, 2023.3),
    labels = c("1990", "1993", "1995", "2003", "2011", "2016 (August)", "2019", "2022", "2023")) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))
)

#ggsave(paste(output, "/1_prop_loss_col_9_excl_Caatinga.png", sep = ""), width = 11, height = 7, dpi = 300)
#ggsave(paste(output, "/legend_lines_excl_Caatinga.png", sep = ""), width = 11, height = 7, dpi = 300)


################################################################################
## Plotting bar graphs per presidential terms ----------------------------------

# Informing percentage of area each biome occupy and creating biome labels

# Original proportion each biome occupy

biome_areas_original_dist <- tibble(biome = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
                                    area = c(49, 13, 24, 10, 2, 2))


# Calculating proportion area biomes currently occupy

current_dist_total <- sum(MB_sum[,ncol(MB_sum)])
round((MB_sum[,ncol(MB_sum)]/current_dist_total)*100, digits = 1)

biome_areas_current_dist <- tibble(territory = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
                                   area = round((MB_sum[,ncol(MB_sum)]/current_dist_total)*100, digits = 1))


biome_labels <- c("Amazon", "Atlantic\nForest", "Cerrado")


# Plotting coloured barplots ---------------------------------------------------
################################################################################


colors_presidents <- c("#E5FFE5", "#FFCCCC",
                       "#F5E5FF", "#FFE5CC", 
                       "gray92",  "#E5FFFF", 
                       "#FFFFE0", "#FFE5CC")



# Mean proportion loss/gains

# Collor -----------------------------------------------------------------------

(mean_Collor <- mtx_loss_gain_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[1], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
     )+
   scale_x_discrete(labels = biome_labels)+
   #annotate("text", x = 4.3, y = 0.003, hjust = 1, label = "Fernando Collor - 3 y", size = 6)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
   )


# Itamar Franco ----------------------------------------------------------------

(mean_Itamar <- mtx_loss_gain_long %>% 
   filter(year > 1992 & year < 1995) %>%
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[2], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)
   

# FHC --------------------------------------------------------------------------

(mean_FHC <- mtx_loss_gain_long %>% 
   filter(year >= 1995 & year < 2003) %>%  
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[3], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)


# Lula -------------------------------------------------------------------------

(mean_Lula <- mtx_loss_gain_long %>% 
   filter(year >= 2003 & year < 2011) %>%   
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[4], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)

# Dilma ------------------------------------------------------------------------

(mean_Dilma<- mtx_loss_gain_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[5], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)

# Temer ------------------------------------------------------------------------

(mean_Temer <- mtx_loss_gain_long %>% 
   filter(year >= 2017 & year < 2019) %>%   
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[6], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)

# Bolsonaro --------------------------------------------------------------------

(mean_Bolsonaro <- mtx_loss_gain_long %>% 
   filter(year >= 2019 & year <= 2022) %>%   
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[7], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.020, -0.01, 0, 0.004),
                      labels = c(-0.020, -0.01, 0, 0.004),
                      limits = c(-0.020, 0.004))+
   scale_fill_manual(values = biome_colors)
)

# Lula III ---------------------------------------------------------------------

(mean_Lula_III <- mtx_loss_gain_long %>% 
   filter(year == 2023) %>%   
   group_by(territory) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          mean_rate_prop = total_prop_loss/num_years, 
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   filter(territory != "Pampa", territory != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[8], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.004),
                      labels = c(-0.02, -0.01, 0, 0.004),
                      limits = c(-0.02, 0.004))+
   scale_fill_manual(values = biome_colors)
)

# Rate of change ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# Collor -----------------------------------------------------------------------

(rate_Collor <- mtx_rate_long %>% 
   filter(year > 1990 & year <= 1993) %>% 
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[1], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# Itamar Franco ----------------------------------------------------------------

(rate_Itamar <- mtx_rate_long %>% 
   filter(year > 1993 & year <= 1995) %>% 
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[2], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# FHC --------------------------------------------------------------------------

(rate_FHC <- mtx_rate_long %>% 
   filter(year > 1995 & year <= 2003) %>% 
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[3], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# Lula -------------------------------------------------------------------------

(rate_Lula <- mtx_rate_long %>% 
   filter(year > 2003 & year <= 2011) %>%  
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[4], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# Dilma ------------------------------------------------------------------------

(rate_Dilma <- mtx_rate_long %>% 
   filter(year > 2011 & year <= 2017) %>%   
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[5], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# Temer ------------------------------------------------------------------------

(rate_Temer <- mtx_rate_long %>% 
   filter(year > 2017 & year <= 2019) %>%    
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[6], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


# Bolsonaro --------------------------------------------------------------------

(rate_Bolsonaro <- mtx_rate_long %>% 
   filter(year > 2019 & year <= 2022) %>%    
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[7], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)

# Lula III ---------------------------------------------------------------------

(rate_Lula_III <- mtx_rate_long %>% 
   filter(year > 2022) %>%    
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[8], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     #axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-0.0078, -0.0039, 0, 0.0035),
                      labels = c(-0.0078, -0.0039, 0, 0.0035),
                      limits = c(-0.0078, 0.0035))+
   scale_fill_manual(values = biome_colors)
)


## Combining plots in a single plot

(all_bar_charts <- plot_grid(mean_Collor, rate_Collor,
                             mean_Itamar, rate_Itamar,
                             mean_FHC, rate_FHC,
                             mean_Lula, rate_Lula,
                             mean_Dilma, rate_Dilma,
                             mean_Temer, rate_Temer,
                             mean_Bolsonaro, rate_Bolsonaro,
                             mean_Lula_III, rate_Lula_III,
                             nrow = 8, ncol = 2))

#ggsave(paste(output, "/all_bar_charts_rate_col_9_excl_Caatinga.png", sep = ""), width = 15, height = 30, dpi = 300)
#ggsave(paste(output, "/LEGEND.png", sep = ""), width = 15, height = 30, dpi = 300)


### Plotting the proportional rate of change
# ------------------------------------------------------------------------------

# Collor -----------------------------------------------------------------------

(prop_rate_Collor <- mtx_rate_prop_long %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[1], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


# Itamar Franco ----------------------------------------------------------------

(prop_rate_Itamar <- mtx_rate_prop_long %>% 
    filter(year > 1992 & year < 1995) %>% 
    group_by(territory) %>%
    mutate(total_rate_change = sum(rate_change),
           median_total_rate_change = median(rate_change),
           num_years = n_distinct(year),
           mean_rate_prop = total_rate_change/num_years,
           q1 = quantile(rate_change, probs = 0.25),
           q3 = quantile(rate_change, probs = 0.75)) %>%
    ungroup() %>% 
    mutate(biome_x = as.numeric(factor(territory))) %>% 
    left_join(biome_areas_current_dist, by = "territory") %>%
    distinct(total_rate_change, .keep_all = TRUE) %>%
    ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[2], inherit.aes = FALSE) +
    geom_bar(stat = "identity", aes(width = area / max(area))) +
    geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
    labs(x = "", y = "", title = "") +
    geom_hline(yintercept = 0)+
    theme_classic()+
    theme(
      legend.position = "none",
      text = element_text(size = 0),       
      axis.title = element_text(size = 0), 
      axis.text = element_text(size = 26),
      axis.text.x = element_blank()
    )+
    scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
    scale_fill_manual(values = biome_colors)
)


# FHC --------------------------------------------------------------------------

(prop_rate_FHC <- mtx_rate_prop_long %>% 
   filter(year >= 1995 & year < 2003) %>% 
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[3], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


# Lula -------------------------------------------------------------------------

(prop_rate_Lula <- mtx_rate_prop_long %>% 
   filter(year >= 2003 & year < 2011) %>%  
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[4], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


# Dilma ------------------------------------------------------------------------

(prop_rate_Dilma <- mtx_rate_prop_long %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[5], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


# Temer ------------------------------------------------------------------------

(prop_rate_Temer <- mtx_rate_prop_long %>% 
   filter(year >= 2017 & year < 2019) %>%    
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[6], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


# Bolsonaro --------------------------------------------------------------------

(prop_rate_Bolsonaro <- mtx_rate_prop_long %>% 
   filter(year >= 2019 & year <= 2022) %>%    
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[7], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-35, 0, 35),
                      labels = c(-35, 0, 35),
                      limits = c(-35, 35))+
   scale_fill_manual(values = biome_colors)
)

# Lula III ---------------------------------------------------------------------

(prop_rate_Lula_III <- mtx_rate_prop_long %>% 
   filter(year > 2022) %>%      
   group_by(territory) %>%
   mutate(total_rate_change = sum(rate_change),
          median_total_rate_change = median(rate_change),
          num_years = n_distinct(year),
          mean_rate_prop = total_rate_change/num_years,
          q1 = quantile(rate_change, probs = 0.25),
          q3 = quantile(rate_change, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(territory))) %>% 
   left_join(biome_areas_current_dist, by = "territory") %>%
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = territory, y = mean_rate_prop, fill = territory)) +
   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = colors_presidents[8], inherit.aes = FALSE) +
   geom_bar(stat = "identity", aes(width = area / max(area))) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.9)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.1, color = "black", size = 0.9) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     legend.position = "none",
     text = element_text(size = 0),       
     axis.title = element_text(size = 0), 
     axis.text = element_text(size = 26),
     #axis.text.x = element_blank()
   )+
   scale_x_discrete(labels = biome_labels)+
   scale_y_continuous(breaks = c(-1, 0, 0.6),
                      labels = c(-1, 0, 0.6),
                      limits = c(-1, 0.6))+
   scale_fill_manual(values = biome_colors)
)


## Combining plots in a single plot

(all_bar_charts <- plot_grid(prop_rate_Collor,
                             prop_rate_Itamar,
                             prop_rate_FHC,
                             prop_rate_Lula,
                             prop_rate_Dilma,
                             prop_rate_Temer,
                             prop_rate_Bolsonaro,
                             prop_rate_Lula_III,
                             nrow = 8, ncol = 1))

#ggsave(paste(output, "/prop_rate_change_col_9_excl_Caatinga.png", sep = ""), width = 7.5, height = 30, dpi = 300)







