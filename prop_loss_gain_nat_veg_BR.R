# Code to create curves of proportional loss of native vegetation in Brazil through presidential terms since 1985
# Vinicius Tonetti  - vrtonetti@gmail.com
# ------------------------------------------------------------------------------

# Cleaning directory
rm(list = ls())


# Packages ---------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(cowplot)


# Directories ------------------------------------------------------------------

input <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/data"
output <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/outputs"


# Loading data -----------------------------------------------------------------
# Data downloaded on 30/05/2024 from MapBiomas collection 8 from the tab "estatísticas" > COBERTURA E TRANSIÇÕES BIOMA & ESTADOS (COLEÇÃO 8) – dados de área (ha) de cobertura e uso da terra por bioma e estado de 1985 a 2022 (atualizada em 01/09/2023)

# https://brasil.mapbiomas.org/estatisticas/

MB <- readxl::read_excel(path = paste(input, "/TABELA-GERAL-MAPBIOMAS-COL8.0-BIOMASxESTADOS-1.xlsx", sep = ""), sheet = "COBERTURA_COL8.0", )


# Checking and filtering data --------------------------------------------------
################################################################################

# Checking unique biome classes
unique(MB$biome)

# Filtering, summarizing, and summing values for each year
# First, checking classes considered natural vegetation

# Checking level_1 class considering natural vegetation only

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


# excluding from level_2 class: "Rocky outcrop", "Salt flat", "Beach and Dune", "Wetland", "Grassland", "Other Non Forest Natural Formation"

MB <- MB %>% 
  filter(level_2 != "Rocky outcrop") %>% 
  filter(level_2 != "Salt flat") %>% 
  filter(level_2 != "Beach and Dune") %>% 
  filter(level_2 != "Wetland") %>%
  filter(level_2 != "Grassland") %>%
  filter(level_2 != "Other Non Forest Natural Formation")


# Checking level_3 class

MB %>% 
  select(level_3) %>% 
  distinct()


# Checking level_4 class

MB %>% 
  select(level_4) %>% 
  distinct()


# Classes level_2, 3, and 4 only varies for artificial formations, e.g., agriculture

# Land cover classes excluded:

# Water
# Rocky outcrop
# Salt flat
# Beach and Dune
# "Wetland"
# "Grassland"
# "Other Non Forest Natural Formation"


# Classes considered in this analysis:
# Forest Formation                  
# Savanna Formation                 
# Flooded Forest                    
# Magrove                           
# Wooded Restinga                   
# Restinga Herbácea/Arbustiva


# Summing area values for the selected values above ----------------------------

MB_sum <- MB %>% 
  filter(level_0 == "Natural") %>% 
  group_by(biome) %>%
  summarise(across(`1985`:`2022`, ~sum(.x, na.rm = TRUE), .names = "sum_{.col}"))


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
colnames(mtx) <- paste("prop_loss_", 1986:2022, sep = "")


# loop to fill the matrix with the prop loss/gain of natural vegetation

for (i in 2:ncol(MB_sum)) {
  mtx[,i-1] <- (MB_sum[,i] - MB_sum[,i-1])/MB_sum[,"sum_1985"]
}


# Replacing Portuguese names to English names

row.names(mtx)[which(row.names(mtx) == "Amazônia")] <- "Amazon"
row.names(mtx)[which(row.names(mtx) == "Mata Atlântica")] <- "Atlantic Forest"

# Filling the prop of loss/gain in 1985 with zeros
mtx$prop_loss_1985 <- c(0, 0, 0, 0, 0, 0)

mtx <- mtx[,paste("prop_loss_", 1985:2022, sep = "")]


# Filling the matrix with the change of rate gain/loss native vegetation

mtx_rate <- mtx
colnames(mtx_rate) <- paste("rate_change_", 1985:2022, sep = "")

for (i in 2:ncol(mtx_rate)) {
  mtx_rate[,i] <- mtx[,i] - mtx[,i-1]
}


################################################################################
# Plotting the proportion of vegetation loss/gain ------------------------------

biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "gray50", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F")

# Converting data from wider to longer
mtx[ ,"biome"] <- row.names(mtx)

(plot_prop_loss_gain <- mtx %>%
  pivot_longer(cols = starts_with("prop_loss_"),  
               names_to = "year",                 
               names_prefix = "prop_loss_",      
               values_to = "prop_loss") %>% 
  filter(biome != "Pantanal", biome != "Pampa") %>%
  ggplot(aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1985.2, xmax = 1989.8, ymin = -Inf, ymax = Inf, fill = as.factor("José Sarney")), colour = NA) +
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
                    values = c("gray80", "#DFE3E8",
                               "#FFE5E0", "gray90", 
                               "#E3CCCD", "#DFF7D3",
                               "#FEFED9", "#B4D4DA"),
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1990) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1992) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 1995) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2003) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2011) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2016.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1992, 1995, 2003, 2011, 2016.9, 2019, 2022),
    labels = c("1985", "1990", "1992", "1995", "2003", "2011", "2016 (August)", "2019", "2022")) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

#ggsave(paste(output, "/1_prop_loss_excl_pantanal_pampa_excl_grass_wet_other.png", sep = ""), width = 10, height = 7, dpi = 300)


################################################################################
## Plotting bar graphs per presidential terms ----------------------------------

# Informing percentage of area each biome occupy

biome_areas <- tibble(
  biome = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
  area = c(49, 13, 24, 10, 2, 2)
)


## Plotting bar charts proportion loss/ gain -----------------------------------

biome_labels <- c("Amazon", "Atlantic\nForest", "Caatinga", "Cerrado")

# Sarney -----------------------------------------------------------------------

(bar_chart_Sarney <- mtx_longer %>% 
  filter(year >= 1985 & year < 1990) %>% 
  group_by(biome) %>%
  mutate(total_prop_loss = sum(prop_loss),
         median_total_prop_loss = median(prop_loss),
         num_years = n_distinct(year),
         q1 = quantile(prop_loss, probs = 0.25),
         q3 = quantile(prop_loss, probs = 0.75)) %>%
  ungroup() %>% 
  mutate(biome_x = as.numeric(factor(biome))) %>% 
  left_join(biome_areas, by = "biome") %>%
    filter(biome != "Pampa", biome != "Pantanal") %>% 
    distinct(total_prop_loss, .keep_all = TRUE) %>%
    ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
    geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
    geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
    labs(x = "", y = "", title = "") +
    geom_hline(yintercept = 0)+
    theme_classic()+
    theme(
      text = element_text(size = 0),       # Adjusts the base font size
      axis.title = element_text(size = 0), # Adjusts the font size of axis titles
      axis.text = element_text(size = 15),  # Adjusts the font size of axis text
      plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
    )+
    scale_x_discrete(labels = biome_labels)+
    annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "José Sarney - 5 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Collor -----------------------------------------------------------------------

(bar_chart_Collor <- mtx_longer %>% 
   filter(year >= 1990 & year <= 1992) %>% 
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Itamar Franco ----------------------------------------------------------------

(bar_chart_Itamar <- mtx_longer %>% 
   filter(year > 1992 & year < 1995) %>% 
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# FHC --------------------------------------------------------------------------

(bar_chart_FHC <- mtx_longer %>% 
   filter(year >= 1995 & year < 2003) %>%  
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Lula -------------------------------------------------------------------------

(bar_chart_Lula <- mtx_longer %>% 
   filter(year >= 2003 & year < 2011) %>%   
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Luis Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Dilma ------------------------------------------------------------------------

(bar_chart_Dilma<- mtx_longer %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Temer ------------------------------------------------------------------------

(bar_chart_Temer <- mtx_longer %>% 
   filter(year >= 2011 & year < 2017) %>%   
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


# Bolsonaro --------------------------------------------------------------------

(bar_chart_Bolsonaro <- mtx_longer %>% 
   filter(year >= 2019 & year <= 2022) %>%   
   group_by(biome) %>%
   mutate(total_prop_loss = sum(prop_loss),
          median_total_prop_loss = median(prop_loss),
          num_years = n_distinct(year),
          q1 = quantile(prop_loss, probs = 0.25),
          q3 = quantile(prop_loss, probs = 0.75)) %>%
   ungroup() %>% 
   mutate(biome_x = as.numeric(factor(biome))) %>% 
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_prop_loss, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = total_prop_loss/num_years)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_prop_loss, yend =  median_total_prop_loss), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.013, hjust = 1, label = "Jair Bolsonaro - 4 years", size = 5)+
   scale_y_continuous(breaks = c(-0.014, -0.008, 0, 0.003) ,labels = c(-0.014, -0.008, 0, 0.003), limits = c(-0.014, 0.003))
)


## Combining plots in a single plot

(all_bar_charts <- plot_grid(bar_chart_Sarney, bar_chart_Collor, bar_chart_Itamar, bar_chart_FHC, bar_chart_Lula, bar_chart_Dilma, bar_chart_Temer, bar_chart_Bolsonaro, labels = "", ncol = 4, nrow = 2))

#ggsave(paste(output, "/3_bar_charts_mean_prop_loss_gain.png", sep = ""), width = 20, height = 7, dpi = 300)



################################################################################
## Plotting bar graphs rate of the prop loss/increase --------------------------

# Informing percentage of area each biome occupy

biome_areas <- tibble(
  biome = c("Amazon", "Caatinga", "Cerrado", "Atlantic Forest", "Pampa", "Pantanal"),
  area = c(49, 13, 24, 10, 2, 2)
)



## Plotting bar charts rate proportion loss/ gain ------------------------------

biome_labels <- c("Amazon", "Atlantic\nForest", "Caatinga", "Cerrado")

# Sarney -----------------------------------------------------------------------

(plot_rate_long_Sarney <- mtx_rate_long %>% 
  filter(year >= 1985 & year < 1990) %>% 
  group_by(biome) %>%
  mutate(total_rate_change = sum(rate_change),
         median_total_rate_change = median(rate_change),
         num_years = n_distinct(year),
         mean_rate_prop = total_rate_change/num_years,
         q1 = quantile(rate_change, probs = 0.25),
         q3 = quantile(rate_change, probs = 0.75)) %>%
  ungroup() %>% 
  mutate(biome_x = as.numeric(factor(biome))) %>% 
  left_join(biome_areas, by = "biome") %>%
    filter(biome != "Pampa", biome != "Pantanal") %>% 
    distinct(total_rate_change, .keep_all = TRUE) %>%
    ggplot(aes(x = biome, y = mean_rate_prop)) +
    geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
    geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
    labs(x = "", y = "", title = "") +
    geom_hline(yintercept = 0)+
    theme_classic()+
    theme(
      text = element_text(size = 0),       # Adjusts the base font size
      axis.title = element_text(size = 0), # Adjusts the font size of axis titles
      axis.text = element_text(size = 15),  # Adjusts the font size of axis text
      plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
    )+
    scale_x_discrete(labels = biome_labels)+
    annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "José Sarney - 5 years", size = 5)+
    scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
)


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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Fernando Collor - 3 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Itamar Franco - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Fernando Henrique Cardoso - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Luís Inácio Lula da Silva - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Dilma Rousseff - 6 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Michel Temer - 2 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
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
   left_join(biome_areas, by = "biome") %>%
   filter(biome != "Pampa", biome != "Pantanal") %>% 
   distinct(total_rate_change, .keep_all = TRUE) %>%
   ggplot(aes(x = biome, y = mean_rate_prop)) +
   geom_bar(stat = "identity", aes(width = area / max(area)), fill = "gray75", position = position_dodge(width = 0.1)) +
   geom_segment(aes(x = biome_x - (area / max(area))/2, xend = biome_x +(area / max(area))/2, y = median_total_rate_change, yend =  median_total_rate_change), color = "black", size = 0.6)+
   geom_errorbar(aes(ymin = q1, ymax = q3), width = 0, color = "red", size = 0.6) +
   labs(x = "", y = "", title = "") +
   geom_hline(yintercept = 0)+
   theme_classic()+
   theme(
     text = element_text(size = 0),       # Adjusts the base font size
     axis.title = element_text(size = 0), # Adjusts the font size of axis titles
     axis.text = element_text(size = 15),  # Adjusts the font size of axis text
     plot.title = element_text(size = 14)  # Adjusts the font size of the plot title
   )+
   scale_x_discrete(labels = biome_labels)+
   annotate("text", x = 4.3, y = -0.004, hjust = 1, label = "Jair Bolsonaro - 8 years", size = 5)+
   scale_y_continuous(breaks = c(-0.0056, -0.0025, 0, 0.0025, 0.0052) ,labels = c(-0.0056, -0.0025, 0, 0.0025, 0.0052), limits = c(-0.0056, 0.0052))
)


## Combining plots in a single plot

(all_bar_charts_rate <- plot_grid(plot_rate_long_Sarney, plot_rate_long_Collor, plot_rate_long_Itamar, plot_rate_long_FHC,
                                  plot_rate_long_Lula, plot_rate_long_Dilma, plot_rate_long_Temer, plot_rate_long_Bolsonaro,                                         labels = "", ncol = 4, nrow = 2))

#ggsave(paste(output, "/4_bar_charts_rate.png", sep = ""), width = 20, height = 7, dpi = 300)



  
  