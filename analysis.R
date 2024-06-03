# Code to create curves of proportional loss of native vegetation in Brazil through presidential terms since 1985
# Vinicius Tonetti  - vrtonetti@gmail.com
# ------------------------------------------------------------------------------

# Cleaning directory
rm(list = ls())

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(readxl)

# Directories ------------------------------------------------------------------

input <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/data"
output <- "D:/_Vinicius/artigos/loss of habitat presidential terms Brazil/outputs"


# Loading data -----------------------------------------------------------------
# Data downloaded on 30/05/2024 from MapBiomas collection 8 from the tab "estatísticas" > COBERTURA E TRANSIÇÕES BIOMA & ESTADOS (COLEÇÃO 8) – dados de área (ha) de cobertura e uso da terra por bioma e estado de 1985 a 2022 (atualizada em 01/09/2023)

# https://brasil.mapbiomas.org/estatisticas/


MB <- readxl::read_excel(path = paste(input, "/TABELA-GERAL-MAPBIOMAS-COL8.0-BIOMASxESTADOS-1.xlsx", sep = ""), sheet = "COBERTURA_COL8.0", )

# Filtering data ---------------------------------------------------------------

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

# excluding from level_2 class: "Rocky outcrop", "Salt flat", "Beach and Dune"  
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


# Classes included in this analysis:
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


# Calculating the proportional loss --------------------------------------------

# First, creating an empty dataframe to be filled
mtx <- matrix(nrow = nrow(MB_sum), ncol = ncol(MB_sum)-1)
mtx <- as.data.frame(mtx)

row.names(mtx) <- row.names(MB_sum)
colnames(mtx) <- paste("prop_loss_", 1986:2022, sep = "")


# loop to fill the matrix
for (i in 2:ncol(MB_sum)) {
  mtx[,i-1] <- (MB_sum[,i] - MB_sum[,i-1])/MB_sum[,"sum_1985"]
}

mtx

# Replacing Portuguese names to English names
row.names(mtx)[which(row.names(mtx) == "Amazônia")] <- "Amazon"
row.names(mtx)[which(row.names(mtx) == "Mata Atlântica")] <- "Atlantic Forest"


################################################################################
# Plotting data ----------------------------------------------------------------

# Converting data from wider to longer
mtx[ ,"biome"] <- row.names(mtx)

mtx_longer <- mtx %>%
  pivot_longer(
    cols = starts_with("prop_loss_"),  
    names_to = "year",                 
    names_prefix = "prop_loss_",      
    values_to = "prop_loss"           
  )


# Plotting the proportion of vegetation loss considering all biomes ------------

biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "gray50", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F", Pampa = "#4477AA", Pantanal = "#B254A5")


ggplot(data = mtx_longer, aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1985.2, xmax = 1989.8, ymin = -Inf, ymax = Inf, fill = as.factor("José Sarney")), colour = NA) +
  geom_rect(aes(xmin = 1990.2, xmax = 1991.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1992.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2015.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2016.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
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
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2015.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1992, 1995, 2003, 2011, 2015.9, 2019, 2022),
    labels = c("1985", "1990", "1992", "1995", "2003", "2011", "2015 (August)", "2019", "2022")) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste(output, "/_1_prop_loss_all_biomes_excl_grass_wet_other.png", sep = ""), width = 10, height = 7, dpi = 300)



# Plotting the proportion of vegetation loss excluding Pantanal ----------------

mtx_longer_no_Pantanal <- mtx_longer %>% 
  filter(biome != "Pantanal")


biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "gray50", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F", Pampa = "#4477AA")


ggplot(data = mtx_longer_no_Pantanal, aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1985.2, xmax = 1989.8, ymin = -Inf, ymax = Inf, fill = as.factor("José Sarney")), colour = NA) +
  geom_rect(aes(xmin = 1990.2, xmax = 1991.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1992.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2015.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2016.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
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
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2015.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1992, 1995, 2003, 2011, 2015.9, 2019, 2022),
    labels = c("1985", "1990", "1992", "1995", "2003", "2011", "2015 (August)", "2019", "2022")) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste(output, "/_2_prop_loss_excl_pantanal_excl_grass_wet_other.png", sep = ""), width = 10, height = 7, dpi = 300)



# Plotting the proportion of vegetation loss excluding Pantanal and Pampa ------

mtx_longer_no_Pantanal_Pampa <- mtx_longer %>% 
  filter(biome != "Pantanal") %>% 
  filter(biome != "Pampa")


biome_colors <- c("Amazon" = "#24693D", "Caatinga" = "gray50", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F")


ggplot(data = mtx_longer_no_Pantanal_Pampa, aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1985.2, xmax = 1989.8, ymin = -Inf, ymax = Inf, fill = as.factor("José Sarney")), colour = NA) +
  geom_rect(aes(xmin = 1990.2, xmax = 1991.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1992.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2015.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2016.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
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
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2015.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1992, 1995, 2003, 2011, 2015.9, 2019, 2022),
    labels = c("1985", "1990", "1992", "1995", "2003", "2011", "2015 (August)", "2019", "2022")) +
  scale_y_continuous(
    breaks = c(-0.02, -0.01, 0, 0.007),
    labels = c(-0.02, -0.01, 0, 0.007)) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste(output, "/_3_prop_loss_excl_pantanal_pampa_excl_grass_wet_other.png", sep = ""), width = 10, height = 7, dpi = 300)



# Plotting the proportion of vegetation loss excluding Pantanal, Pampa, and Caatinga ------

mtx_longer_no_Pantanal_Pampa_Caatinga <- mtx_longer %>% 
  filter(biome != "Pantanal") %>% 
  filter(biome != "Pampa") %>% 
  filter(biome != "Caatinga")


biome_colors <- c("Amazon" = "#24693D", "Cerrado" = "#CCBB44",
                  "Atlantic Forest" = "#DF5E1F")


ggplot(data = mtx_longer_no_Pantanal_Pampa_Caatinga, aes(x  = as.numeric(year), y= prop_loss)) +
  geom_rect(aes(xmin = 1985.2, xmax = 1989.8, ymin = -Inf, ymax = Inf, fill = as.factor("José Sarney")), colour = NA) +
  geom_rect(aes(xmin = 1990.2, xmax = 1991.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Collor")), colour = NA) +
  geom_rect(aes(xmin = 1992.2, xmax = 1994.8, ymin = -Inf, ymax = Inf, fill = as.factor("Itamar Franco")), colour = NA) +
  geom_rect(aes(xmin = 1995.2, xmax = 2002.8, ymin = -Inf, ymax = Inf, fill = as.factor("Fernando Henrique Cardoso")), colour = NA) +
  geom_rect(aes(xmin = 2003.2, xmax = 2010.8, ymin = -Inf, ymax = Inf, fill = as.factor("Luiz Inácio Lula da Silva")), colour = NA) +
  geom_rect(aes(xmin = 2011.2, xmax = 2015.7, ymin = -Inf, ymax = Inf, fill = as.factor("Dilma Rousseff")), colour = NA) +
  geom_rect(aes(xmin = 2016.11, xmax = 2018.8, ymin = -Inf, ymax = Inf, fill = as.factor("Michel Temer")), colour = NA) +
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
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2015.9) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2019) +
  geom_vline(color = "gray70", linetype = "dashed", size = 0.6, xintercept = 2022) +
  scale_x_continuous(
    breaks = c(1985, 1990, 1992, 1995, 2003, 2011, 2015.9, 2019, 2022),
    labels = c("1985", "1990", "1992", "1995", "2003", "2011", "2015 (August)", "2019", "2022")) +
  scale_y_continuous(
    breaks = c(-0.02, -0.01, 0, 0.007),
    labels = c(-0.02, -0.01, 0, 0.007)) +
  xlab("Year") + 
  ylab("Proportional gains or losses of native vegetation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste(output, "/_4_prop_loss_excl_pantanal_pampa_caatinga_excl_grass_wet_other.png", sep = ""), width = 10, height = 7, dpi = 300)



