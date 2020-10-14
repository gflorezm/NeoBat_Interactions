# PROF. ESSE SCRIPT É UM BORRADOR

# A ideia é selecionar o tipo de gráfico que irá nesta seção

# a seção visa comparar a distribuição do número de interações das plantas de acordo as
# caeaterísticas ecológicas (Forma de vida, Estagio sucessional [falta]) e dos animais
# de acordo ao grupo trófico.

# Eu particularmente gostei mais do Violin with jitter, é mais limpo e da para ver melhor a distribuição
# das observações.


### PREPARE THE LIBRARY ###

#devtools::install_github("mikabr/ggpirate") # This package came from a github repo

library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggpubr)
library(grid)
library(ggpirate)

### PREPARE THE DATA ###

# Read the Records matrix and filter the "unidentified" genus of plants
Records <- read.csv("./Data/NeoBat_Interactions_Records.csv")
Records <- Records %>% 
      filter(!is.na(LifeForm))


# In this new data frame each observation is a plan species, then we have the 
# number of interactions by type (nectarivory and frugivory) and trophyc guild of bats.
DATA <- Records %>% 
      group_by(CurrentPlantSpecies,CurrentBatSpecies, 
               LifeForm, Interaction) %>%
      summarise() %>%
      group_by(CurrentPlantSpecies, LifeForm, Interaction) %>%
      summarise(N_interactions = n())


# Pirate plot

gp <- ggplot(data = DATA, aes(x = Interaction, y = N_interactions)) + 
   geom_pirate(aes(colour = Interaction, fill = Interaction),
               show.legend = FALSE, cis = FALSE,
               points_params = list(shape = 19, alpha = 0.2)) +
   theme_bw() +
   facet_wrap(. ~ LifeForm) +
   # Customize the theme
   scale_color_manual(values = c("#C59F00","#980063")) +
   scale_fill_manual(values = c("#C59F00","#980063")) +
   labs(x = " ", y = "Number of interactions (bat species)") + 
   theme(axis.text.y = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.text.x = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     family = "Arial Narrow", face = "bold"),
         #the text of the grey panel
         strip.text = element_text(size = 10, colour = "black",
                                   family = "Arial Narrow", face = "bold"),
         plot.margin = unit(c(1,1,2,1), "lines"))


# Violin with jitter

gp2 <- ggplot(data = DATA, aes(x = Interaction, y = N_interactions)) + 
   geom_violin(colour = "#006154", fill = "#006154", alpha = 0.2, width = 0.9) + 
   facet_wrap(. ~ LifeForm) +
   geom_jitter(colour = "#006154", fill = "#006154", alpha = 0.5, width = 0.15) +
   theme_bw() + 
   labs(x = " ", y = "Number of interactions (bat species)") +
   theme(axis.text.y = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.text.x = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     family = "Arial Narrow", face = "bold"),
         #the text of the grey panel
         strip.text = element_text(size = 10, colour = "black",
                                   family = "Arial Narrow", face = "bold"),
         plot.margin = unit(c(1,1,2,1), "lines"))
   