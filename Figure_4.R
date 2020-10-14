################################################################################
#
# FIGURE 4. Number of recorded interactions by plant life form and interaction type
#
################################################################################


################################################################################
##### SET THE STAGE
################################################################################


#Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Delete all previous objects
rm(list= ls())


################################################################################
##### LOAD THE PACKAGES
################################################################################


library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggpubr)
library(grid)
library(ggpirate)
library(yarrr)


################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with the interaction records and filter out "unidentified"
# plant genera
records <- read.csv("./data/NeoBat_Interactions_records.csv")
records <- records %>% 
      filter(!is.na(LifeForm))

# Check the data
class(records)
str(records)
head(records)

# Make a new table in which each observation represents a plant species,
# and the number of interactions is given by type (nectarivory and frugivory)
# and plant life form.
data <- records %>% 
      group_by(CurrentPlantSpecies,CurrentBatSpecies, 
               LifeForm, Interaction) %>%
      summarise() %>%
      group_by(CurrentPlantSpecies, LifeForm, Interaction) %>%
      summarise(N_interactions = n())

#Check the data
class(data)
str(data)
head(data)


# Make a violin plot with jitter

colors = c("green", "purple")

g1 <- ggplot(data = data, aes(x = Interaction, y = N_interactions, fill = Interaction)) + 
   geom_violin(alpha = 0.1, aes(colour = Interaction,
                                             fill = Interaction)) + 
   facet_wrap(. ~ LifeForm) +
   geom_jitter(alpha = 0.5, aes(colour = Interaction,
                                              fill = Interaction)) +
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

# See the plot
g1


# Export the map as a PNG image
png("./Figures/Figure_4.png", res = 300,
    width = 3000, height = 2000, unit = "px")
g1

dev.off()


################################ END ###########################################
