################################################################################
#
# FIGURE 4 
# A. Number of interactions by plant life form and interaction type
# B. Number of interactions by plant successional stage and interaction type
# C. Number of interactions by bat trophic guild and interaction type
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

if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyr)) install.packages('tidyr')
if (!require(cowplot)) install.packages('cowplot')

################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with the interaction records and filter out "unidentified"
# plant genera
records <- read.csv("../data/NeoBat_Interactions_records.csv")

# Check the data
class(records)
str(records)
head(records)

# Make a tibble in which each observation represents a plant species,
# and the number of interactions is given by type (nectarivory and frugivory)
# and plant life form.
lifeform <- records %>%
   dplyr::filter(!is.na(LifeForm)) %>%
   dplyr::group_by(CurrentPlantSpecies,CurrentBatSpecies, 
                   LifeForm, Interaction) %>%
   dplyr::summarise() %>%
   dplyr::group_by(CurrentPlantSpecies, LifeForm, Interaction) %>%
   dplyr::summarise(N_interactions = n())

#Check the data
class(lifeform)
str(lifeform)
lifeform

# Make another tibble in which each observation represents a plant species,
# and the number of interactions is given by type (nectarivory and frugivory)
# and plant successional stage.
sstage <- records %>%
   dplyr::filter(!is.na(SuccessionalStage)) %>%
   dplyr::group_by(CurrentPlantSpecies,CurrentBatSpecies, 
                   SuccessionalStage, Interaction) %>%
   dplyr::summarise() %>%
   dplyr::group_by(CurrentPlantSpecies, SuccessionalStage, Interaction) %>%
   dplyr::summarise(N_interactions = n())

#Check the data
class(sstage)
str(sstage)
sstage

# Make a third tibble in which each observation represents a bat species,
# and the number of interactions is given by type (nectarivory and frugivory)
# and bat trophic guild.
batguild <- records %>%
   dplyr::filter(!is.na(TrophicGuild)) %>%
   dplyr::group_by(CurrentPlantSpecies,CurrentBatSpecies, 
                   TrophicGuild, Interaction) %>%
   dplyr::summarise() %>%
   dplyr::group_by(CurrentBatSpecies, TrophicGuild, Interaction) %>%
   dplyr::summarise(N_interactions = n())

#Check the data
class(batguild)
str(batguild)
batguild

# Make a violin plot with jitter, showing the number of interactions by
# plant life form and interaction type
p1 <- ggplot(data = lifeform, aes(x = Interaction, y = N_interactions)) + 
   geom_violin(alpha = 0.3,colour = "white",
               fill = "#980063") + 
   facet_wrap(. ~ LifeForm, nrow = 3) +
   geom_jitter(alpha = 0.5, colour = "#980063",
               fill = "#980063") +
   theme_bw() +
   ylim(0,24) +
   labs(x = " ", y = "Number of interactions", 
        title = "A. Number of interactions of plant species by life form") +
   theme(panel.grid = element_blank(),
         title = element_text(size = 10, colour = "black", vjust = 3,
                              face = "bold"),
         axis.text.y = element_text(size = 9, colour = "black"),
         axis.text.x = element_text(size = 9, colour = "black"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     face = "bold"),
         #the text of the grey panel
         strip.text = element_text(size = 10, colour = "black",
                                   face = "bold"),
         plot.margin = unit(c(1,1,0.4,1), "lines"))

# See the plot
p1

# Make a violin plot with jitter, showing the number of interactions by
# plant successional stage and interaction type
p2 <- ggplot(data = sstage, aes(x = Interaction, y = N_interactions)) + 
   geom_violin(alpha = 0.3,colour = "white",
               fill = "#980063") + 
   facet_wrap(. ~ SuccessionalStage) +
   geom_jitter(alpha = 0.5, colour = "#980063",
               fill = "#980063") +
   theme_bw() + 
   ylim(0,24) +
   labs(x = " ", y = "Number of interactions", 
        title = "B. Number of interactions of plant species by succesional stage") +
   theme(panel.grid = element_blank(),
         title = element_text(size = 10, colour = "black", vjust = 3,
                              face = "bold"),
         axis.text.y = element_text(size = 9, colour = "black"),
         axis.text.x = element_text(size = 9, colour = "black"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     face = "bold"),
         #the text of the grey panel
         strip.text = element_text(size = 10, colour = "black",
                                   face = "bold"),
         plot.margin = unit(c(0.4,1,0.5,1), "lines"))

# See the plot
p2

# Make a violin plot with jitter, showing the number of interactions by
# bat trophic guild and interaction type
p3 <- ggplot(data = batguild, aes(x = Interaction, y = N_interactions)) + 
   geom_violin(alpha = 0.3,colour = "white",
               fill = "#C59F00") + 
   facet_wrap(. ~ TrophicGuild) +
   geom_jitter(alpha = 0.5, colour = "#C59F00",
               fill = "#C59F00") +
   theme_bw() + 
   labs(x = " ", y = "Number of interactions", 
        title = "C. Number of interactions of bat species by trophic guild") +
   theme(panel.grid = element_blank(),
         title = element_text(size = 10, colour = "black", vjust = 3,
                              face = "bold"),
         axis.text.y = element_text(size = 9, colour = "black"),
         axis.text.x = element_text(size = 9, colour = "black"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     face = "bold"),
         #the text of the grey panel
         strip.text = element_text(size = 10, colour = "black",
                                   face = "bold"),
         plot.margin = unit(c(0.4,1,1,1), "lines"))

# See the plot
p3


# Export all three plots as a PNG image
png("../Figures/Figure_4.png", res = 300,
    width = 2000, height = 3200, unit = "px")

cowplot::plot_grid(p1, p2, p3, nrow = 3, 
                   rel_heights = c(2.6,1,1.8))

dev.off()


################################ END ###########################################