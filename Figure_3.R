################################################################################
#
# FIGURE 3. 
# 3.A Abundance of the 15 most abundant bats species
# 3.B Number of interactions of the 15 most abundant bats species
# 3.C Abundance of the 15 most abundant plants genus
# 3.D Number of interactions of the 15 most abundant plants species
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
library(cowplot)


################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with the interaction records
records <- read.csv("./Data/NeoBat_Interactions_records.csv")

# Check the data
class(records)
str(records)
head(records)

# Make a table with the number of interaction records per bat species
batrecords <- records %>% 
      dplyr::group_by(CurrentBatSpecies) %>%
      dplyr::summarise(Frequency = n()) %>%
      dplyr::arrange(desc(Frequency))

# Check the data
class(batrecords)
str(batrecords)
head(batrecords)

# Pick only the 15 most frequent species
batrecords15 <- batrecords[1:15,]

# Check the data
batrecords15

# Load a custom-made function to abbreviate the scientific names

source("abbr_name.R")

batrecords15$names <- abbr_name(batrecords15$CurrentBatSpecies)

# Check the names
batrecords15$names


# Make the table with the number of interactions of each bat species
batdegree <- records %>% 
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::group_by(CurrentBatSpecies, CurrentPlantSpecies) %>%
      dplyr::summarise(Frequency = n()) %>%                  
      dplyr::group_by(CurrentBatSpecies) %>%
      dplyr::summarise(Degree = n()) %>%                     
      dplyr::arrange(desc(Degree))               

# Check the data
class(batdegree)
str(batdegree)
head(batdegree)

# Pick the 15 species with most interactions
batdegree15 <- batdegree[1:15,]

# Check the data
batdegree15

# Abbreviate the scientific names
batdegree15$Bat <- abbr_name(batdegree15$CurrentBatSpecies)

# Check the names
batdegree15$Bat


# Make a table with the number of interaction records per plant genus
plantrecords <- records %>% 
      dplyr::group_by(PlantGenus) %>%
      dplyr::summarise(Frequency = n()) %>%
      dplyr::arrange(desc(Frequency))

# Check the data
class(plantrecords)
str(plantrecords)
head(plantrecords)

# Pick only the 15 most frequent species
plantrecords15 <- plantrecords[1:15,]

# Check the data
plantrecords15


# Make the table with the number of interactions of each plant species
plantdegree <-records %>% 
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::group_by(CurrentBatSpecies, CurrentPlantSpecies) %>%
      dplyr::summarise(Frequency = n()) %>%                 
      dplyr::group_by(CurrentPlantSpecies) %>%
      dplyr::summarise(Degree = n()) %>%                     
      dplyr::arrange(desc(Degree))

# Check the data
class(plantdegree)
str(plantdegree)
head(plantdegree)

# Pick the 15 species with most interactions
plantdegree15 <- plantdegree[1:15,]

# Check the data
plantdegree15

# Abbreviate the scientific names
plantdegree15$Plant <- abbr_name(plantdegree15$CurrentPlantSpecies)

# Check the names
plantdegree15$Plant


################################################################################
##### MAKE THE PLOTS
################################################################################


# Plot the bat species abundance
g1 <- ggplot(batrecords15, aes(x = reorder(names, -Frequency), y = Frequency)) +
      geom_bar(stat = "identity", color = "Black", fill = "#C59F00") +
      theme_bw() + 
      ylim(c(0,450)) +
      labs(x = " ", y = "Absolute frequency") +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(size = 9, colour = "black",
                                       face = "italic", angle = 80,
                                       vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 9, colour = "black"),
            axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,1,1), "lines"))

#See the plot
g1

# Plot the bat species degree (number of interactions)
batdp <- ggplot(batdegree15, aes(x = reorder(Bat, Degree), y = Degree)) + 
      geom_bar(stat = "identity", color = "Black", fill = "#C59F00") +
      theme_bw() +
      coord_flip() +
      labs(x = " ", y = "Number of interactions (plant species)") +
      theme(rect = element_rect(fill = "transparent", colour = "NA"),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7, colour = "black"),
            axis.text.y = element_text(size = 7, colour = "black",
                                       face = "italic"),
            axis.title.x = element_text(size = 7, colour = "black", vjust = -3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,1,1), "lines"))

# See the plot
batdp

# Plot the plant genera abundance
g2 <- ggplot(plantrecords15, aes(x = reorder(PlantGenus, -Frequency), 
                                 y = Frequency)) +
      geom_bar(stat = "identity", color = "Black", fill = "#980063") +
      theme_bw() + 
      ylim(c(0,450)) +
      labs(x = " ", y = "Absolute frequency") + 
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(size = 9, colour = "black",
                                       face = "italic", angle = 80,
                                       vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 9, colour = "black"),
            axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,1,1), "lines"))

#See the plot
g2


# Plot the plant species degree (number of interactions)
plantdp <- ggplot(plantdegree15, aes(x = reorder(Plant, Degree), y = Degree)) +
      geom_bar(stat = "identity", color = "Black", fill = "#980063") +
      theme_bw() +
      coord_flip() +
      labs(x = " ", y = "Number of interactions (bat species)") +
      theme(rect = element_rect(fill = "transparent", colour = "NA"),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7, colour = "black"),
            axis.text.y = element_text(size = 7, colour = "black", 
                                       face = "italic"),
            axis.title.x = element_text(size = 7, colour = "black", vjust = -3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,1,1), "lines"))

#See the plot
plantdp


# Export both plots together as PNG image
png("./Figures/Figure_3.png", res = 200,
    width = 2000, height = 1400, unit = "px")

# Draw all the plots together:
# Number of interactions plot will be inside of the abundance plots

cowplot::plot_grid(
      cowplot::ggdraw(g1) +
            cowplot::draw_plot(batdp, 0.40, 0.47, 0.55, 0.5) +
            cowplot::draw_plot_label(c("A", "B"), c(0, 0.42), c(0.98, 0.96),
                                     size = 12, family = NULL),
      cowplot::ggdraw(g2) +
            cowplot::draw_plot(plantdp, 0.40, 0.47, 0.55, 0.5) +
            cowplot::draw_plot_label(c("C", "D"), c(0, 0.42), c(0.98, 0.96),
                                     size = 12, family = NULL)
)

dev.off()


################################ END ###########################################
