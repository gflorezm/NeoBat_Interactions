################################################################################
#
# FIGURE 3. Abundance of bats and plants species
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
   group_by(CurrentBatSpecies) %>%
   summarise(Frequency = n()) %>%
   arrange(desc(Frequency))

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

# Make a table with the number of interaction records per plant genus
plantrecords <- records %>% 
   group_by(PlantGenus) %>%
   summarise(Frequency = n()) %>%
   arrange(desc(Frequency))

# Check the data
class(plantrecords)
str(plantrecords)
head(plantrecords)

# Pick only the 15 most frequent species
plantrecords15 <- plantrecords[1:15,]

# Check the data
plantrecords15


################################################################################
##### MAKE THE PLOTS
################################################################################


# Plot the bat bat species
g1 <- ggplot(batrecords15, aes(x = reorder(names, Frequency), y = Frequency)) +
   geom_bar(stat = "identity", color = "Black", fill = "#C59F00") +
   theme_bw() + coord_flip() + ylim(c(0,450)) +
   labs(x = " ", y = "Absolute frequency") +
   theme(axis.text.y = element_text(size = 9, colour = "black", face = "italic",
                                    family = "Arial Narrow"),
         axis.text.x = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.title.x = element_text(size = 10, colour = "black", vjust = -3,
                                     family = "Arial Narrow", face = "bold"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     family = "Arial Narrow", face = "bold"),
         plot.margin = unit(c(1,1,2,2), "lines"))

#See the plot
g1


# Plot the plant genera
g2 <- ggplot(plantrecords15, aes(x = reorder(PlantGenus, Frequency), y = Frequency)) +
   geom_bar(stat = "identity", color = "Black", fill = "#980063") +
   theme_bw() + coord_flip() + ylim(c(0,450)) +
   labs(x = " ", y = "Absolute frequency") + 
   theme(axis.text.y = element_text(size = 9, colour = "black", face = "italic",
                                    family = "Arial Narrow"),
         axis.text.x = element_text(size = 9, colour = "black",
                                    family = "Arial Narrow"),
         axis.title.x = element_text(size = 10, colour = "black", vjust = -3,
                                     family = "Arial Narrow", face = "bold"),
         axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                     family = "Arial Narrow", face = "bold"),
         plot.margin = unit(c(1,1,2,2), "lines"))

#See the plot
g2

# Export both plots together as PNG image
png("./Figures/Figure_3.png", res = 300,
    width = 2000, height = 1200, unit = "px")

# Draw the two plots together with the same size (it only accepts objects of the class Grob)
grid.draw(cbind(ggplotGrob(g1), ggplotGrob(g2), size = "first"))
# Draw the legend
grid.text(label = c("A","B"), x = c(0.03,0.54), y = c(0.96,0.96),
          gp = gpar(fontsize = 14, fontfamily = "Arial Narrow", fontface = "bold"))

dev.off()


################################ END ###########################################
