#### PROF MARCO ######

# Não sei se deixar o gráfico do grau dos morcegos e as plantas como um gráfico aparte
# ou se juntar ao gráfico 3 e deixar 4 plots no mesmo grid.

#####################################

### PREPARE THE LIBRARY ###
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggpubr)
library(grid)


### PREPARE THE DATA ###

# Read the Records matrix and filter the "unidentified" genus of plants
Records <- read.csv("./Data/NeoBat_Interactions_Records.csv")
Records <- Records %>% 
      filter(!PlantGenus == "Unidentified")


# Accout the number of inteactions of each bat species
BatDegree <- Records %>% 
      group_by(CurrentBatSpecies, CurrentPlantSpecies) %>%
      summarise(Frequency = n()) %>%                  # First organize the unique pair of interactions
      group_by(CurrentBatSpecies) %>%
      summarise(Degree = n()) %>%                     # Then count the degree of each bat
      arrange(desc(Degree))                           # and arrange it in descending order

# I will plot the first 15 species
BatDegree15 <- BatDegree[1:15,]

# I will use the Abr_name() function I've created previously to abreviate the
# scientific name and store it in another column
BatDegree15$Bat <- abr_name(BatDegree15$CurrentBatSpecies)



# The same for plant species

PlantDegree <- Records %>% 
      group_by(CurrentBatSpecies, CurrentPlantSpecies) %>%
      summarise(Frequency = n()) %>%                 
      group_by(CurrentPlantSpecies) %>%
      summarise(Degree = n()) %>%                     
      arrange(desc(Degree))                           

# I will plot the first 15 species
PlantDegree15 <- PlantDegree[1:15,]

# I will use the Abr_name() function I've created previously to abreviate the
# scientific name and store it in another column
PlantDegree15$Plant <- abr_name(PlantDegree15$CurrentPlantSpecies)



### PLOTS ###

loadfonts(device = "win") # To set the fonts we'll use

# Make the bar plot for the bat species
g5 <- ggplot(BatDegree15, aes(x = reorder(Bat, -Degree), y = Degree)) +
      geom_bar(stat = "identity", color = "Black", fill = "#C59F00") +
      theme_bw() +
      labs(x = " ", y = "Number of interactions (plant species)") +
      theme(axis.text.y = element_text(size = 9, colour = "black",
                                       family = "Arial Narrow"),
            axis.text.x = element_text(size = 9, colour = "black", face = "italic",
                                       family = "Arial Narrow", angle = 60,
                                       vjust = 1, hjust = 1),
            axis.title.x = element_text(size = 10, colour = "black", vjust = -3,
                                        family = "Arial Narrow", face = "bold"),
            axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                        family = "Arial Narrow", face = "bold"),
            plot.margin = unit(c(1,1,2,1), "lines"))



# Make the bar plot for the plant genus
g6 <- ggplot(PlantDegree15, aes(x = reorder(Plant, -Degree), y = Degree)) +
      geom_bar(stat = "identity", color = "Black", fill = "#980063") +
      theme_bw() +
      labs(x = " ", y = "Number of interactions (bat species)") +
      theme(axis.text.y = element_text(size = 9, colour = "black",
                                       family = "Arial Narrow"),
            axis.text.x = element_text(size = 9, colour = "black", face = "italic",
                                       family = "Arial Narrow", angle = 60,
                                       vjust = 1, hjust = 1),
            axis.title.x = element_text(size = 10, colour = "black", vjust = -3,
                                        family = "Arial Narrow", face = "bold"),
            axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                        family = "Arial Narrow", face = "bold"),
            plot.margin = unit(c(1,1,2,1), "lines"))


# print them together as a .png
png("Figure_IND.png", res = 400,
    width = 20, height = 10, unit = "cm")

# Draw the two plots together with the same size (only acept objets of class Gorb)
grid.draw(cbind(ggplotGrob(g5), ggplotGrob(g6), size = "first"))
# Draw the legend
grid.text(label = c("A","B"), x = c(0.03,0.54), y = c(0.96,0.96),
          gp = gpar(fontsize = 14, fontfamily = "Arial Narrow", fontface = "bold"))

dev.off()


### END ###