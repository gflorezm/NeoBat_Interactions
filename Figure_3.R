####################################################
#
# FIGURE 3. Abundance of bats and plants species
#
####################################################



### PREPARE THE LIBRARY ###
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggpubr)
library(grid)


### PREPARE THE DATA ###

# Read the Records matrix 
Records <- read.csv("./Data/NeoBat_Interactions_Records.csv")

# Separate the number of bat records
BatRecords <- Records %>% 
   group_by(CurrentBatSpecies) %>%
   summarise(Frequency = n()) %>%
   arrange(desc(Frequency))

# We will plot the first 15 species
Bats15 <- BatRecords[1:15,]


# We'll create a function to abbreviate the scientific name

abr_name <- function(X) {
   
   # a vector to store the result
   Abr_name <- character(length = length(X))
   
   # a list with the names separates in genus and epithet
   lista <- strsplit(X, " ")
   
   # pick the first letter of the genus and paste it with the epithet
   for (i in 1:length(X)) {
      X[i] <- if(lista[[i]][2] == "sp.") {
         paste(lista[[i]][1], 
               lista[[i]][2], sep = " ") 
      } else {
         paste(substr(lista[[i]][1],1,1), 
               lista[[i]][2], sep = ". ")  
      }
   }
   X
}

Bats15$names <- abr_name(Bats15$CurrentBatSpecies)


# do the same for plant genus
PlantRecords <- Records %>% 
   group_by(PlantGenus) %>%
   summarise(Frequency = n()) %>%
   arrange(desc(Frequency))

Plants15 <- PlantRecords[1:15,]





### PLOTS ###

loadfonts(device = "win") # To set the fonts we'll use

# Make the bar plot for the bat species
g3 <- ggplot(Bats15, aes(x = reorder(names, Frequency), y = Frequency)) +
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




# Make the bar plot for the plant genus
g4 <- ggplot(Plants15, aes(x = reorder(PlantGenus, Frequency), y = Frequency)) +
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


# print them together as a .png
png("Figure_3.png", res = 400,
    width = 20, height = 12, unit = "cm")

# Draw the two plots together with the same size (only acept objets of class Gorb)
grid.draw(cbind(ggplotGrob(g3), ggplotGrob(g4), size = "first"))
# Draw the legend
grid.text(label = c("A","B"), x = c(0.03,0.54), y = c(0.96,0.96),
          gp = gpar(fontsize = 14, fontfamily = "Arial Narrow", fontface = "bold"))

dev.off()


### END ###