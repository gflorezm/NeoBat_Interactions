################################################################################
#
# FIGURE 1
# Geographic distribution of sampling sites by study type
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
library(grDevices)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)


################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with site coordinates
sites <- read.csv("./Data/NeoBat_Interactions_Sites.csv")

# Check the data
class(sites)
str(sites)
head(sites)

# Select the columns with the coordinates and study types
sites_short <- sites %>% 
      dplyr::select(Latitude, Longitude, StudyType)

#Check the data
head(sites_short)

# Load the world map from the mapdata package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


################################################################################
##### PLOT THE MAP
################################################################################


# Make the base map
g1 <- ggplot(data = world) +
      geom_sf(colour = "white", fill = "#d3d3d3") +
      coord_sf(xlim = c(-121, -30), ylim = c(-58,40), expand = FALSE) +
      theme_bw() + 
      # Plot the sites
      geom_point(data = sites_short, aes(x = Longitude, y = Latitude, 
                                         colour = StudyType), 
                 alpha = 0.6, size = 2) +
      # Customize the colors and labels
      scale_color_manual(values = c("#C59F00","#980063")) + 
      labs(colour = "Study type", x = "Longitude", y = "Latitude") +
      theme(panel.grid = element_blank(),
            legend.text = element_text(size = 11),
            legend.title = element_text(face = "bold", size = 10),
            axis.text = element_text(size = 11, colour = "black"),
            axis.title.x = element_text(size = 12, colour = "black", vjust = -4,
                                        face = "bold"),
            axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                        face = "bold"),
            legend.position = c(0.25,0.4),
            legend.background = element_rect(fill = "NA"),
            legend.key = element_rect(fill = "NA"),
            plot.margin = unit(rep(0.5,4), "lines")) +
      # Add a scale bar
      ggspatial::annotation_scale(location = "bl", width_hint = 0.3,
                                  bar_cols = c("grey30", "white")) +
      # Add a north arrow
      ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                        height = unit(1.5, "cm"), 
                                        width = unit(1.5, "cm"),
                                        style = north_arrow_fancy_orienteering
                                        (
                                              fill = c("white","grey30")))

# See the map
g1

# Export the map as a PNG image
png("./Figures/Figure_1.png", res = 300,
    width = 2000, height = 2200, unit = "px")
g1

dev.off()


################################ END ###########################################
