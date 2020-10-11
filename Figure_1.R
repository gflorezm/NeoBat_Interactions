##########################################################
#
# FIGURE 1
# Geographic istribution of sampling sites by study type
#
##########################################################



### PREPARE THE LIBRARY ###

# you also need the package rgeos
# install.packages("rgeos")

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(extrafont)
library(ggspatial)

### PREPARE THE DATA ###

#Read the csv from the directory
sites <- read.csv("./Data/NeoBat_Interactions_Sites.csv")

# Separate the columns with the coordinates and the study type
sampleSites <- sites %>% select(Latitude, Longitude, StudyType)

# Load the Worl map from 'mapdata' package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")



### MAKE THE MAP ###

loadfonts(device = "win") # To set the fonts we'll use

# The base map

g1 <- ggplot(data = world) +
  geom_sf(colour = "white", fill = "#d3d3d3") +
  coord_sf(xlim = c(-124, -30), ylim = c(-58,40), expand = FALSE) +
  theme_bw() + 
  # Points
  geom_point(data = sampleSites, aes(x = Longitude, y = Latitude, 
                                     colour = StudyType), 
             alpha = 0.5, size = 1.6) +
  # Customize the colors and labels
  scale_color_manual(values = c("#C59F00","#006154")) + 
  labs(colour = "Study type", x = "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9, family = "Arial Narrow"),
        legend.title = element_text(face = "bold", size = 9, family = "Arial Narrow"),
        axis.text = element_text(size = 9, colour = "black",
                                 family = "Arial Narrow"),
        axis.title.x = element_text(size = 10, colour = "black", vjust = -4,
                                    family = "Arial Narrow", face = "bold"),
        axis.title.y = element_text(size = 10, colour = "black", vjust = 3,
                                    family = "Arial Narrow", face = "bold"),
        legend.position = c(0.25,0.4),
        legend.background = element_rect(fill = "NA"),
        legend.key = element_rect(fill = "NA"),
        plot.margin = unit(rep(0.5,4), "lines")) +
  # Add the scale bar
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2,
                              bar_cols = c("grey30", "white"),
                              text_family = "Arial Narrow") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                                    style = north_arrow_fancy_orienteering(fill = c("white", "grey30")))

# Save the map as a png
png("Figure_1.png", res = 400,
    width = 10, height = 14, unit = "cm")
g1

dev.off()

### END ###

