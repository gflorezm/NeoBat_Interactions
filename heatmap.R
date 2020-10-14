################################################################################
#
# FIGURE heatmap. Number of recorded interactions by plant life form and interaction type
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
library(hrbrthemes)
library(tidyr)


################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with the interaction records
records <- read.csv("./Data/NeoBat_interactions_records.csv")

# Check the data
class(records)
str(records)
head(records)

# Make a data frame with all potential interactions between bat genera and
# plant families
possible <- records %>% expand(BatGenus, PlantFamily)

# Check the data
class(possible)
str(possible)
head(possible)

# Make a data frame with observed and potential interactions
interactions <- records %>% group_by(BatGenus,PlantFamily) %>% 
      summarise(account = n())

interactions <- possible %>% 
      full_join(interactions, by = c("BatGenus", "PlantFamily")) %>%
      mutate(account = coalesce(account, 0))

# Check the data
class(interactions)
str(interactions)
head(interactions)

# Pick the names of bats and plants for the axes
bats <- unique(interactions$BatGenus)
plants <- unique(interactions$PlantFamily)

# Check the data
bats
plants

# plot (Ainda não está terminada, acho que não sei muito bem como organizá-lo para
# ficar ordenado por número de registros)
ggplot(interactions, aes(BatGenus, PlantFamily)) + 
      geom_tile(aes(fill = account), colour = "white") +
      scale_fill_gradient(low = "white", high= "#006154") +
      scale_x_discrete(breaks = length(bats)) + 
      scale_y_discrete(breaks = length(plants)) +
      theme_bw() +
      coord_equal()


