library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

Records <- read.csv("./Data/NeoBat_Interactions_Records.csv")

# A data frame with all possible interactions between genus of bats and family of plants
Possible <- Records %>% expand(BatGenus, PlantFamily)


# A dataframe with the observed interactions
Interactions <- Records %>% group_by(BatGenus,PlantFamily) %>% 
      summarise(account = n())

Interactions <- Possible %>% 
      full_join(Interactions, by = c("BatGenus", "PlantFamily")) %>%
      mutate(account = coalesce(account, 0))

# Names of bats and plants for fill the axis
Bats <- unique(Interactions$BatGenus)
Plants <- unique(Interactions$PlantFamily)


# plot (Ainda não está terminada, acho que não sei muito bem como organizá-lo para
# ficar ordenado por número de registros)
ggplot(Interactions, aes(BatGenus, PlantFamily)) + 
      geom_tile(aes(fill = account), colour = "white") +
      scale_fill_gradient(low = "white", high= "#006154") +
      scale_x_discrete(breaks = 1:length(Bats)) + 
      scale_y_discrete(breaks = 1:length(Plants)) +
      theme_bw() +
      coord_equal()


