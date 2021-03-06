################################################################################
#
# FIGURE 5 
# Interactions accumulation curve
#
################################################################################


#Delete all previous objects
rm(list= ls())

################################################################################
##### LOAD THE PACKAGES
################################################################################

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(vegan)) install.packages('vegan')
if (!require(reshape2)) install.packages('reshape2')

################################################################################
##### IMPORT THE DATA
################################################################################

# Import the data set with the interaction records and filter out "unidentified"
# plant genera
records <- read.csv("./data/NeoBat_Interactions_records.csv")

# Check the data
class(records)
str(records)
head(records)

# Filter the data to identify the pairwise interactions by study site (used as sampling effort)
interactions <- records %>% 
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::mutate(Interaction = str_c(CurrentBatSpecies, CurrentPlantSpecies, 
                                        sep = "_", collapse = NULL)) %>%
      dplyr::group_by(SiteCode, Interaction) %>%
      dplyr::summarise(Frequency = n())



# Transform these list into a Interactions vs sites matrix
int_hab_matrix <- reshape2::acast(interactions, SiteCode ~ Interaction, 
                                  value = "Frequency") %>%
      tidyr::replace_na(0)  # replace the NAs with 0
      

# Extract the data for the SAC
rare1 <- vegan::specaccum(int_hab_matrix, method = "exact", permutations = 100, 
                   gamma = "Chao2")
rare2 <- vegan::specaccum(int_hab_matrix, method = "collector") # the collector curve

rareData <- data.frame(Sites = rare1$sites,
                       collector = rare2$richness,
                       Chao2 = rare1$richness,
                       ic_min = rare1$richness - (rare1$sd*1.96),
                       ic_max = rare1$richness + (rare1$sd*1.96))

################################################################################
##### MAKE THE PLOTS
################################################################################


rareplot <- ggplot(rareData) + 
      geom_line(aes(x=Sites, y=Chao2), colour = "#006154", size = 1.5) +
      geom_point(aes(x=Sites, y=collector), alpha=0.9) +
      geom_ribbon(aes(x=Sites, ymin=ic_min,ymax=ic_max),
                  fill = "#006154", alpha=0.2) + 
      theme_bw() +
      labs(x = "Number of sampling sites", y = "Number of unique interactions") +
      theme(panel.grid = element_blank(),
            axis.text = element_text(size = 11, colour = "black"),
            axis.title.x = element_text(size = 12, colour = "black", vjust = -3,
                                        face = "bold"),
            axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,2,2), "lines"))


# See the plot
rareplot

# Export as PNG image
png("./Figures/Figure_5.png", res = 300,
    width = 2400, height = 1700, unit = "px")

rareplot

dev.off()
