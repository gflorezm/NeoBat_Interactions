################################################################################
#####
##### Code for reproducing tables 1 and 2
#####
################################################################################

## load the packages ####

if (!require(tidyverse)) install.packages('tidyverse')

## open data ####

records <- read.csv("./Data/NeoBat_Interactions_Records.csv")
plants_iucn <- read.csv("./Other/Plants_IUCN.csv")
bats_iucn <- read.csv("./Other/Bats_IUCN.csv")

################################################################################


################################################################################
#####
##### TABLE 1 ####
#####
################################################################################

# Bat trophic guilds

batguilds <- records %>%
      dplyr::group_by(CurrentBatSpecies, TrophicGuild) %>%
      dplyr::summarise() %>%
      dplyr::group_by(TrophicGuild) %>%
      dplyr::summarise(Frequency = n()) %>% 
      dplyr::mutate(Frequency_R = round(100*(Frequency/sum(Frequency)),1)) %>%
      tibble::add_column(X = "Trophic guild of bats", .before = "TrophicGuild")

# Plant life forms

lifeform <- records %>%
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::group_by(CurrentPlantSpecies, LifeForm) %>%
      dplyr::summarise() %>%
      dplyr::group_by(LifeForm) %>%
      dplyr::summarise(Frequency = n()) %>%
      dplyr::mutate(Frequency_R = round(100*(Frequency/sum(Frequency)),1)) %>%
      dplyr::mutate(LifeForm = tidyr::replace_na(LifeForm, "Not information")) %>%
      tibble::add_column(X = "Life form of plants", .before = "LifeForm")

# Plant successional stage

sstage <-  records %>%
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::group_by(CurrentPlantSpecies, SuccessionalStage) %>%
      dplyr::summarise() %>%
      dplyr::group_by(SuccessionalStage) %>%
      dplyr::summarise(Frequency = n()) %>%
      dplyr::mutate(Frequency_R = round(100*(Frequency/sum(Frequency)),1)) %>%
      dplyr::mutate(SuccessionalStage = tidyr::replace_na(SuccessionalStage, 
                                                          "Not information")) %>%
      tibble::add_column(X = "Succesional stage of plants", 
                         .before = "SuccessionalStage")

names(batguilds) <- c("Ecological trait", "Class", "Number of species", "%")
names(lifeform) <- c("Ecological trait","Class", "Number of species", "%")
names(sstage) <- c("Ecological trait","Class", "Number of species", "%")

# merge all tibbles
all <- rbind(batguilds, sstage, lifeform)

# check the table
all



################################################################################
#####
##### TABLE 2 ####
#####
################################################################################

# number of plant species
n_plants <- records %>%
      dplyr::filter(!PlantGenus == "Unidentified") %>%
      dplyr::summarise(plants = unique(CurrentPlantSpecies)) %>%
      nrow()

##number of bats species
n_bats <- records %>%
      dplyr::summarise(bats = unique(CurrentBatSpecies)) %>%
      nrow()

# IUCN categories
iucnlevels <- c("Extinct", "Extinct in the Wild","Critically Endangered", 
                "Endangered", "Vulnerable", "Near Threatened", "Least Concern", 
                "Conservation Dependent", "Data Deficient", "Not Evaluated")

# Number of plants by category
plantsiucn <- plants_iucn %>%
      dplyr::group_by(Category, Code) %>%
      dplyr::mutate(Category = factor(Category, levels = iucnlevels)) %>%
      dplyr::summarise(Frequency = n()) %>% 
      dplyr::mutate(Frequency_R = round(100*(Frequency/n_plants),1)) %>%
      dplyr::mutate(Category = paste0(Category, " (", Code, ")")) %>%
      tibble::add_column(X = "Plants", .before = "Category") %>%
      dplyr::select(-Code)

# Number of bats by category
batsiucn <- bats_iucn %>%
      dplyr::group_by(Category, Code) %>%
      dplyr::mutate(Category = factor(Category, levels = iucnlevels)) %>%
      dplyr::summarise(Frequency = n()) %>% 
      dplyr::mutate(Frequency_R = round(100*(Frequency/n_bats),1)) %>%
      dplyr::mutate(Category = paste0(Category, " (", Code, ")")) %>%
      tibble::add_column(X = "Bats", .before = "Category") %>%
      dplyr::select(-Code)

# merge all tibbles
alliucn <- rbind(plantsiucn, batsiucn)
names(alliucn) <- c("Group", "IUCN Status", "Number of species", "%")

# check the table
alliucn

## END ####