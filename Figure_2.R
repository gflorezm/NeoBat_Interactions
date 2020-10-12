#####################################################
#
# FIGURE 2
# 2.A Cartogram: number of studies per country
# 2.B Histogram: number of per year of publication
#
#####################################################



### PREPARE THE LIBRARY ###

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(extrafont)
library(ggpubr)


### PREPARE THE DATA ###

#Read the csv from the directory
sites <- read.csv("./Data/NeoBat_Interactions_Sites.csv")
refs <- read.csv("./Data/NeoBat_Interactions_References.csv")

# Frequency table with the number of studies per country
f_countries <- data.frame(table(sites$Country))
colnames(f_countries) <- c("region","frequency")

# A vector with the year of publication of each paper
r_years <- data.frame("year" = refs$Year)

# To standardize the USA name with "world map
f_countries <- f_countries %>% 
   mutate(region = recode(region, "United States" = "USA", 
                          "Trinidad and Tobago" = "Trinidad"))

# Load the Worl map from 'mapdata' package
world <- map_data("world")

# A cut off of America without Canada
America <- world %>% filter(lat > -60 & lat < 50, long > -125 & long < -30,
                            region != "Canada")

# Merge the data of America and f_countries and replace NA by 0
America <- America %>% full_join(f_countries, by = "region") %>%
   mutate(frequency = coalesce(frequency, 0))

# Remove some data from the global environment to save memory
rm(sites, world, refs)


### MAKE THE PLOTS ###

loadfonts(device = "win") # To set the fonts we'll use


# The Cartogram 
g1 <- ggplot() + geom_polygon(data = America, 
                              aes(x = long, y = lat, fill = frequency, 
                                  group = group), colour = "black") + 
   theme_void() + coord_fixed(1.1) +
   scale_fill_gradient(low = "#dcdcdc", high = "#006154") +
   labs(fill = "Number of studies") +
   theme(legend.text = element_text(size = 12, family = "Arial Narrow"),
         legend.title = element_text(face = "bold", size = 12, family = "Arial Narrow"),
         legend.key.height = unit(0.8, "lines"),
         legend.key.width = unit(1.2, "lines"),
         legend.position = "right", 
         legend.box = "vertical",
         plot.margin = unit(c(0,2,0.5,1), "lines"))


# The histogram
g2 <- ggplot(r_years, aes(x = year)) +
   geom_histogram(binwidth = 4, fill = "#006154", color = "black", alpha = 0.9) +
   theme_bw() +
   labs(x = "Year of publication", y = "Number of Studies") +
   theme(axis.text = element_text(size = 12, colour = "black",
                                  family = "Arial Narrow"),
         axis.title.x = element_text(size = 12, colour = "black", vjust = -3,
                                     family = "Arial Narrow", face = "bold"),
         axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                     family = "Arial Narrow", face = "bold"),
         plot.margin = unit(c(1,1,2,2), "lines"))


# print them together as a .png
png("Figure_2.png", res = 400,
    width = 20, height = 10, unit = "cm")

ggarrange(g1, g2, ncol = 2, nrow = 1,
          labels = c("A", "B"), hjust = -1,
          font.label = list(size = 16, family = "Arial Narrow"))

dev.off()


### END ###