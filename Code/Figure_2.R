################################################################################
#
# FIGURE 2
# 2.A Cartogram: number of studies per country
# 2.B Histogram: number of per year of publication
#
################################################################################

#Delete all previous objects
rm(list= ls())


################################################################################
##### LOAD THE PACKAGES
################################################################################

if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(maps)) install.packages('maps')
if (!require(mapdata)) install.packages('mapdata')
if (!require(cowplot)) install.packages('cowplot')

################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data sets with sites and references
sites <- read.csv("./Data/NeoBat_Interactions_Sites.csv")
refs <- read.csv("./Data/NeoBat_Interactions_References.csv")

# Check the data
class(sites)
str(sites)
head(sites)

class(refs)
str(refs)
head(refs)

# Make a frequency table with the number of studies per country
f_countries <- data.frame(table(sites$Country))

#Give the table human-readable column names
colnames(f_countries) <- c("region","frequency")

# Check the data
head(f_countries)

# Make a vector with each paper's publication year
r_years <- data.frame("year" = refs$Year)

#Check the data
head(r_years)

# Standardize country names as in the package world map
f_countries <- f_countries %>% 
      dplyr::mutate(region = recode(region, "United States" = "USA", 
                                    "Trinidad and Tobago" = "Trinidad"))

# Load the world map from mapdata package
world <- ggplot2::map_data("world")

# Select the Americas without Canada
Americas <- world %>% 
      dplyr::filter(lat > -60 & lat < 50, long > -125 & long < -30,
                    region != "Canada")

# Merge the data of the Americas and f_countries and replace NA by 0
Americas <- Americas %>% 
      dplyr::full_join(f_countries, by = "region") %>%
      dplyr::mutate(frequency = coalesce(frequency, 0))


################################################################################
##### MAKE THE PLOTS
################################################################################


# Make the Cartogram 
g1 <- ggplot() +
      geom_polygon(data = Americas, 
                   aes(x = long, y = lat, fill = frequency, 
                       group = group), colour = "black") + 
      theme_void() +
      coord_fixed(1.1) +
      scale_fill_gradient(low = "white", high = "#006154") +
      labs(fill = "Number of studies") +
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(face = "bold", size = 12),
            legend.key.height = unit(0.8, "lines"),
            legend.key.width = unit(1.2, "lines"),
            legend.position = "right", 
            legend.box = "vertical",
            plot.margin = unit(c(0,2,0.5,1), "lines"))

#See the cartogram
g1

# Make the histogram
g2 <- ggplot(r_years, aes(x = year)) +
      geom_histogram(binwidth = 4, fill = "#006154", 
                     color = "black", alpha = 0.9) +
      theme_bw() +
      labs(x = "Year of publication", y = "Number of studies") +
      scale_x_continuous(breaks = c(1957, 1967, 1977, 1987, 1997, 2007),
            limits = c(1955, 2010)) +
      theme(panel.grid = element_blank(),
            axis.text = element_text(size = 11, colour = "black"),
            axis.title.x = element_text(size = 12, colour = "black", vjust = -3,
                                        face = "bold"),
            axis.title.y = element_text(size = 12, colour = "black", vjust = 3,
                                        face = "bold"),
            plot.margin = unit(c(1,1,2,2), "lines"))

# See the histogram
g2

# Export both plots together as PNG image
png("./Figures/Figure_2.png", res = 300,
    width = 3000, height = 1700, unit = "px")

cowplot::plot_grid(g1, g2, labels = c("A", "B"))

dev.off()


################################ END ###########################################