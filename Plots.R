# Install necessary packages (if not already installed)
# install.packages(c("readxl", "dplyr", "tidyr", "table1", "epiDisplay", 
# "NbClust", "cluster", "factoextra", "fpc", "clValid", "ggpubr", 
# "sf", "maps", "maptools", "ggplot2", "readr", "tmap", "tmaptools", 
# "rgeos", "haven", "rgdal", "gridExtra"))

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(table1)
library(epiDisplay)
library(NbClust)
library(cluster)
library(factoextra)
library(fpc)
library(clValid)
library(ggpubr)
library(sf)
library(maps)
library(maptools)
library(ggplot2)
library(readr)
library(tmap)
library(tmaptools)
library(rgeos)
library(haven)
library(rgdal)
library(gridExtra)


# Load data
shocks_final <- read_excel("shocks_final_data1.xlsx")

# Set global options
options(scipen = 999)


######################### MAP VISUALIZATION #################################

# Load regional map data (Level 1)
mymap <- sf::st_read("Raw Data/gadm41_SEN_shp/gadm41_SEN_1.shp", stringsAsFactors = FALSE)
mymap <- mymap %>% dplyr::select(NAME_1, geometry)

# Mark specific regions of interest
mymap$work <- 0
mymap$work <- ifelse(mymap$NAME_1 == "Louga", 1, mymap$work)
mymap$work <- ifelse(mymap$NAME_1 == "Kaffrine", 1, mymap$work)
region_names <- c("Louga", "Kaffrine")

# Load commune map data (Level 4)
communes <- sf::st_read("Raw Data/gadm41_SEN_shp/gadm41_SEN_4.shp", stringsAsFactors = FALSE)
communes <- communes %>% dplyr::select(NAME_1, NAME_4, geometry)

# Mark specific communes of interest
communes$exp <- 0
communes$exp <- ifelse(communes$NAME_4 == "Mabo", 1, communes$exp)
communes$exp <- ifelse(communes$NAME_4 == "Thiel", 1, communes$exp)
commune_names <- c("Mabo", "Thiel")

# Filter the commune data for selected regions
communes_to_plot <- communes %>% 
  filter(NAME_1 %in% region_names & NAME_4 %in% commune_names)

# Generate commune plot
communes_plot <- ggplot() +
  geom_sf(data = communes_to_plot)

# Create the final plot with labels
ggplot() +
  geom_sf(data = mymap, aes(fill = as.factor(work))) +
  geom_sf(data = communes_to_plot) +
  geom_sf_text(aes(label = NAME_1), data = mymap[mymap$NAME_1 %in% region_names, ]) +
  geom_sf_label(data = communes_to_plot, aes(label = NAME_4), 
                size = 3, fontface = "bold") +
  scale_fill_manual(values = c("white", "grey")) +
  coord_sf() +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    line = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    text = element_text(size = 20),
    plot.title = element_text(size = 18)
  )

################### HIERARCHICAL CLUSTER ANALYSIS ########################

######################## Silhouette Cluster Analysis ###########################

# Prepare data for clustering
cluster.data <- shocks_final %>% dplyr::select(Participant_ID, Session_ID, sm_q2:sm_q8)
cluster.data <- unique(cluster.data)

# Identify and remove any missing data
missing_idx <- which(is.na(cluster.data$sm_q8))
cluster.data <- cluster.data[-missing_idx, ]  # Remove rows with missing data for clustering

# Perform k-means clustering with 2 clusters
k2 <- kmeans(cluster.data[, 3:9], centers = 2, nstart = 20)
fviz_cluster(k2, data = cluster.data[, 3:9])

# Determine the optimal number of clusters using silhouette method
silh <- fviz_nbclust(cluster.data[, 3:9], kmeans, method = 'silhouette')
silh

# Plot silhouette analysis for the chosen clustering
pam.res2 <- pam(cluster.data, 2, metric = "euclidean", stand = FALSE)
silhouette_plot <- fviz_silhouette(pam.res2, palette = "jco", ggtheme = theme_classic())

# Arrange and display the plots for optimal cluster number and silhouette analysis
grid.arrange(silh, silhouette_plot, ncol = 2, nrow = 1)

################ PLOT AVERAGE CONTRIBUTIONS TO THE PUBLIC FUND BY CLUSTER ################

# Assign cluster labels to the data
cluster.data$cluster <- k2[["cluster"]]

# Calculate average contributions by cluster
average_contributions <- cluster.data %>% 
  group_by(cluster) %>% 
  summarise(across(sm_q2:sm_q8, mean))

# Transform data from wide to long format for plotting
plot_data <- gather(average_contributions, type, contributions, sm_q2:sm_q8)
plot_data$contributions <- round(plot_data$contributions, digits = 0)
plot_data$cluster <- as.factor(plot_data$cluster)

# Calculate the average contribution across rounds
plot_data <- plot_data %>% 
  group_by(cluster) %>%
  mutate_at(vars(contributions), list(Mean = mean), na.rm = TRUE)

# Create labels for the x-axis
SM <- c("0", "1000", "1500", "2500", "3000", "4000", "5000")

# Plot average contributions with dots and lines
ggplot(plot_data, aes(x = type, y = contributions, colour = cluster, group = cluster)) +
  geom_point() +
  geom_line() +
  ylab("Average contributions to the public fund") +
  xlab("Contribution of the other player to the public fund") +
  scale_x_discrete(labels = SM) +
  scale_y_continuous(limits = c(0, 5000)) +
  scale_colour_grey(name = "Cluster", labels = c("1 (60.3%)", "2 (39.7%)")) +
  geom_text(aes(label = contributions, vjust = "inward", hjust = "inward"), 
            colour = "black", size = 3.5) +
  labs(fill = "Cluster", caption = "Note: 1 = Conditional cooperator, 2 = Weak conditional cooperator") +
  theme_minimal()


