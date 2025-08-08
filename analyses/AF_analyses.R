# Aimee's script 

install.packages("librarian")
librarian::shelf("tidyverse", "vegan", "ggplot2")

# Load data

setwd("~/Desktop/MBA_code_repository/2025_KFI_projects/output/processed")

df <- load("cleaned_survey_data.Rda")


View(quad_data) # rda for quad data

print(quad_data)

# Step 1: Make groupings to prep for normalizing data

groups <- quad_data[c(1:3, 5:6, 59)] # grouping together character variables

bio_data <- quad_data[10:58] # grouping together biological numerical variables

View(groups)
View(bio_data)

# Step 2: Normalizing quad_data Using Z-Score to max

# norm_data <- decostand(bio_data, method = "max") # normalizing it to the max (1) 

norm_data <- decostand(bio_data, method = "standarize") # normalizing to 0 


View(norm_data)


# Step 3: Triangle Distance Matrix


dist_z <- metaMDS(norm_data, distance = "euclidean") # using euclidean because it's already z scored

scores_z <- as.data.frame(scores(dist_z, display = "sites")) 

scores_z$site_type <- groups$site_type  


# Step 4: Visualizing with a NMDS Plot

ggplot() + 
  geom_point(
    data = scores_z, aes(x = NMDS1, y = NMDS2, color = site_type, shape = site_type), 
    size = 2, alpha = 0.2
  ) + 
  stat_ellipse(
    data = scores_z, aes(x = NMDS1, y = NMDS2, color = site_type) 
    , type = "norm", linetype = 1, size = 1
  ) +
  theme_bw()



