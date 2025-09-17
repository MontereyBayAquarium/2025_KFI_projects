###################################################
### Spatial patterns of Dermasterias occurrence ###
###################################################

rm(list=ls())

# Research question -------------------------------------------------------

# Where in space are Dermasterias eating urchins?

# Hypothesis --------------------------------------------------------------

# Urchin predation will be most common in incipient 
# forests in comparison to persistent forests and barrens

# Load packages, data, set dir --------------------------------------------

require(librarian)
librarian::shelf(tidyverse, ggplot2, janitor, readxl, googlesheets4)

gs4_auth()

recovery_derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=344647275#gid=344647275",
                                sheet = 2) %>% clean_names()

margin_derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=931069509#gid=931069509", 
                              sheet = 2) %>% clean_names()

# Prepare data ------------------------------------------------------------

derm_recovery_spatial <- recovery_derm_raw %>% dplyr::select(site, site_type, zone, 
                                                             transect, depth, species, 
                                                             size, count, diet, urchin_size)

derm_margin_spatial <- margin_derm_raw %>% dplyr::select(site, heading_out, transect, 
                                                         depth_start, depth_end, segment, 
                                                         species, size, count, 
                                                         diet, urchin_size)

# Wrangling (recovery) ----------------------------------------------------

# filter occurrence data for site type
derm_site_type <- derm_recovery_spatial %>% 
  select(site_type)

view(derm_site_type)

# filter + mutate for sizes at different depths
derm_depth_size <- derm_recovery_spatial %>% 
  select(depth, size)   

view(derm_depth_size)

# filter for site-specific predation, remove none's

derm_site_pred <- derm_recovery_spatial %>% 
  select(site_type, diet) %>% 
  filter(!(diet %in% 
             c("None")))

view(derm_site_pred)

# filter for just urchin eaters and site type

derm_site_urchin <- filter(derm_recovery_spatial, 
                           diet == "Urchin")

# Visualization (recovery) ------------------------------------------------

# site occurrence bar plot
ggplot(derm_site_type, 
       aes(x = site_type)) + 
  geom_bar(show.legend = "FALSE") + 
  theme_minimal()

# size at depth scatterplot

ggplot(derm_depth_size, 
       aes(x = depth, 
           y = size)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_minimal()

# Bar plot of site-specific predation

ggplot(derm_site_pred, 
       aes(x = diet, 
           fill = site_type)) + 
  geom_bar(show.legend = FALSE) + 
  facet_wrap(~site_type, 
             ncol = 1) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal()

# Bar plot of site-specific urchin predation

ggplot(derm_site_urchin, 
       aes(x = site_type, 
           fill = site_type)) + 
  geom_bar(show.legend = FALSE) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal()

# Final figures (recovery) ------------------------------------------------





# Asking R to do math for me (nicely) -------------------------------------














































































