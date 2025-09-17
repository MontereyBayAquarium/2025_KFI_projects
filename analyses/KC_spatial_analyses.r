###################################################
### Spatial patterns of Dermasterias occurrence ###
###################################################

rm(list=ls())

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













































