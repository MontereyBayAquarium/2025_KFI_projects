# Keenan reconciling dermy data by himself

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)
gs4_auth()

####################################################################################
# 1. Set directory

datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

####################################################################################
# 2. Read original data

derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 4) %>% clean_names()

####################################################################################
# 3. Read QA/QC data

derm_qc <- read_sheet("https://docs.google.com/spreadsheets/d/10JlfhROxqXfnPoM21-UUPfGthjCls4uY1UbdxxFRPvg/edit?gid=0#gid=0",
                      sheet = 2) %>% clean_names()

####################################################################################
# 4. Set data types and apply standard site naming convention

derm_raw_build1 <- derm_raw %>%
  data.frame() %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count),
    diet = as.factor(diet)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -windows_ctrl_alt_shift_1_mac_command_option_shift_1,
         -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) 

derm_qc_build1 <- derm_qc %>%
  data.frame() %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count),
    diet = as.factor(diet)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -windows_ctrl_alt_shift_1_mac_command_option_shift_1,
         -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) 

####################################################################################
# 5. Identify mismatched entries

derm_discrep_values <- derm_qc_build1 %>%
  inner_join(derm_raw_build1, 
             by = c("site", "site_type", "zone", "date", "transect", 
                    "depth", "depth_units", "species", "size"), 
             suffix = c("_qc", "_raw")) %>%
  # Compare count and diet values
  mutate(
    count_diff = if_else(count_raw != count_qc, 
                         paste(count_raw, "≠", count_qc), 
                         NA_character_),
    
    diet_diff = if_else(as.character(diet_raw) != as.character(diet_qc), 
                        paste(as.character(diet_raw), "≠", as.character(diet_qc)), 
                        NA_character_)
  ) %>%
  # Keep only mismatches
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  # Select relevant columns
  select(site, site_type, zone, date, transect, depth, depth_units, 
         species, size, count_diff, diet_diff) %>%
  arrange(site, zone, date, transect, depth, species, size) %>%
  mutate(resolved = "")

####################################################################################













