rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
#gs4_auth()

#read data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

gonad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>%
  clean_names()


################################################################################
# process metadata
reco_meta_build1 <- reco_meta %>%
  mutate(across(everything(), as.character)) %>%       # Convert all columns to character
  mutate(across(everything(), ~ na_if(., "NULL"))) %>% # Replace "NULL" with NA
  type_convert()   %>%
  #set column types
  mutate(site_long = factor(site_long),
         survey_type = factor(survey_type),
         region = factor(region),
         site_name_2024 = factor(site_name_2024),
         site_type_2024 = factor(site_type_2024),
         site_short_2024 = factor(site_short_2024),
         site_long_2024 = factor(site_long_2024),
         site_name_2025 = factor(site_name_2025),
         site_type_2025 = factor(site_type_2025),
         transect = factor(transect),
         old_latitude = as.numeric(old_latitude),
         old_longitude = as.numeric(old_longitude),
         new_latitude = as.numeric(new_latitude),
         new_longitude = as.numeric(new_longitude),
         reprojected_coords = factor(reprojected_coords),
         target_depth_meters = as.numeric(target_depth_meters),
         uc_heading = as.numeric(uc_heading),
         dc_heading = as.numeric(dc_heading),
         original_date_surveyed = as.Date(original_date_surveyed, format = "%Y-%m-%d"),
         resite_date = as.Date(resite_date, format = "%Y-%m-%d"),
         in_stack = factor(in_stack),
         notes = as.character(notes)
  ) %>%
  # Replace date_surveyed with resite_date if resite_date is not NA
  mutate(original_survey_date_official = if_else(is.na(resite_date),original_date_surveyed,resite_date))%>%
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site_name_2025 = str_replace(site_name_2025, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    }),
    site_name_2024 = str_replace(site_name_2024, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  #drop columns and rename
  select(survey_type, region, site = site_name_2025, site_type = site_type_2025,
         site_old = site_name_2024, site_type_old = site_type_2024, zone = transect,
         latitude = new_latitude, longitude = new_longitude, latitude_old = old_latitude,
         longitude_old = old_longitude, survey_date_2024 = original_survey_date_official,
         notes
  ) %>%
  #set data types
  mutate(
    survey_type = factor(survey_type),
    region = factor(region),
    site = factor(site),
    site_type = factor(site_type),
    site_old = factor(site_old),
    site_type_old = factor(site_type_old),
    zone = factor(zone),
  )

#######

# process quadrat entry

quad_raw_build1 <- quad_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  data.frame()%>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0)%>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate),
    drift_superlayer = as.character(drift_superlayer)
  ) %>%
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
         -observer_buddy,
         -write_in,
         -notes) %>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate)
  ) %>%
  ##############################################################################
# Extrapolate densities for subsamples
##############################################################################
#first change NA to 0 --- these are true zeroes
mutate(across(c(purple_urchins, purple_conceiled, red_urchins, red_conceiled, tegula, pomaulax, lamr, macr, macj, nerj, ptej, lsetj, eisj),
              ~ replace_na(.x, 0))) %>%
  #next change subsampled quadrants to 4 if NA --- these are assumed 4
  mutate(across(c(purple_quadrants_sampled, red_quadrants_sampled, tegula_quadrants_sampled, 
                  pomaulax_quadrants_sampled),
                ~ replace_na(.x, 4))) %>%
  #now apply scalar
  mutate(
    purple_urchin_densitym2 = purple_urchins * (4 / purple_quadrants_sampled),
    purple_urchin_conceiledm2 = purple_conceiled * (4 / purple_quadrants_sampled),
    red_urchin_densitym2 = red_urchins * (4 / red_quadrants_sampled),
    red_urchin_conceiledm2 = red_conceiled * (4 / red_quadrants_sampled),
    tegula_densitym2 = tegula * (4 / tegula_quadrants_sampled),
    pomaulax_densitym2 = pomaulax * (4 / pomaulax_quadrants_sampled)
  ) %>%
  # Drop old columns
  select(-purple_urchins, -purple_conceiled, -purple_quadrants_sampled,
         -red_urchins, -red_conceiled, -red_quadrants_sampled,
         -tegula, -tegula_quadrants_sampled, -pomaulax, -pomaulax_quadrants_sampled) %>%
  ##############################################################################
#calculate upc
##############################################################################
# Step 1: Convert upc1 through upc8 columns to long format
pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  # Remove rows where species is NA (ignoring those UPC points)
  filter(!is.na(species)) %>%
  # Group by quadrat and calculate total UPC points per quadrat
  group_by(site, site_type, zone, survey_date,
           transect, quadrat, substrate) %>%
  mutate(total_points = n()) %>%  # Calculate total points per quadrat
  # Calculate percent cover for each species based on the total points that were quantified
  group_by(site, site_type, zone, survey_date, 
           transect, quadrat, substrate, relief, risk, species, 
           purple_urchin_densitym2, purple_urchin_conceiledm2,
           red_urchin_densitym2, red_urchin_conceiledm2, 
           tegula_densitym2, pomaulax_densitym2, lamr, macr, macj, nerj, ptej, lsetj, eisj) %>%
  summarise(
    percent_cover = (n() / first(total_points)) * 100,  # Use total points dynamically for each quadrat
    .groups = 'drop'
  ) %>%
  # Step 4: Reshape back to wide format with species as columns, adding 'upc_' prefix
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0,
              names_prefix = "upc_") %>%
  # Clean column names and remove any columns with 'na' in the name
  clean_names() %>%
  #clean up
  mutate(substrate = word(substrate, 1)) %>%
  #clean up
  rename_with(~ str_replace(., "^upc_", "cov_")) %>%
  ##############################################################################
# Apply standard site naming
##############################################################################
mutate(
  # Use a function within str_replace to process each match
  site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
    # Extract letters and numbers
    parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
    letters <- toupper(parts[, 2])   # Convert to uppercase if needed
    numbers <- parts[, 3]
    # Pad numbers with leading zero
    numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
    # Combine parts with underscore
    paste0(letters, "_", numbers_padded)
  })) %>%
  #rename(date_surveyed = survey_date)%>%
  #get the latest survey date for each site, site_type, zone, and transect by
  #joining metadata
  #check what didn't work
  #fix dates
  ##############################################################################
#join with site table and update site names
##############################################################################
mutate(year = year(survey_date)) 

# Step 2: Prepare the lookup table from reco_meta_build1
site_lookup <- reco_meta_build1 %>%
  select(site_old, site_new = site) %>%
  distinct()

# Step 3: Join and update only where year is 2024
quad_raw_build2 <- quad_raw_build1 %>%
  left_join(site_lookup, by = c("site" = "site_old")) %>%
  mutate(site = if_else(year == 2024 & !is.na(site_new), site_new, site)) %>%
  select(-site_new, -year)


#remove everything except quad_raw_build1 
quad_data <- quad_raw_build2
rm(list = setdiff(ls(), "quad_data"))

#####

###averaging counts and % cover per transect

# Helper function to get the most common value (mode)
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

quad_transect_avgs <- quad_data %>%
  group_by(site, site_type, zone, survey_date, transect) %>%
  summarise(
    across(where(is.numeric) & !any_of("quadrat"), ~ mean(.x, na.rm = TRUE)),
    substrate = get_mode(substrate),
    .groups = "drop"
  )


# View result
print(quad_transect_avgs)

#now averaging per survey 
quad_avgs <- quad_transect_avgs %>%
  group_by(site, site_type, zone, survey_date) %>%
  summarise( 
    across(where(is.numeric) & !any_of("transect"), ~mean(.x, na.rm = TRUE)), 
    substrate = get_mode(substrate), 
    .groups = "drop")

# View result
print(quad_avgs)

#now making a new column to represent urchin behavior i.e. how many urchins are actively grazing 

quad_avgs <- quad_avgs %>%
  mutate(
    purple_urchin_active_foraging_densitym2 = purple_urchin_densitym2 - purple_urchin_conceiledm2,
    red_urchin_active_foraging_densitym2 = red_urchin_densitym2 - red_urchin_conceiledm2
  ) %>%
  mutate(total_urchin_densitym2 = purple_urchin_densitym2 + red_urchin_densitym2, 
         total_urchin_foraging_densitym2 = purple_urchin_active_foraging_densitym2 + red_urchin_active_foraging_densitym2)

#####

# process swath entry

kelp_raw_build1 <- kelp_raw %>%
  # Remove example first row and classifiers
  slice(-614, -615, -616) %>%
  data.frame()%>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8, -x16)%>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    species = factor(species),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
         -observer,
         -buddy) %>%
  # Set column types
  mutate(
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    date = ymd(date),
    transect = as.numeric(transect))

#fix subsample meters that were logged incorrectly

kelp_raw_build1[8, "subsample_meter"] <- 8.6
kelp_raw_build1[72, "subsample_meter"] <- 5
kelp_raw_build1[59, "subsample_meter"] <- 3.9
kelp_raw_build1[211, "subsample_meter"] <- 9.8
kelp_raw_build1[73, "subsample_meter"] <- 9.0
kelp_raw_build1[34, "subsample_meter"] <- 8.4
kelp_raw_build1[53, "subsample_meter"] <- 6.3


# Extrapolate densities for subsamples

#change subsample meter to fraction

kelp_raw_build2 <- kelp_raw_build1 %>%
  mutate(sampling_fraction = subsample_meter / 10)

kelp_raw_build2$sampling_fraction[is.na(kelp_raw_build2$sampling_fraction)] <- 1

#this is the adjusted counts after factoring in the subsample meter
kelp_raw_build2 <- kelp_raw_build2 %>%
  mutate(adjusted_count = count / sampling_fraction)   


# Step 4: Reshape back to wide format with species as columns, adding 'upc_' prefix
kelp_raw_build3 <- kelp_raw_build2 %>%
  slice(-227, -228, -229, -230) %>%
  select(-count, -subsample_meter, -sampling_fraction, -depth, -depth_units) %>%
  group_by(site, site_type, zone, date, transect, species) %>%
  summarise(adjusted_count = sum(adjusted_count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = species, values_from = adjusted_count, values_fill = 0) %>%
  clean_names() %>% 
  mutate(
    # Use a function within str_replace to process each match
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    })) %>%
  #rename(date_surveyed = survey_date)%>%
  #get the latest survey date for each site, site_type, zone, and transect by
  #joining metadata
  #check what didn't work
  #fix dates
  ##############################################################################
#join with site table and update site names
##############################################################################
mutate(year = year(date)) 

# Step 2: Prepare the lookup table from reco_meta_build1
site_lookup <- reco_meta_build1 %>%
  select(site_old, site_new = site) %>%
  distinct()

# Step 3: Join and update only where year is 2024
kelp_raw_build4 <- kelp_raw_build3 %>%
  left_join(site_lookup, by = c("site" = "site_old")) %>%
  mutate(site = if_else(year == 2024 & !is.na(site_new), site_new, site)) %>%
  select(-site_new, -year)


#remove everything except quad_raw_build1 
kelp_data <- kelp_raw_build3
rm(list = setdiff(ls(), "kelp_data"))


#averaging transects
kelp_avgs <- kelp_data %>%
  group_by(site, site_type, zone, date) %>%
  summarise( 
    across(where(is.numeric) & !any_of("transect"), ~mean(.x, na.rm = TRUE)), 
    .groups = "drop") 

#####


quad_kelp <- kelp_avgs %>%
  left_join(quad_avgs, by = c("site", "zone", "year"))

