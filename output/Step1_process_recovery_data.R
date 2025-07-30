

################################################################################
# About
# data processing script written by JG.Smith jogsmith@ucsc.edu

################################################################################
#set directories and load data
rm(list=ls())

require(librarian) #use install.packages('librarian') if not already installed
librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)

gs4_auth()


#read original data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

#site metdata
reco_meta <- read_sheet("https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=134064181#gid=134064181",
                       sheet = 3) %>% clean_names()

################################################################################
# process metadata
################################################################################

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


################################################################################
# process quadrat entry
################################################################################

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
  mutate(across(c(purple_urchins, purple_conceiled, red_urchins, red_conceiled,
                  lamr, macr, macj, nerj, ptej, lsetj, eisj,
                  tegula, pomaulax),
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
             lamr, macr, macj, nerj, ptej, lsetj, eisj,
             tegula_densitym2, pomaulax_densitym2) %>%
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
    #join with site table and uopdate site names
    ##############################################################################
    mutate(year = year(survey_date)) %>%
    #clean up names
    rename(lamr_densitym2 = lamr,
           macr_densitym2 = macr,
           macj_densitym2 = macj,
           nerj_densitym2 = nerj,
           ptej_densitym2 = ptej,
           lsetj_densitym2 = lsetj,
           eisj_densitym2 = eisj)
  
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
quad_data <- quad_raw_build1





################################################################################
#Step 2 - process urchin size data
################################################################################


#build raw size fq
urch_build <- urchin_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = factor(depth_units),
    species = factor(species),
    size = factor(size),
    count = as.numeric(count)
  ) %>%
  #convert feet to meters
  mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth) %>%
  select(name_of_data_enterer, survey_date, everything()) %>%
  mutate(size = as.numeric(as.character(size))) %>%
  rename(size_cm = size) %>% select(-x15) %>%
  select(-date)%>%
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
    }))

    # Step 2: Join and update only where year is 2024
    urch_build2 <- urch_build %>%
      mutate(year = year(survey_date))%>%
      left_join(site_lookup, by = c("site" = "site_old")) %>%
      mutate(site = if_else(year == 2024 & !is.na(site_new), site_new, site)) %>%
      select(-site_new, -year) %>%
      select(site, site_type, survey_date, everything())
    
urch_size_fq <- urch_build

################################################################################
#Step 3 - process swath data 
################################################################################

    #build kelp
    kelp_build <- kelp_raw %>%
      select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
      # Set column types
      mutate(
        name_of_data_enterer = factor(name_of_data_enterer),
        site = factor(site),
        site_type = factor(site_type),
        zone = factor(zone),
        survey_date = ymd(date),
        transect = as.numeric(transect),
        depth = as.numeric(depth),
        depth_units = factor(depth_units),
        species = factor(species),
        count = as.numeric(count)
      ) %>%
      #convert feet to meters
      mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
      select(-depth_units, -depth, -date) %>%
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
      }))
    
    
    # Step 2: Join and update only where year is 2024
    kelp_build2 <- kelp_build %>%
      mutate(year = year(survey_date))%>%
      left_join(site_lookup, by = c("site" = "site_old")) %>%
      mutate(site = if_else(year == 2024 & !is.na(site_new), site_new, site)) %>%
      select(-site_new, -year) %>%
      select(site, site_type, survey_date, everything())

    ##############################################################################
    #calculate macro density
    ##############################################################################
    macro_density <- kelp_build %>% filter(species == "MACPYR") %>%
      #macro is not subsampled
      select(-subsample_meter, -count) %>%
      group_by(survey_date, site, site_type, zone, 
               transect)%>%
      summarize(n_macro_plants_20m2 = n(),
                macro_stipe_density_20m2 = mean(stipe_counts_macrocystis_only, na.rm =TRUE),
                macro_stipe_sd_20m2 = sd(stipe_counts_macrocystis_only, na.rm =TRUE),
      ) 

    # Fill in missing site_type from reco_meta_build1
    macro_density_filled <- macro_density %>%
      left_join(
        reco_meta_build1 %>%
          select(site, zone, site_type),
        by = c("site", "zone"),
        suffix = c("", ".meta")
      ) %>%
      mutate(site_type = coalesce(site_type, site_type.meta)) %>%
      select(-site_type.meta)
    
    # Expand the data to include all combinations
    macro_data <- macro_density_filled %>%
      ungroup() %>%
      complete(
        survey_date,
        site,
        site_type,
        zone,
        transect,
        fill = list(
          n_macro_plants_20m2 = 0,
          macro_stipe_density_20m2 = 0,
          macro_stipe_sd_20m2 = 0
        )
      )
    
    ##############################################################################
    #calculate density of all other algae.
    #Note:: Macrocystis is not sub-sampled. All other are. We need to create a 
    #scalar to extrapolate counts to the full transect. 
    ##############################################################################

    kelp_data <- kelp_build %>%
      filter(species != "MACPYR") %>%
      select(-stipe_counts_macrocystis_only) %>%
      
      # Fix subsample meters to linear meters sampled
      mutate(
        linear_meters_sampled = case_when(
          subsample_meter < 10 ~ subsample_meter,
          subsample_meter >= 10 & subsample_meter <= 20 ~ subsample_meter - 10,
          subsample_meter > 20 & subsample_meter <= 30 ~ 30 - subsample_meter,
          TRUE ~ subsample_meter
        ),
        linear_meters_sampled = ifelse(is.na(linear_meters_sampled), 10, linear_meters_sampled),
        scalar = 10 / linear_meters_sampled,
        scalar = ifelse(scalar == 0, 1, scalar),
        density_20m2 = count * scalar
      ) %>%
      
      # Rename species column so we get proper pivot column names
      mutate(species_name = paste0("density20m2_", species)) %>%
      
      # Aggregate density_20m2 in case there are duplicate rows
      group_by(survey_date, site, site_type, zone, transect, species_name) %>%
      summarise(density_20m2 = sum(density_20m2, na.rm = TRUE), .groups = "drop") %>%
      
      # Pivot wider
      pivot_wider(
        names_from = species_name,
        values_from = density_20m2,
        values_fill = list(density_20m2 = 0)
      ) %>%
      
      clean_names()
    
##############################################################################
#export
##############################################################################

keep_vars <- c("quad_data", "urch_size_fq", "macro_data", "kelp_data")
rm(list = setdiff(ls(), keep_vars))
    
save(quad_data, urch_size_fq, macro_data, kelp_data,
   file = here("output/processed", "cleaned_survey_data.Rda"))
    
    

