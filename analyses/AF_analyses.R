# Aimee's script 

install.packages("librarian")
librarian::shelf("tidyverse", "vegan", "ggplot2", "dplyr", "car", "ggpubr", "minpack.lm", "googledrive",
                 "viridis", "plotly", "hrbrmisc")

install.packages("googledrive")
library(googledrive)


# Load data

drive_deauth()

drive_auth(scopes = "https://www.googleapis.com/auth/drive")

file <- drive_find("kelp_recovery_data.rda",
                   shared_drive = "MBA_kelp_recovery_research")

file

tmp <- tempfile(fileext = ".rda")
drive_download(as_id("1aYx6OzGgcI7llw3C69TQYNNLkoFZnj_i"), 
               path = tmp, 
               overwrite = TRUE)

file.exists(tmp)

objs <- load(tmp)

print(objs)



# setwd("~/Desktop/MBA_code_repository/2025_KFI_projects/output/processed")

# df <- load("cleaned_survey_data.Rda")

##############################################################################

# KELP vs URCHINS


View(kelp_data)

# Create new data frames

kelp_data <- kelp_data %>% # create new column for year in kelp_data
  mutate(year = year(survey_date))

quad_data <- quad_data %>% # create new column for year in quad_data
  mutate(year = year(survey_date))

#recruit_sum <- # created new data frame to total recruits and juveniles + proportion of purps exposed
#  quad_data %>% 
#  mutate(prop_exp = (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2) %>%
#  mutate(total_recruit = lamr + macr + macj + nerj + ptej + lsetj + eisj) %>%
#  group_by(year, site, site_type, zone, transect) %>%
#  summarise(total_recruit = sum(total_recruit, na.rm = TRUE), 
#            mean_urch_prop = mean(prop_exp, na.rm = TRUE),
#            mean_urch_den = mean(purple_urchin_densitym2, na.rm = TRUE))

quad_sum <- # created new data frame with mean urch + purps exposed + recruits and combined macro data
  quad_data %>% 
  mutate(prop_exp = (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2) %>% # calculated proportion of exposed urchins
  mutate(exposed_urchin = purple_urchin_densitym2 - purple_urchin_conceiledm2) %>%
  mutate(total_recruit = lamr + macr + macj + nerj + ptej + lsetj + eisj) %>%
  group_by(year, site, site_type, zone, transect) %>% 
  summarise(mean_urch_den = mean(purple_urchin_densitym2, na.rm = TRUE), # find means
            mean_urch_concealed = mean(purple_urchin_conceiledm2, na.rm = TRUE),
            total_exposed = sum(exposed_urchin, na.rm = TRUE),
            mean_exposed = mean(exposed_urchin, na.rm = TRUE),
            mean_urch_prop = mean(prop_exp, na.rm = TRUE),
            total_recruit = sum(total_recruit, na.rm = TRUE)) %>%
  left_join(.,kelp_data, by = c("year", "site", "site_type", "zone", "transect")) # join kelp_data to quad_data


###############################################################################

# PLOTS

###############################################################################

# Question 1. Is there a correlation between purple sea urchin density and kelp 
# density and recruitment?

# 1. Number of Kelp Stipes vs Mean Urchin Density

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = macro_stipe_density_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
#  facet_wrap(~site_type) + # adds site_type as a variable
  labs(x = "Mean purple sea urchin density", y = "Total Macrocystis pyrifera stipes")

# transforming data to see what happens

ggplot(data = quad_sum, aes(x = mean_urch_den)) + #original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

ggplot(data = quad_sum, aes(x = macro_stipe_density_20m2)) + #original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

urchin_den_log <- # transform urchin and stipe data with log +1
  quad_sum %>%
  mutate(logUrchin = log(mean_urch_den + 1), logStipes = log(macro_stipe_density_20m2 + 1))

ggplot(data = urchin_den_log, aes(x = logUrchin)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

ggplot(data = urchin_den_log, aes(x = logStipes)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

#ggplot(quad_sum, aes(x = mean_urch_den, y = macro_stipe_density_20m2)) +
#  geom_point() +                                    # scatter plot
#  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
#  theme_minimal()

ggplot(urchin_den_log, 
       aes(x = logUrchin, y = logStipes)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  labs(x = "Mean purple sea urchin density", y = "Total Macrocystis pyrifera stipes")


# 2. Number of Plants vs Mean Urchin Density

#ggplot(quad_sum, aes(x = mean_urch_den, y = n_macro_plants_20m2)) +
#  geom_point() +                                    # scatter plot
#  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
#  theme_minimal()

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = n_macro_plants_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$n_macro_plants_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
#  facet_wrap(~site_type) + # adds site_type as a variable
  labs(x = "Mean purple sea urchin density", y = "Total Number of Macrocystis pyrifera plants")



# 3. Number of Recruits vs Mean Purple Urchins

#ggplot(recruit_sum %>% filter(purple_urchin_densitym2 < 100),
#       aes(x = purple_urchin_densitym2, y = total_recruit)) +
#  geom_point() +                                    # scatter plot
#  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
#  theme_minimal()

ggplot(recruit_sum, 
       aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
#  facet_wrap(~site_type) +
  labs(x = "Mean purple urchin density", y = "Total number of recruits and juveniles")

# Transforming Data 

### Urch Density Variable

ggplot(data = recruit_sum, aes(x = mean_urch_den)) + #original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
  labs(title= "", x= "", y="") +
  theme_classic()

urchin_den_log <- # transform data with log +1
  recruit_sum %>%
  mutate(logUrchin = log(mean_urch_den + 1))

ggplot(data = urchin_den_log, aes(x = logUrchin)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

shapiro.test(urchin_den_log$logUrchin)

### Recruit Variable


ggplot(data = recruit_sum, aes(x = total_recruit)) + # original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
  labs(title= "", x= "", y="") +
  theme_classic() 


recruit_data_log <- # transform data with log +1
  recruit_sum %>%
  mutate(logRecruit = log(total_recruit + 1))


ggplot(data = recruit_data_log, aes(x = logRecruit)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

shapiro.test(recruit_data_log$logRecruit)  # P value close enough 

#recruit_data_sq <- # square root transformation
#  recruit_sum %>%
#  mutate(sqRecruit = sqrt(total_recruit))

#ggplot(data = recruit_data_sq, aes(x = sqRecruit)) + # transformed SQRT histogram
#  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
#  labs(title= "", x= "", y="") +
#  theme_classic()

# shapiro.test(recruit_data_sq$sqRecruit) 

### Transform both at same time

recruit_data_log <- # transform data with log +1
  recruit_sum %>%
  mutate(logRecruit = log(total_recruit + 1), logUrch = log(purple_urchin_densitym2 + 1))


ggplot(recruit_data_log, aes(x = logUrch, y = logRecruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_data_log$logRecruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal()



###############################################################################



# Question 2. Is there a correlation between purple sea urchin behavior (active or passive grazing)
# and kelp density and recruitment?

# 1. Total stipes vs exposed urchins

ggplot(quad_sum, 
       aes(x = mean_exposed, y = macro_stipe_density_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  labs(x = "Mean purple sea urchins exposed", y = "Total Macrocystis pyrifera stipes")

#  facet_wrap(~site_type) + # adds site_type as a variable


ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = macro_stipe_density_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  labs(x = "Mean proportion of purple sea urchins exposed", y = "Total Macrocystis pyrifera stipes")


# 2. Number of macro plants vs exposed urchins

ggplot(quad_sum, 
       aes(x = mean_exposed, y = n_macro_plants_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$n_macro_plants_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  labs(x = "Mean purple sea urchins exposed", y = "Total Macrocystis pyrifera plants")

# 3. Recruits vs exposed urchins


ggplot(recruit_sum, 
       aes(x = mean_urch_prop, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
#   facet_wrap(~site_type) +
  labs(x = "Mean proportion of purple urchins exposed", y = "Total number of recruits and juveniles")


ggplot(quad_sum, 
       aes(x = mean_exposed, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  #   facet_wrap(~site_type) +
  labs(x = "Mean purple urchins exposed", y = "Total number of recruits and juveniles")



################################################################################


# QUESTION 3: Can these relationships be used as predictors for kelp forest recovery?


# facet_wrap using site_type 

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  facet_wrap(~site_type) +
  labs(x = "Mean purple urchin density", y = "Total number of recruits and juveniles")

ggplot(quad_sum, 
       aes(x = mean_exposed, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal() +
  facet_wrap(~site_type) +
  labs(x = "Mean purple urchins exposed", y = "Total number of recruits and juveniles")


# bubble plot

ggplot(quad_sum,
       aes(x = mean_urch_den, 
           y = macro_stipe_density_20m2, 
           size = mean_exposed, 
           fill = site_type)) +
  geom_point(alpha = 0.5, shape = 21, color = "black") +
  scale_fill_viridis_d(option = "plasma", name = "Site type") +
  scale_size_continuous(range = c(2, 10), name = "Mean Exposed Urchins") +
  theme_classic() +
  labs(x = "Mean purple sea urchin density", 
       y = "Total Stipe Density") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(size = 5)))


ggplot(quad_sum,
       aes(x = mean_urch_den, 
           y = macro_stipe_density_20m2, 
           size = mean_exposed, 
           fill = site_type)) +
  geom_point(alpha = 0.5, shape = 21, color = "black") +
  scale_fill_manual(values = c("FOR" = "#009E73", 
                               "BAR" = "#E69F00", 
                               "INCIP" = "#0072B2")) +
  scale_size_continuous(range = c(2, 10), name = "Mean Exposed Urchins") +
  theme_classic() +
  labs(x = "Mean purple sea urchin density",
       y = "Total stipe density",
       fill = "Site Type") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(size = 5)))







###############################################################################
# OLD PROJECT

# Step 1: Make groupings to prep for normalizing data

View(quad_data) # rda for quad data

print(quad_data)



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

scores_z <- as.data.frame(scores(dist_z, display = "sites")) %>%
                    mutate(
                      site_type = groups$site_type,
                      year = groups$year,
                      year_site_type = paste(year, site_type)
                    )

scores_z$site_type <- groups$site_type  


scores_z <- scores_z %>% mutate(year_site_type = paste(year, site_type))

View(scores_z)

# Step 4: Visualizing with a NMDS Plot

## Characterized by site type 

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

## Characterized by year comparison 

ggplot() + 
  geom_point(
    data = scores_z, aes(x = NMDS1, y = NMDS2, color = year_site_type, shape = year_site_type), 
    size = 2, alpha = 0.2
  ) + 
  stat_ellipse(
    data = scores_z, aes(x = NMDS1, y = NMDS2, color = year_site_type) 
    , type = "norm", linetype = 1, size = 1
  ) +
  theme_bw()










