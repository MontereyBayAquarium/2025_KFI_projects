# Aimee's script 

install.packages("librarian")
librarian::shelf("tidyverse", "vegan", "ggplot2, dplyr", "car", "ggpubr", "minpack.lm")

# Load data

setwd("~/Desktop/MBA_code_repository/2025_KFI_projects/output/processed")

df <- load("cleaned_survey_data.Rda")


# KELP vs URCHINS

View(macro_data)

# 1. Create new data frames

macro_data <- macro_data %>% # create new column for year
  mutate(year = year(survey_date))


recruit_sum <- # created new data frame to total recruits and juveniles + proportion of purps exposed
  quad_data %>% 
  mutate(prop_exp = (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2) %>%
  mutate(total_recruit = lamr_densitym2 + macr_densitym2 + macj_densitym2 + ptej_densitym2 + 
           nerj_densitym2 + lsetj_densitym2 + eisj_densitym2) %>%
  group_by(year, site, site_type, zone, transect) %>%
  summarise(total_recruit = sum(total_recruit, na.rm = TRUE), 
            prop_exp = sum(prop_exp, na.rm = TRUE),
            mean_urch_den = mean(purple_urchin_densitym2, na.rm = TRUE))

quad_sum <- # created new data frame with mean urch + purps exposed and combined macro data
  quad_data %>% 
  mutate(prop_exp = (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2) %>% # calculated proportion of exposed urchins
  group_by(year, site, site_type, zone, transect) %>% 
  summarise(mean_urch_den = mean(purple_urchin_densitym2, na.rm = TRUE), # find means
            mean_urch_concealed = mean(purple_urchin_conceiledm2, na.rm = TRUE),
            mean_urch_prop = mean(prop_exp, na.rm = TRUE)) %>%
  left_join(.,macro_data, by = c("year", "site", "site_type", "zone", "transect")) # join macro_data to quad_data
  
# 1. Number of Kelp Stipes vs Mean Urchin Density

ggplot(quad_sum, aes(x = mean_urch_den, y = macro_stipe_density_20m2)) +
  geom_point() +                                    # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
  theme_minimal()

# 2. Number of Plants vs Mean Urchin Density

ggplot(quad_sum, aes(x = mean_urch_den, y = n_macro_plants_20m2)) +
  geom_point() +                                    # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
  theme_minimal()

# 3. Number of Recruits vs Total Purple Urchins

ggplot(recruit_sum %>% filter(purple_urchin_densitym2 < 100),
       aes(x = purple_urchin_densitym2, y = total_recruit)) +
  geom_point() +                                    # scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear regression line
  theme_minimal()

ggplot(recruit_sum, 
       aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  theme_minimal()


## Transforming Data 

### Urch Density Variable

ggplot(data = recruit_sum, aes(x = purple_urchin_densitym2)) + #original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
  labs(title= "", x= "", y="") +
  theme_classic()

urchin_den_log <- # transform data with log +1
  recruit_sum %>%
  mutate(logUrchin = log(purple_urchin_densitym2 + 1))

ggplot(data = urchin_den_log, aes(x = logUrchin)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
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
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
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



## Trying to fit nls () 

# Fit negative exponential model

exp_model <- nls(total_recruit ~ a * exp(-b * mean_urch_den),
                 data = recruit_sum,
                 start = list(a = max(recruit_sum$total_recruit, na.rm = TRUE), 
                              b = 0.1))

summary(exp_model)

# Create prediction data across the observed urchin density

newdata <- data.frame(mean_urch_den = seq(min(recruit_sum$mean_urch_den, na.rm = TRUE),
                                                    max(recruit_sum$mean_urch_den, na.rm = TRUE),
                                                    length.out = 200))


newdata$pred <- predict(exp_model, newdata)

# Plot -> looks the same as other ggplot :( 

ggplot(recruit_sum, aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  geom_line(data = newdata, aes(x = mean_urch_den, y = pred),
            color = "red", size = 1) +
  theme_minimal()










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









