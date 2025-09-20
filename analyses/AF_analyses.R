# Aimee's script 

install.packages("librarian")
librarian::shelf("tidyverse", "vegan", "ggplot2", "dplyr", "car", "ggpubr", "minpack.lm", "googledrive",
                 "viridis", "plotly", "viridis", "cowplot", "ggpubr", "patchwork")

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

# Create new data frames

kelp_data <- kelp_data %>% # create new column for year in kelp_data
  mutate(year = year(survey_date)) %>%
  mutate(total_adult_stipes = macro_stipe_density_20m2 + density20m2_ptecal + density20m2_eisarb +
  density20m2_nerlue + density20m2_lamset + density20m2_lamstump + density20m2_macstump)

quad_data <- quad_data %>% # create new column for year in quad_data
  mutate(year = year(survey_date))


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
  left_join(.,kelp_data, by = c("year", "site", "site_type", "zone", "transect"))  # join kelp_data to quad_data

  
#recruit_sum <- # created new data frame to total recruits and juveniles + proportion of purps exposed
#  quad_data %>% 
#  mutate(prop_exp = (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2) %>%
#  mutate(total_recruit = lamr + macr + macj + nerj + ptej + lsetj + eisj) %>%
#  group_by(year, site, site_type, zone, transect) %>%
#  summarise(total_recruit = sum(total_recruit, na.rm = TRUE), 
#            mean_urch_prop = mean(prop_exp, na.rm = TRUE),
#            mean_urch_den = mean(purple_urchin_densitym2, na.rm = TRUE))

###############################################################################

# PLOTS

###############################################################################

# Question 1. Is there a correlation between purple sea urchin density and kelp 
# density and recruitment?

# 1. Number of Kelp Stipes vs Mean Urchin Density

# only macro stipes

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = macro_stipe_density_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
   xlim(0, 50) +
   theme_minimal() +
  labs(x = "Mean purple sea urchin density", y = "Total Macrocystis pyrifera stipes")


# total stipitates

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = total_adult_stipes)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_adult_stipes), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential 
  xlim(0, 50) +
  ylim(0, 200) +
  theme_minimal() +
  labs(x = "Mean purple sea urchin density", y = "Total stipitates")

# Make the line fit better:

# Fit nonlinear model with baseline

stipefit <- nls(total_adult_stipes ~ a * exp(-b * mean_urch_den),
           data = quad_sum,
           start = list(a = max(quad_sum$total_adult_stipes, na.rm = TRUE),
                        b = 0.1))

# Predictions

quad_sum$stipepred <- predict(fit)

# Calculate R²

ss_res_stipe <- sum((quad_sum$total_adult_stipes - quad_sum$stipepred)^2)
ss_tot_stipe <- sum((quad_sum$total_adult_stipes - mean(quad_sum$total_adult_stipes))^2)
r2_stipe <- 1 - ss_res/ss_tot 
r2_stipe <- 0.12


# Plot with R² annotation
ggplot(quad_sum, aes(x = mean_urch_den, y = total_adult_stipes)) +
  geom_point() +
  geom_line(aes(y = stipepred), color = "blue", size = 1) +
  annotate("text", x = 40, y = 170, 
           label = paste0("R² = ", round(r2, 2)),
           hjust = 0, size = 5, color = "red") +
  xlim(0, 50) +
  ylim(0, 200) +
  theme_minimal() +
  labs(x = "Mean purple sea urchin density", 
       y = "Total adult kelp stipitates",
       title = "Decline of Kelp Stipitates with Increasing Purple Sea Urchin Density")

# calculate a and b

coef(stipefit)

# a = 67, b = 0.16, x half = 4.33 m2

# Half-life x-value

x_half_stipe <- log(2)/0.16   # 4.33
y_half_stipe <- 67 / 2        # 33.5

# Create exponential predictions

quad_sum$stipepred <- 67 * exp(-0.16 * quad_sum$mean_urch_den)

ggplot(quad_sum, aes(x = mean_urch_den, y = total_adult_stipes)) +
  geom_point() +
  geom_line(aes(y = stipepred), color = "blue", size = 1) +
  geom_vline(xintercept = x_half_stipe, linetype = "dashed", color = "red") +
  geom_hline(yintercept = y_half_stipe, linetype = "dashed", color = "red") +
  annotate("text", x = x_half_stipe + 2, y = y_half_stipe + 5, 
           label = paste0("Half-life at x = ", round(x_half_stipe, 2)), 
           color = "red", hjust = -0.5) +
  annotate("text", x = 40, y = 150, 
           label = paste0("R² = ", round(r2_stipe, 2)), 
           color = "red", hjust = 0) +
  xlim(0, 50) +
  ylim(0, 200) +
  theme_minimal() +
  labs(
    title = "Effect of Purple Sea Urchin Density on Adult Kelp Stipitates",
    x = "Mean purple sea urchin density",
    y = "Total adult kelp stipitates"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# 2. Number of Plants vs Mean Urchin Density

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = n_macro_plants_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$n_macro_plants_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  xlim(0, 50) +
  theme_minimal() +
  labs(x = "Mean purple sea urchin density", y = "Total Number of Macrocystis pyrifera plants")



# 3. Number of Recruits vs Mean Purple Urchins

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  xlim(0, 50) +
  ylim(0, 100) +
  theme_minimal() +
  labs(x = "Mean purple urchin density", y = "Total number of recruits and juveniles")

# Fit 

recruitfit <- nls(total_recruit ~ a * exp(-b * mean_urch_den),
           data = quad_sum,
           start = list(a = max(quad_sum$total_recruit, na.rm = TRUE),
                        b = 0.1))


quad_sum$recruitpred <- predict(recruitfit)

ss_res_recruit <- sum((quad_sum$total_recruit - quad_sum$recruitpred)^2)
ss_tot_recruit <- sum((quad_sum$total_recruit - mean(quad_sum$total_recruit))^2)
r2_recruit <- 1 - ss_res/ss_tot 
r2_recruit
#<- 0.10

coef(recruitfit)

# a = 27 b = 0.26

x_half_recruit <- log(2)/0.26 # 2.67
y_half_recruit <- 27 / 2   # 13.5


quad_sum$recruitpred <- 27 * exp(-0.26 * quad_sum$mean_urch_den)


ggplot(quad_sum, aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  geom_line(aes(y = recruitpred), color = "blue", size = 1) +
  geom_vline(xintercept = x_half_recruit, linetype = "dashed", color = "red") +
  geom_hline(yintercept = y_half_recruit, linetype = "dashed", color = "red") +
  annotate("text", x = x_half_recruit + 2, y = y_half_recruit + 5, 
           label = paste0("Half-life at x = ", round(x_half_recruit, 2)), 
           color = "red", hjust = -0.5) +
  annotate("text", x = 40, y = 80, 
           label = paste0("R² = ", round(r2_recruit, 2)), 
           color = "red", hjust = 0) +
  xlim(0, 50) +
  ylim(0, 100) +
  theme_minimal() +
  labs(
    title = "Effect of Purple Sea Urchin Density on Kelp Recruitment",
    x = "Mean purple sea urchin density",
    y = "Total recruits and juveniles"
  ) +
  theme(plot.title = element_text(hjust = 0.5))




###############################################################################


# Question 2. Is there a correlation between purple sea urchin behavior (active or passive grazing)
# and kelp density and recruitment?

# 1. Total stipes vs exposed urchins

# total macro stipes

ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = macro_stipe_density_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$macro_stipe_density_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  ylim(0,100) +
  theme_minimal() +
  labs(x = "Proportion of exposed purple sea urchins", y = "Total Macrocystis pyrifera stipes")



# total stipitates

ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = total_adult_stipes)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_adult_stipes), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential 
  ylim(0, 200) +
  theme_minimal() +
  labs(x = "Proportion of exposed purple sea urchins", y = "Total stipitates")




prop_stipe_fit <- nls(total_adult_stipes ~ a * exp(-b * mean_urch_prop),
                      data = quad_sum,
                      start = list(a = max(quad_sum$total_adult_stipes), b = 0.1),
                      control = nls.control(maxiter = 500, warnOnly = TRUE))


quad_sum$prop_stipe_pred <- predict(prop_stipe_fit, newdata = quad_sum)

ss_res_prop_stipe <- sum((quad_sum$total_adult_stipes - quad_sum$prop_stipe_pred)^2)
ss_tot_prop_stipe <- sum((quad_sum$total_adult_stipes - mean(quad_sum$total_adult_stipes))^2)
r2_prop_stipe <- 1 - ss_res/ss_tot 
r2_prop_stipe

coef(prop_stipe_fit)

# a = 41 b = 2


x_half_prop_stipe <- log(2)/2 # 0.34
y_half_prop_stipe <- 41 / 2   # 20.4

x_half_prop_stipe
y_half_prop_stipe



quad_sum$prop_stipe_pred <- 41 * exp(-2.0 * quad_sum$mean_urch_prop)


ggplot(quad_sum, aes(x = mean_urch_prop, y = total_adult_stipes)) +
  geom_point() +
  geom_line(aes(y = prop_stipe_pred), color = "blue", size = 1) +
  geom_vline(xintercept = x_half_prop_stipe, linetype = "dashed", color = "red") +
  geom_hline(yintercept = y_half_prop_stipe, linetype = "dashed", color = "red") +
  annotate("text", x = x_half_prop_stipe + 0.02, y = y_half_prop_stipe + 5, 
           label = paste0("Half-life at x = ", round(x_half_prop_stipe, 2)), 
           color = "red", hjust = -0.7) +
  annotate("text", x = 0.7, y = 150, 
           label = paste0("R² = ", round(r2_prop_stipe, 2)), 
           color = "red", hjust = 0) +
  ylim(0, 200) +
  theme_minimal() +
  labs(
    title = "Effect of Exposed Purple Sea Urchins on Kelp Stipitates",
    x = "Proportion of exposed purple sea urchins",
    y = "Total adult kelp stipitates"
  ) +
  theme(plot.title = element_text(hjust = 0.5))






# 2. Number of macro plants vs exposed urchins

ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = n_macro_plants_20m2)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$n_macro_plants_20m2), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential 
  ylim(0, 40) + 
  theme_minimal() +
  labs(x = "Proportion of exposed purple sea urchins", y = "Total Macrocystis pyrifera plants")

# 3. Recruits vs exposed urchins


ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(recruit_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  ylim(0, 100) +
  theme_minimal() +
  labs(x = "Proportion of exposed purple sea urchins", y = "Total number of recruits and juveniles")

prop_recruit_fit <- nls(total_recruit ~ a * exp(-b * mean_urch_prop),
                      data = quad_sum,
                      start = list(a = max(quad_sum$total_recruit), b = 0.1),
                      control = nls.control(maxiter = 500, warnOnly = TRUE))


quad_sum$prop_recruit_pred <- predict(prop_recruit_fit, newdata = quad_sum)

ss_res_prop_recruit <- sum((quad_sum$total_recruit - quad_sum$prop_recruit_pred)^2)
ss_tot_prop_recruit <- sum((quad_sum$total_recruit - mean(quad_sum$total_recruit))^2)
r2_prop_recruit <- 1 - ss_res/ss_tot 
r2_prop_recruit

coef(prop_recruit_fit)

# a = 11 b = 1.8


x_half_prop_recruit <- log(2)/1.8 # 0.39
y_half_prop_recruit <- 11 / 2   # 5.5

x_half_prop_recruit
y_half_prop_recruit



quad_sum$prop_recruit_pred <- 11 * exp(-1.8 * quad_sum$mean_urch_prop)


ggplot(quad_sum, aes(x = mean_urch_prop, y = total_recruit)) +
  geom_point() +
  geom_line(aes(y = prop_recruit_pred), color = "blue", size = 1) +
  geom_vline(xintercept = x_half_prop_recruit, linetype = "dashed", color = "red") +
  geom_hline(yintercept = y_half_prop_recruit, linetype = "dashed", color = "red") +
  annotate("text", x = x_half_prop_recruit + 0.02, y = y_half_prop_recruit + 5, 
           label = paste0("Half-life at x = ", round(x_half_prop_recruit, 2)), 
           color = "red", hjust = -0.7) +
  annotate("text", x = 0.7, y = 80, 
           label = paste0("R² = ", round(r2_prop_recruit, 2)), 
           color = "red", hjust = 0) +
  ylim(0, 100) +
  theme_minimal() +
  labs(
    title = "Effect of Exposed Purple Sea Urchins on Kelp Recruitment",
    x = "Proportion of exposed purple sea urchins",
    y = "Total kelp recruits and juveniles"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################


# QUESTION 3: Can these relationships be used as predictors for kelp forest recovery?


# facet_wrap using site_type 

quad_sum$site_type <- factor(quad_sum$site_type, levels = c("BAR", "INCIP", "FOR")) #organize site type in correct order

ggplot(quad_sum, 
       aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  xlim(0, 50) +
  ylim(0, 100) +
  theme_minimal() +
  facet_wrap(~site_type) +
  labs(x = "Mean purple urchin density", y = "Total number of recruits and juveniles")


ggplot(quad_sum, 
       aes(x = mean_urch_prop, y = total_recruit)) +
  geom_point() +
  stat_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") + # negative exponential
  ylim(0, 50) +
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


# Josh's suggestion

ggplot(quad_sum,
       aes(x = mean_urch_den, 
           y = macro_stipe_density_20m2, 
           size = mean_urch_prop, 
           fill = site_type)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  scale_fill_manual(values = c("FOR" = "#009E73", 
                               "BAR" = "#E69F00", 
                               "INCIP" = "#0072B2")) +
  scale_size_continuous(range = c(2, 7), name = "Proportion of Exposed Urchins") +
  theme_classic() +
  labs(x = "Mean purple sea urchin density",
       y = "Total stipe density",
       fill = "Site Type") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# only macro stipes

ggplot(quad_sum, aes(
  x = mean_urch_den, 
  y = mean_urch_prop, 
  size = macro_stipe_density_20m2,
  fill = macro_stipe_density_20m2
)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  scale_size_continuous(range = c(2, 7), name = "Number of Stipes") +
  scale_fill_viridis_c(option = "C", name = "Number of Stipes", guide = "legend") +
  theme_classic() +
  labs(
    x = "Mean purple sea urchin density",
    y = "Proportion of exposed purple sea urchins"
  ) +
  theme(legend.position = "right") +
  guides(
    size = guide_legend(title = "Number of Stipes"),   # size legend will reflect actual bubble sizes
    fill = guide_legend(title = "Number of Stipes")    # fill legend will reflect colors
  ) +
  xlim(0, 50)

# total stiptates

ggplot(quad_sum, aes(
  x = mean_urch_den, 
  y = mean_urch_prop, 
  size = total_adult_stipes,
  fill = total_adult_stipes
)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  scale_size_continuous(range = c(2, 7), name = "Total number of stipes", limits = c(0, 200)) +
  scale_fill_viridis_c(option = "C", name = "Total number of stipes", guide = "legend", limits= c(0,200)) +
  theme_classic() +
  labs(
    x = "Mean purple sea urchin density",
    y = "Proportion of exposed purple sea urchins"
  ) +
  theme(legend.position = "right") +
  guides(
    size = guide_legend(title = "Total number of stipes"),   # size legend will reflect actual bubble sizes
    fill = guide_legend(title = "Total number of stipes")    # fill legend will reflect colors
  ) +
  xlim(0, 50)



# RECRUITS  VS STIPITATES


ggplot(quad_sum, aes(x = total_adult_stipes, y = total_recruit)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = 400, y = 200, label = paste0("R² = ", round(r2, 2)), color = "red", size = 5) +
  theme_minimal() +
  labs(
    x = "Total kelp stipitates",
    y = "Total kelp recruits"
  )

# 19% of the variation in total recruits is explained by total adult stipes

lin_fit <- lm(total_recruit ~ total_adult_stipes, data = quad_sum)
r2 <- summary(lin_fit)$r.squared

ggplot(quad_sum, aes(x = total_adult_stipes, y = total_recruit)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = 150, y = 95, label = paste0("R² = ", round(r2, 2)), color = "red", size = 5) +
  theme_minimal() +
  labs(
    x = "Total kelp stipitates",
    y = "Total kelp recruits",
    title = "Linear Fit of Kelp Recruits vs Adult Stipitates"
  ) +
  xlim(0, 200) +
  ylim(0, 100)



# patchwork to try to make drawing

plot1 <- 
  ggplot(quad_sum, aes(x = mean_urch_den, y = total_adult_stipes)) +
  geom_point() +
  geom_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_adult_stipes), b = 0.1)),
              se = FALSE,
              color = "blue") +
  theme_classic() + 
  xlim(0, 50) +
  ylim(0, 200) +
  facet_wrap(~site_type)

plot1

plot2 <-
  ggplot(quad_sum, aes(x = mean_urch_den, y = total_recruit)) +
  geom_point() +
  geom_smooth(method = "nls",   
              formula = y ~ a * exp(-b * x),
              method.args = list(start = list(a = max(quad_sum$total_recruit), b = 0.1)),
              se = FALSE,
              color = "blue") +
  theme_classic() +
  xlim(0, 50) + 
  ylim(0, 200) +
  facet_wrap(~site_type)

plot2

(plot1 / plot2) + 
  plot_annotation(tag_levels = "A") # getting there, but not ideal

# Try it with facet grid

quad_long <- quad_sum %>%
  pivot_longer(
    cols = c(total_recruit, total_adult_stipes),
    names_to = "variable",
    values_to = "value"
  )

# nls ggplot with uchin density 

ggplot(quad_long, aes(x = mean_urch_den, y = value)) +
  geom_point() +
  geom_smooth(
    method = "nls",
    formula = y ~ a * exp(-b * x),
    method.args = list(start = list(a = max(quad_long$value), b = 0.1)),
    se = FALSE,
    color = "blue"
    ) +
  facet_grid(variable ~ site_type, switch = "y", 
             labeller = 
               labeller(variable = c(
                total_recruit = "Kelp recruits and juveniles",
                total_adult_stipes = "Adult kelp stipitates"))) +
  theme_minimal() +
  xlim(0, 50) +
  ylim(0, 200) +
  labs(
    x = "Mean purple sea urchin density",
    y = "",
    title = "Relationship Between Purple Sea Urchin Density on Kelp Recruitment
    and Persistance Across Patch Types") +
  theme(
    strip.placement = "outside",               # push strips outward
    strip.switch.pad.grid = unit(0.1, "cm"), 
    axis.text.y = element_text(margin = margin(r = 0)) # pull y-ticks inward
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(data = r2_df, 
            aes(x = 40, y = 160, label = paste0("R² = ", round(r2, 2))),
            inherit.aes = FALSE, color = "red")


# Calculate r^2 for each fit

r2_df <- quad_long %>%
  group_by(site_type, variable) %>%
  summarise(
    r2 = {
      df <- cur_data()
      fit <- tryCatch(
        nls(value ~ a * exp(-b * mean_urch_den),
            data = df,
            start = list(a = max(df$value), b = 0.1),
            control = nls.control(maxiter = 200, warnOnly = TRUE)),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds <- predict(fit)
        ss_res <- sum((df$value - preds)^2)
        ss_tot <- sum((df$value - mean(df$value))^2)
        1 - ss_res/ss_tot
      } else NA
    },
    .groups = "drop"
  )

r2_df # mean purple sea urchin is a weak indicator of adult stipitates and kelp recruits across site_type


# nls ggplot with proportion of urchins exposed

ggplot(quad_long, aes(x = mean_urch_prop, y = value)) +
  geom_point() +
  geom_smooth(
    method = "nls",
    formula = y ~ a * exp(-b * x),
    method.args = list(start = list(a = max(quad_long$value), b = 0.1)),
    se = FALSE,
    color = "blue"
  ) +
  facet_grid(variable ~ site_type, switch = "y", 
             labeller = 
               labeller(variable = c(
                 total_recruit = "Kelp recruits and juveniles",
                 total_adult_stipes = "Adult kelp stipitates"))) +
  theme_minimal() +
  ylim(0, 200) +
  labs(
    x = "Proportion of exposed purple sea urchins",
    y = "",
    title = "Effects of Exposed Purple Sea Urchins on Kelp Recruitment 
    and Persistance Across Patch Types") +
  theme(
    strip.placement = "outside",               # push strips outward
    strip.switch.pad.grid = unit(0.1, "cm"), 
    axis.text.y = element_text(margin = margin(r = 0)) # pull y-ticks inward
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# calculate r^2 for proportion exposed

r2_prop_df <- quad_long %>%
  group_by(site_type, variable) %>%
  summarise(
    r2 = {
      df <- cur_data()
      fit <- tryCatch(
        nls(value ~ a * exp(-b * mean_urch_prop),
            data = df,
            start = list(a = max(df$value), b = 0.1),
            control = nls.control(maxiter = 200, warnOnly = TRUE)),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds <- predict(fit)
        ss_res <- sum((df$value - preds)^2)
        ss_tot <- sum((df$value - mean(df$value))^2)
        1 - ss_res/ss_tot
      } else NA
    },
    .groups = "drop"
  )

r2_prop_df # yikes, even lower than mean urchin density


cor(quad_sum$mean_urch_den, quad_sum$mean_urch_prop) # 1 -> they are interchangeable




##############################################################################

# Exploring Data and Transforming

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

# don't need to transform!


### Urch Density Variable

ggplot(data = recruit_sum, aes(x = mean_urch_den)) + #original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
  labs(title= "", x= "", y="") +
  theme_classic()


ggplot(data = urchin_den_log, aes(x = logUrchin)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

shapiro.test(urchin_den_log$logUrchin)

ggplot(data = recruit_sum, aes(x = total_recruit)) + # original histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 4) +
  labs(title= "", x= "", y="") +
  theme_classic() 


ggplot(data = recruit_data_log, aes(x = logRecruit)) + # transformed log+1 histogram
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 6) +
  labs(title= "", x= "", y="") +
  theme_classic()

shapiro.test(recruit_data_log$logRecruit)  # P value close enough 


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

# don't need to transform!





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










