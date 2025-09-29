#####################
## Keenan's script ##
#####################

rm(list=ls())

################################################################################
# Load packages, data, set dir

require(librarian)
librarian::shelf(tidyverse, ggplot2, janitor, readxl, googlesheets4)

gs4_auth()

recovery_derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=344647275#gid=344647275",
                       sheet = 2) %>% clean_names()

margin_derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=931069509#gid=931069509", 
                              sheet = 2) %>% clean_names()

################################################################################
# Merge data

derm_recovery <- recovery_derm_raw %>% dplyr::select(species, size, count, diet, urchin_size)

derm_margin <- margin_derm_raw %>% dplyr::select(species, size, count, diet, urchin_size)

derm_merge <- rbind(derm_recovery, derm_margin) %>%
  mutate(urchin_size = ifelse(urchin_size == "NA", NA, urchin_size),
         urchin_size = ifelse(urchin_size == "NULL", NA, urchin_size))

################################################################################
# Prep data for Fig. 1

# Recovery diet

diet_overall <- derm_merge %>%
  filter(!is.na(diet), diet != "None") %>%
  count(diet) %>%
  arrange(desc(n)) %>%
  mutate(
    diet = factor(diet, levels = diet),        # preserve order
    prop = n / sum(n),
    xmin = lag(cumsum(prop), default = 0),
    xmax = cumsum(prop),
    xmid = (xmin + xmax) / 2                   # center label
  )

# Margin diet

margin_diet_overall <- margin_derm_raw %>%
  filter(!is.na(diet), diet != "None") %>%
  count(diet) %>%
  arrange(desc(n)) %>%
  mutate(
    diet = factor(diet, levels = diet),        # preserve order
    prop = n / sum(n),
    xmin = lag(cumsum(prop), default = 0),
    xmax = cumsum(prop),
    xmid = (xmin + xmax) / 2                   # center label
  )

################################################################################
# Consistent colors for all figures

my_colors <- c(
  "Other" = "#1B9E77", 
  "Urchin" = "#7570B3", 
  "Gastropod" = "#D95F02", 
  "Limpet" = "#E7298A", 
  "Chiton" = "#66A61E", 
  "Barnacle" = "#E6AB02"
)

my_colors_plot3 <- c(
  "Other prey" = "#1B9E77", 
  "Urchin" = "#7570B3"
)
  

################################################################################
# Fig. 1 - Plot diet composition

p <- ggplot(diet_overall) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = diet), color = "white") +
  geom_text(
    data = diet_overall %>% filter(prop > 0.04),
    aes(x = xmid, y = 0.5, label = n),
    color = "white", size = 4
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Proportion",
    y = NULL,
    fill = "Diet"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.text = element_text(size = 7)
  )

p

################################################################################
# Fig. 2 - Stacked bar -> shows how diet varies with star size - problem = needs to be equal number of observations per quartile

derm_merge$size_class <- cut(derm_merge$size, breaks = seq(0, max(derm_merge$size), by = 10))

ggplot(subset(derm_merge, !(diet %in% c("None"))), 
       aes(x = size_class, 
           y = count, 
           fill = diet)) + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  labs(x = "Star size (cm)", 
       y = "Proportion of diet", 
       fill = "Diet") + 
  scale_fill_manual(values = my_colors, 
                    breaks = c("Other", "Urchin", "Gastropod", "Limpet", "Chiton", "Barnacle")
                    ) + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.text = element_text(size = 7)
  )

################################################################################
# Fig. 3 - Star size vs. predation fq histogram -> compared size of stars eating other prey vs. 
# size of stars eating urchins -> urchin-eaters are larger on average

derm_merge_predation <- filter(derm_merge, diet != "None") %>%
  mutate(diet_condensed = recode(diet, 
                                 "Barnacle" = "Other prey", 
                                 "Chiton" = "Other prey", 
                                 "Gastropod" = "Other prey", 
                                 "Limpet" = "Other prey",
                                 "Other" = "Other prey", 
                                 "Urchin" = "Urchin"))
                                 
derm_size_avg <- aggregate(size ~ diet_condensed, data = derm_merge_predation, FUN = mean)

ggplot(derm_merge_predation, aes(x = size, weight = count, fill = diet_condensed)) + 
  geom_histogram(binwidth = 1, 
                 color = "white") + 
  geom_vline(derm_size_avg, 
             mapping = aes(xintercept = size, 
                           linetype = diet_condensed)) + 
  scale_linetype(labs(title = "Mean star size (cm)")) + 
  scale_fill_manual(values = my_colors_plot3, 
                    labs("Diet")) +  
  labs(x = 'Star size (cm)', 
       y = 'Predation frequency') + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.text = element_text(size = 7)
  )

################################################################################
# Wrangling

derm_merge_urchin <- filter(derm_merge, 
                            diet == "Urchin") # filter for just urchin eaters

# filter for all other prey
derm_merge_other <- derm_merge_predation %>% 
  filter(!(diet %in% 
             c("Urchin")))

################################################################################
# Visualization

# Fig. 4: histogram - star size class vs. predation fq

quartile_urchin <- derm_merge_urchin %>%
  uncount(weights = count)

q <- quantile(quartile_urchin$size, probs = c(0.33, 0.66))

ggplot(derm_merge_urchin, aes(x = size, 
                              weight = count)) + 
  geom_histogram(binwidth = 1, 
                 color = "white", 
                 fill = "#7570B3") + 
  geom_vline(xintercept = q[1]) + 
  geom_vline(xintercept = q[2]) + 
  labs(x = "Star size (cm)", 
       y = "Predation frequency") + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )

?geom_vline

# Fig. 5: scatterplot - star size vs. urchin size

ggplot(derm_merge_urchin, aes(x = size, 
                              y = urchin_size)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              color = "#7570B3") + 
  labs(x = "Star size (cm)", 
       y = "Urchin size (cm)") + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )

################################################################################
# Asking R to do math for me

# average derm size for other eaters and urchin eaters
mean(derm_merge_urchin$size) # n = 36
mean(derm_merge_other$size) # n = 101

# average urchin size
mean(derm_merge_urchin$urchin_size) # n = 36

# urchin prop of diet per star size class
304-222
7/82

138-91
22/47

22-14
7/8

################################################################################
# Export

# ggsave(
#  filename = here::here("~", "Downloads", "diet_composition_plot.png"),
#  plot = p,  
#  width = 6,
#  height = 2,
#  dpi = 600,
#  bg = "white"
#  )

################################################################################