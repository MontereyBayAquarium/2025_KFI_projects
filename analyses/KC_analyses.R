# Keenan's script

rm(list=ls())

################################################################################
# Load packages, data, set dir

require(librarian)
librarian::shelf(tidyverse, ggplot2, janitor, readxl, googlesheets4)

gs4_auth()

derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 4) %>% clean_names()

margin_derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=1363312898#gid=1363312898", 
                              sheet = 5) %>% clean_names()

################################################################################
# Load packages, data, set dir

# Prep data

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
# Merge data

derm_recovery <- derm_raw %>% dplyr::select(species, size, count, diet, urchin_size)

derm_margin <- margin_derm_raw %>% dplyr::select(species, size, count, diet, urchin_size)

derm_merge <- rbind(derm_recovery, derm_margin) %>%
  mutate(urchin_size = ifelse(urchin_size =="NA", NA,urchin_size),
         urchin_size = ifelse(urchin_size == "NULL",NA, urchin_size))

################################################################################
# Plot diet composition

p <- ggplot(diet_overall) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = diet), color = "white") +
  geom_text(
    data = diet_overall %>% filter(prop > 0.04),
    aes(x = xmid, y = 0.5, label = n),
    color = "white", size = 4
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +  # ✨ Try "Set2", "Dark2", or "Paired"
  labs(
    title = "Diet composition of actively foraging Dermasterias",
    x = "Proportion",
    y = NULL,
    fill = "Diet"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size=10),
    legend.text = element_text(size=7)
  )

p

################################################################################
# Star size frequency histogram

ggplot(data = derm_merge, aes(x = size, weight = count)) + 
  geom_histogram(binwidth = 1, color = "white", fill = "steelblue") +
  labs(x = 'star_size_cm', y = 'star_count') + 
  theme_minimal()

################################################################################
# Overlayed histogram with quartiles -> star size fq vs. urchin predation fq

# ggplot(data = derm_merge, aes(x = x)) + 
  # geom_histogram(binwidth = 1, color = "white", fill = "steelblue") + 
  # geom_vline(xintercept = qs, linetype = "dashed", color = "red", linewidth = 1) + 
  # labs(x = "star_size", y = "predation_fq", 
       # title = "predation fq relative to star size",
       # subtitle = "red dashed lines = Q1, median, Q3")

################################################################################
# Overlayed histogram with quartiles -> urchin size fq vs. urchin predation fq



################################################################################
# Stacked bar -> shows how diet varies with star size - problem = needs to be equal number of observations per quartile

derm_merge$size_bin <- cut(derm_merge$size, breaks = seq(0, max(derm_merge$size), by = 5))

ggplot(subset(derm_merge, !(diet %in% c("None", "Other"))), 
       aes(x = size_bin, y = count, fill = diet)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(x = "Dermasterias size (binned, cm)", y = "Proportion of diet") + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal()

################################################################################
# Stacked bar with "Other" and "None" - problem = needs to be equal number of observations per quartile

derm_merge$size_bin <- cut(derm_merge$size, breaks = seq(0, max(derm_merge$size), by = 5))

ggplot(derm_merge, aes(x = size_bin, y = count, fill = diet)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(x = "Dermasterias size (binned, cm)", y = "Proportion of diet") + 
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal()

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