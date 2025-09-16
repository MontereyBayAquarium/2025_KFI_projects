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
# Merge data

derm_recovery <- derm_raw %>% dplyr::select(species, size, count, diet, urchin_size)

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
# Fig. 1 - Plot diet composition

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
    plot.title = element_text(size = 14, hjust = 0.1),
    legend.text = element_text(size = 7)
  )

p

################################################################################
# Fig. 2 - Stacked bar -> shows how diet varies with star size - problem = needs to be equal number of observations per quartile

derm_merge$size_bin <- cut(derm_merge$size, breaks = seq(0, max(derm_merge$size), by = 5))

ggplot(subset(derm_merge, !(diet %in% c("None"))), 
       aes(x = size_bin, 
           y = count, 
           fill = diet)) + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  labs(title = "Diet composition of different Dermasterias size classes", 
       x = "Dermasterias size (binned, cm)", 
       y = "Proportion of diet", fill = "Diet") + 
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.title = element_text(size = 14, hjust = 0.1), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_blank(), 
        legend.box.background = element_blank())

################################################################################
# Fig. 3 - Star size vs. predation fq histogram -> compared size of stars eating other prey vs. 
# size of stars eating urchins -> urchin-eaters are larger on average

derm_merge_predation <- filter(derm_merge, diet != "None") %>%
  mutate(diet_condensed = recode(diet, 
                                 "Barnacle" = "Other Prey", 
                                 "Chiton" = "Other Prey", 
                                 "Gastropod" = "Other Prey", 
                                 "Limpet" = "Other Prey",
                                 "Other" = "Other Prey", 
                                 "Urchin" = "Urchin"))
                                 
derm_size_avg <- aggregate(size ~ diet_condensed, data = derm_merge_predation, FUN = mean)

ggplot(derm_merge_predation, aes(x = size, weight = count, fill = diet_condensed)) + 
  geom_histogram(binwidth = 1, 
                 color = "white") + 
  geom_vline(derm_size_avg, 
             mapping = aes(xintercept = size, 
                           linetype = diet_condensed)) + 
  scale_linetype(labs(title = "Average Star Size")) + 
  scale_fill_manual(values = c("steelblue", "maroon"), 
                    labs("Diet")) + 
  labs(title = 'Predation frequency of actively foraging Dermasterias', 
       x = 'Star Size (cm)', 
       y = 'Predation Frequency') + 
  theme(plot.title = element_text(size = 14, hjust = 0.1), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_blank(), 
        legend.box.background = element_blank())

################################################################################
# Fig. 4 - Dot plot: star size vs. urchin size

ggplot(derm_merge, aes(x = size, y = urchin_size)) + 
  geom_point()

################################################################################
# Fig. 5 - 



################################################################################
# Stacked bar with "Other" and "None" - problem = needs to be equal number of observations per quartile

# derm_merge$size_bin <- cut(derm_merge$size, breaks = seq(0, max(derm_merge$size), by = 5))

# ggplot(derm_merge, aes(x = size_bin, y = count, fill = diet)) + 
  # geom_bar(stat = "identity", position = "fill") + 
  # labs(x = "Dermasterias size (binned, cm)", y = "Proportion of diet") + 
  # scale_fill_brewer(palette = "Set2") + 
  # theme_minimal()

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