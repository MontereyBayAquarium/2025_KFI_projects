<<<<<<< HEAD
# Keenan's Script

rm(list=ls())

################################################################################
#Load packages, data, set dir

require(librarian)
librarian::shelf(tidyverse,ggplot2, janitor, readxl, googlesheets4)

gs4_auth()

derm_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 4) %>% clean_names()

################################################################################
#Load packages, data, set dir

# Step 1: prep data
diet_overall <- derm_raw %>%
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
#Plot size fq

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



# Export
# ggsave(
  # filename = here::here("~", "Downloads", "diet_composition_plot.png"),
  # plot = p,  
  # width = 6,
  # height = 2,
  # dpi = 600,
  # bg = "white"
  # )

=======
##Keenan's Script
>>>>>>> 88dc2b9b3ffc7afbb74cddbe3236e2ed74c5e430
