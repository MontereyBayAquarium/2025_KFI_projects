#SR_analyses

# 1. Run First ---------------------------------------------------------------

install.packages('librarian')
require(librarian)
librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)

library(librarian)
library(googlesheets4)
library(tidyverse)
library(httpuv)
library(dplyr)
library(ggplot2)

# 2. Data Sets -------------------------------------------------------------------

gs4_auth()

spawn_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11DLr38iVRDcvWiDoBY1hl_Cn5M9oAxDwBRQsx_eMbHk/edit?usp=sharing") 

spawn_working <- subset(spawn_raw, select = -c(Data_Enterer,Gonad_Mass_total,Spawn_Mass_total,19,20,21,22,23))

spawn_working$Spawn_Mass_g <- as.numeric(as.character(spawn_working$Spawn_Mass_g))
spawn_working$Spawn_Mass_false <- as.numeric(as.character(spawn_working$Spawn_Mass_false))

spawn_working$Spawn_Mass_false <- as.numeric(spawn_working$Spawn_Mass_false)
spawn_working$Animal_24hr_Mass_g <- as.numeric(spawn_working$Animal_24hr_Mass_g)

outlier_values <- boxplot.stats(spawn_working$Animal_24hr_Mass_g)$out

# 3. Plots ----------------------------------------------------------------

ggplot(data = spawn_working, aes(x = Ecosystem, y = Spawn_Mass_g, color = Sex)) +
  geom_point()

#spawn with full dataset
ggplot(data = spawn_working, aes(x = Ecosystem, y = Spawn_Mass_g)) +
  geom_boxplot() +
  #facet_wrap(~ Ecosystem) +
  labs(
    title = "Spawn Mass by Sex and Ecosystem Type",
    x = "Sex",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

ggplot(data = spawn_working %>% 
         filter(Spawn_Mass_false != 0)
         , aes(x = Ecosystem, y = Spawn_Mass_g)) +
  geom_boxplot() +
  facet_wrap(~ Sex) +
  labs(
    title = "Spawn Mass by Sex and Ecosystem Type",
    x = "Sex",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

#spawn without blue/yellow lines
ggplot(data = spawn_working, aes(x = Ecosystem, y = Spawn_Mass_false)) +
  geom_boxplot() +
  #facet_wrap(~ Ecosystem) +
  labs(
    title = "Spawn Mass by Sex and Ecosystem Type",
    x = "Sex",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

#urchin mass
ggplot(data = spawn_working, aes(x = Sex, y = Animal_24hr_Mass_g, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Ecosystem) +
  labs(
    title = "Urchin Mass by Sex and Ecosystem Type",
    x = "Sex",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

total_counts<- spawn_working%>%
   data.frame()%>%
   filter(!is.na(Ecosystem),!is.na(Sex))%>%
   filter(Spawn_Mass_false != 0)%>%
  filter(!(Sex == "NA"))%>%
   mutate(Ecosystem=factor(Ecosystem),
         Sex=factor(Sex))%>%
  group_by(Ecosystem)%>%
  summarize(total_n=n())

sex_counts<- spawn_working%>%
                  data.frame()%>%
                  filter(!is.na(Ecosystem),!is.na(Sex))%>%
                  filter(Spawn_Mass_false != 0)%>%
                  filter(!(Sex == "NA"))%>%
                  mutate(Ecosystem=factor(Ecosystem),
                         Sex=factor(Sex))%>%
                  group_by(Ecosystem,Sex)%>%
                  summarize(sex_counts=n(),.groups="drop")%>%
                  left_join(total_counts,by="Ecosystem")%>%
                  mutate(prop= (sex_counts/total_n))


#Sex Counts
ggplot(data = sex_counts
       , aes(x = Ecosystem, y = prop, fill=Sex)) +
  geom_col() +
  #facet_wrap(~ Sex) +
  labs(
    title = "Spawn Mass by Sex and Ecosystem Type",
    x = "Sex",
    y = "Count ",
    fill = "Sex") +
  theme_classic()

