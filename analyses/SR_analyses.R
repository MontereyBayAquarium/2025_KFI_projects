install.packages('librarian')
require(librarian)
librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive,googlesheets4,httpuv,dplyr,ggplot2,pwr2)


# Data Sets ---------------------------------------------------------------

spawn_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11DLr38iVRDcvWiDoBY1hl_Cn5M9oAxDwBRQsx_eMbHk/edit?usp=sharing") 
spawn_raw

#delete unnecessary rows
spawn_working <- subset(spawn_raw, select = -c(Data_Enterer,Gonad_Mass_total,Spawn_Mass_total,Treatment,Species))
spawn_working

#make values numeric
spawn_working$Spawn_Mass_g <- as.numeric(as.character(spawn_working$Spawn_Mass_g))
spawn_working$Spawn_Mass_false <- as.numeric(as.character(spawn_working$Spawn_Mass_false))
spawn_working$Animal_24hr_Mass_g <- as.numeric(spawn_working$Animal_24hr_Mass_g)

#total count of urchins spawned for each ecosystem state
total_counts<- spawn_working%>%
  data.frame()%>%
  filter(!is.na(State),!is.na(Sex))%>%
  filter(Spawn_Mass_false != 0)%>%
  filter(!(Sex == "NA"))%>%
  mutate(State=factor(State),
         Sex=factor(Sex))%>%
  group_by(State)%>%
  summarize(total_n=n())

#total count of each sex spawned for each ecosystem state
sex_counts<- spawn_working%>%
  data.frame()%>%
  filter(!is.na(State),!is.na(Sex))%>%
  filter(Spawn_Mass_false != 0)%>%
  filter(!(Sex == "NA"))%>%
  mutate(State=factor(State),
         Sex=factor(Sex))%>%
  group_by(State,Sex)%>%
  summarize(sex_counts=n(),.groups="drop")%>%
  left_join(total_counts,by="State")%>%
  mutate(prop= (sex_counts/total_n))

# Figures -----------------------------------------------------------------

#boxplot overall spawn mass per ecosystem state
ggplot(data = spawn_working, aes(x = State, y = Spawn_Mass_false)) +
  geom_boxplot() +
  #facet_wrap(~ State) +
  labs(
    x = "Ecosystem State",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

#boxplot spawn mass per ecosystem state by sex
ggplot(data = spawn_working %>% 
         filter(Spawn_Mass_false != 0)
       , aes(x = State, y = Spawn_Mass_g)) +
  geom_boxplot() +
  facet_wrap(~ Sex) +
  labs(
    x = "Ecosystem State",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

#boxplot 24hr urchin mass per ecosystem state by sex
ggplot(data = spawn_working, aes(x = Sex, y = Animal_24hr_Mass_g, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ State) +
  labs(
    x = "Sex",
    y = "Mass (g) ",
    fill = "Sex") +
  theme_classic()

#sex distribution for each state 
ggplot(data = sex_counts
       , aes(x = State, y = prop, fill=Sex)) +
  geom_col() +
  #facet_wrap(~ Sex) +
  labs(
    x = "Ecosystem State",
    y = "Percent (%) ",
    fill = "Sex") +
  theme_classic()

#


# Stats -------------------------------------------------------------------



# To Do -------------------------------------------------------------------

#1 - update ecosystem states
#2 - power analysis
#3 - distribution of spawn mass by sex per square meter, and total spawn mass by individuals per square meter
#4 - sampling distribution of gonad mass for all individuals sampled at a given site, then randomly draw from that distribution for the number of urchins observed per square meter, then simply sum the gonad mass across individuals to get total gonad biomass/m2

