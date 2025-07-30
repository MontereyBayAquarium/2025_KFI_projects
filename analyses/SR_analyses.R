#SR_analyses

# 1. Run First ---------------------------------------------------------------
rm(list=ls())

install.packages('librarian')
require(librarian)
librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)

library(librarian)
library(googlesheets4)
library(httpuv)
library(dplyr)

# 2. Data Sets -------------------------------------------------------------------

gs4_auth()

spawn_raw <- read_sheet("https://docs.google.com/spreadsheets/d/11DLr38iVRDcvWiDoBY1hl_Cn5M9oAxDwBRQsx_eMbHk/edit?usp=sharing") 

spawn_working <- subset(spawn_raw, select = -c(Data_Enterer,Gonad_Mass_total,Spawn_Mass_total,17,18,19,20,21,22))


# 3. Plots ----------------------------------------------------------------


