## May 9 2017
## Andrea Gaede
## Make PSTH from raw data and stimulus file


library(purrr)
library(tidyverse)
library(readr)
library(stringr)

## set working directory to the directory holding the folder we want to look inside
setwd("~/Documents/Projects/Flocculus-recording")

raw_directory <- "2017_flocculus"
stimulus_directory <- "2017_flocculus_stimulus_files"


raw_cell_files <- list.files(raw_directory, full.names = TRUE)
stimulus_files <- list.files(stimulus_directory, full.names = TRUE)

stim1 <- read.csv(stimulus_files[1], header = FALSE)

raw1 <- read.csv(raw_cell_files[2], header = TRUE)
names(raw1) <- c("time", "cell1", "cell2", "stimulus")
names(stim1) <- c("direction", "speed", "size")

stim1_reconfig <- stim1 %>% 
  slice(1:nrow(stim1)-1) %>% 
  select(direction, speed)
## why am I getting an NA row?

nrow(stim1)-1

stim_switch <- raw1 %>%
  select(time, stimulus) %>% 
  filter(stimulus == 1)

bind_cols(stim_switch, stim1)

left_join()

