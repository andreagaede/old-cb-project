
## TESTING SPEED TUNING WIDTH IN ZEBRA FINCH LM

## set working directory to the directory holding the folder we want to look inside
setwd("~/Documents/Projects/Flocculus-recording")
## setwd("~/Documents/Projects/Flocculus-recording/2016 - CB speed working data")


library(purrr)
library(tidyverse)
library(readr)
library(stringr)

##############################
##   Analyze LM activity   ##
##############################
lm_cell_directory <- "LM-ZF-cells"

lm_cell_files <- list.files(lm_cell_directory, full.names = TRUE)

names(lm_cell_files) <- list.files(lm_cell_directory) %>% 
  gsub(pattern = ".csv$", replacement = "")

## Alternative method I'd like to figure out later:
## set_names(map(list.files(cell_directory), list.files(cell_directory, full.names = TRUE)), 
## gsub(list.files(cell_directory), pattern = ".csv$", replacement = ""))


## add IDs and make giant df
lm_all_cells <- map_df(lm_cell_files, read_csv, col_names = FALSE,
                        .id = "bird_id")
## Name columns
lm_all_cells <- set_names(lm_all_cells, nm = c("bird_id", "direction", "speed", paste("bin", as.character(1:20), sep = "")))

lm_bin_mean <- lm_all_cells %>% 
  select(-bird_id, -direction, -speed) %>% 
  rowMeans()

## fix untidy directions
lm_all_cells$direction <- round(lm_all_cells$direction)

## Gives you single spike rate for each 5 second sweep
lm_all_cells_intermediate <- lm_all_cells %>% 
  select(bird_id, direction, speed) %>% 
  mutate(mean_spike_r = lm_bin_mean)

n_reps <- lm_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarize(n = n())

lm_mean_baseline <- lm_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  rename(mean_base = mean, sd_base = sd) %>% 
  filter(speed == "NaN") %>% 
  ungroup() %>% 
  select(bird_id, mean_base, sd_base)

lm_all_cells_intermediate <- left_join(lm_all_cells_intermediate, lm_mean_baseline, by = "bird_id")


lm_find_max_sweep <- lm_all_cells_intermediate %>% 
  group_by(bird_id) %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_base)) %>% 
  summarize(max_r = max(norm_spike_r)) 

lm_all_cells_intermediate <- left_join(lm_all_cells_intermediate, lm_find_max_sweep, by = "bird_id")


## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
lm_all_cells_norm <- lm_all_cells_intermediate %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_base)/max_r)) %>% 
  select(bird_id, speed, direction, norm_spike_r) %>% 
  group_by(bird_id, speed, direction) %>%
  filter(!is.na(speed)) %>% 
  summarize_each(funs(mean, sd)) %>% 
  rename(norm_mean = mean, norm_sd = sd)


## get speed in degrees/s instead of pixels/frame
lm_all_cells_norm$degrees <- acos(((sqrt((((lm_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((lm_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2-((lm_all_cells_norm$speed)*144*0.02768)^2)/(2*(sqrt((((lm_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((lm_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))))*180/pi
lm_all_cells_norm$degrees <- round(lm_all_cells_norm$degrees, 2)



## add number of sweeps so we can caluclate SEM
lm_all_cells_norm <- left_join(lm_all_cells_norm, n_reps, by = c("bird_id", "speed", "direction"))

## calculate SEM
lm_all_cells_norm <- lm_all_cells_norm %>%
  mutate(norm_sem = norm_sd/sqrt(n))

#### Write this to csv so it can be read into other scripts
# write_csv(all_cells_norm, "all_cells_norm.csv")
# write_csv(all_cells_intermediate, "all_cells_intermediate.csv")

## find the max speed for every cell
lm_my_data_max <- lm_all_cells_norm %>% 
  group_by(bird_id) %>% 
  filter(norm_mean == max(norm_mean))



##############################
##   Analyze CSA activity   ##
##############################
csa_cell_directory <- "2016_CB_CSA_working_data"

csa_cell_files <- list.files(csa_cell_directory, full.names = TRUE)

names(csa_cell_files) <- list.files(csa_cell_directory) %>% 
  gsub(pattern = ".csv$", replacement = "")

## Alternative method I'd like to figure out later:
## set_names(map(list.files(cell_directory), list.files(cell_directory, full.names = TRUE)), 
## gsub(list.files(cell_directory), pattern = ".csv$", replacement = ""))


## add IDs and make giant df
csa_all_cells <- map_df(csa_cell_files, read_csv, col_names = FALSE,
                        .id = "bird_id")
## Name columns
csa_all_cells <- set_names(csa_all_cells, nm = c("bird_id", "direction", "speed", paste("bin", as.character(1:20), sep = "")))

csa_bin_mean <- csa_all_cells %>% 
  select(-bird_id, -direction, -speed) %>% 
  rowMeans()

## fix untidy directions
csa_all_cells$direction <- round(csa_all_cells$direction)

## Gives you single spike rate for each 5 second sweep
csa_all_cells_intermediate <- csa_all_cells %>% 
  select(bird_id, direction, speed) %>% 
  mutate(mean_spike_r = csa_bin_mean)

n_reps <- csa_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarize(n = n())

csa_mean_baseline <- csa_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  rename(mean_base = mean, sd_base = sd) %>% 
  filter(speed == "NaN") %>% 
  ungroup() %>% 
  select(bird_id, mean_base, sd_base)

csa_all_cells_intermediate <- left_join(csa_all_cells_intermediate, csa_mean_baseline, by = "bird_id")


csa_find_max_sweep <- csa_all_cells_intermediate %>% 
  group_by(bird_id) %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_base)) %>% 
  summarize(max_r = max(norm_spike_r)) 

csa_all_cells_intermediate <- left_join(csa_all_cells_intermediate, csa_find_max_sweep, by = "bird_id")


## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
csa_all_cells_norm <- csa_all_cells_intermediate %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_base)/max_r)) %>% 
  select(bird_id, speed, direction, norm_spike_r) %>% 
  group_by(bird_id, speed, direction) %>%
  filter(!is.na(speed)) %>% 
  summarize_each(funs(mean, sd)) %>% 
  rename(norm_mean = mean, norm_sd = sd)


## get speed in degrees/s instead of pixels/frame
csa_all_cells_norm$degrees <- acos(((sqrt((((csa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((csa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2-((csa_all_cells_norm$speed)*144*0.02768)^2)/(2*(sqrt((((csa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((csa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))))*180/pi
csa_all_cells_norm$degrees <- round(csa_all_cells_norm$degrees, 2)



## add number of sweeps so we can caluclate SEM
csa_all_cells_norm <- left_join(csa_all_cells_norm, n_reps, by = c("bird_id", "speed", "direction"))

## calculate SEM
csa_all_cells_norm <- csa_all_cells_norm %>%
  mutate(norm_sem = norm_sd/sqrt(n))

#### Write this to csv so it can be read into other scripts
# write_csv(all_cells_norm, "all_cells_norm.csv")
# write_csv(all_cells_intermediate, "all_cells_intermediate.csv")

## find the max speed for every cell
csa_my_data_max <- csa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  filter(norm_mean == max(norm_mean))


# might want to find better way of turning speeds into a factor earlier on in script
csa_my_data_max_graph <- csa_my_data_max %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = nn)

num_cells <- sum(csa_my_data_max_graph$num_max)

csa_my_data_max_graph <- csa_my_data_max_graph %>% 
  mutate(proportion_pop = num_max/num_cells)

csa_all_speeds <- csa_all_cells_norm %>% 
  ungroup() %>% 
  select(degrees) %>% 
  distinct()

csa_my_data_max_graph_na <- full_join(csa_all_speeds, csa_my_data_max_graph)

## my_data_max_graph$degrees <- as.factor(my_data_max_graph$degrees)

csa_my_data_max_graph_na$degrees <- as.factor(csa_my_data_max_graph_na$degrees)

## If just using CSA, here is the graphing code:
c1 <- ggplot(csa_my_data_max_graph_na, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#F79441") + #  color = "black" gives black outlines
  ggtitle("CSA - Proportion of population with max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")
c1
## ggsave("csa_max_speed.pdf", plot = c1, width = 11, height = 8.5, units = c("in"))


##############################
## Now analyze SSA activity ##
##############################

ssa_cell_directory <- "2016_CB_SSA_working_data"

ssa_cell_files <- list.files(ssa_cell_directory, full.names = TRUE)

names(ssa_cell_files) <- list.files(ssa_cell_directory) %>% 
  gsub(pattern = ".csv$", replacement = "")

## Alternative method I'd like to figure out later:
## set_names(map(list.files(cell_directory), list.files(cell_directory, full.names = TRUE)), 
## gsub(list.files(cell_directory), pattern = ".csv$", replacement = ""))


## add IDs and make giant df
ssa_all_cells <- map_df(ssa_cell_files, read_csv, col_names = FALSE,
                        .id = "bird_id")
## Name columns
ssa_all_cells <- set_names(ssa_all_cells, nm = c("bird_id", "direction", "speed", paste("bin", as.character(1:20), sep = "")))

ssa_bin_mean <- ssa_all_cells %>% 
  select(-bird_id, -direction, -speed) %>% 
  rowMeans()

## fix untidy directions
ssa_all_cells$direction <- round(ssa_all_cells$direction)

## Gives you single spike rate for each 5 second sweep
ssa_all_cells_intermediate <- ssa_all_cells %>% 
  select(bird_id, direction, speed) %>% 
  mutate(mean_spike_r = ssa_bin_mean)

n_reps <- ssa_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarize(n = n())

ssa_mean_baseline <- ssa_all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  rename(mean_base = mean, sd_base = sd) %>% 
  filter(speed == "NaN") %>% 
  ungroup() %>% 
  select(bird_id, mean_base, sd_base)

ssa_all_cells_intermediate <- left_join(ssa_all_cells_intermediate, ssa_mean_baseline, by = "bird_id")


ssa_find_max_sweep <- ssa_all_cells_intermediate %>% 
  group_by(bird_id) %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_base)) %>% 
  summarize(max_r = max(norm_spike_r)) 

ssa_all_cells_intermediate <- left_join(ssa_all_cells_intermediate, ssa_find_max_sweep, by = "bird_id")


## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
ssa_all_cells_norm <- ssa_all_cells_intermediate %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_base)/max_r)) %>% 
  select(bird_id, speed, direction, norm_spike_r) %>% 
  group_by(bird_id, speed, direction) %>%
  filter(!is.na(speed)) %>% 
  summarize_each(funs(mean, sd)) %>% 
  rename(norm_mean = mean, norm_sd = sd)


## get speed in degrees/s instead of pixels/frame
ssa_all_cells_norm$degrees <- acos(((sqrt((((ssa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((ssa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2-((ssa_all_cells_norm$speed)*144*0.02768)^2)/(2*(sqrt((((ssa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((ssa_all_cells_norm$speed)*144*0.02768)/2)^2+30^2))))*180/pi
ssa_all_cells_norm$degrees <- round(ssa_all_cells_norm$degrees, 2)



## add number of sweeps so we can caluclate SEM
ssa_all_cells_norm <- left_join(ssa_all_cells_norm, n_reps, by = c("bird_id", "speed", "direction"))

## calculate SEM
ssa_all_cells_norm <- ssa_all_cells_norm %>%
  mutate(norm_sem = norm_sd/sqrt(n))

#### Write this to csv so it can be read into other scripts
# write_csv(all_cells_norm, "all_cells_norm.csv")
# write_csv(all_cells_intermediate, "all_cells_intermediate.csv")






###############################################
## Get ready for a LOT of inefficient coding ##
##                 CSA vs SSA                ##
##             Speed tuning width            ##
###############################################

# need number of speed bins with firing above each threshold
## sp50, etc... indicates the threshold 

## LM
lm_tuning <- lm_all_cells_norm %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  mutate(max_speed = max(norm_mean), 
         sp50 = 0.50*(max(norm_mean)), 
         sp55 = 0.55*(max(norm_mean)), 
         sp60 = 0.60*(max(norm_mean)), 
         sp65 = 0.65*(max(norm_mean)), 
         sp70 = 0.70*(max(norm_mean)), 
         sp75 = 0.75*(max(norm_mean)), 
         sp80 = 0.80*(max(norm_mean)), 
         sp85 = 0.85*(max(norm_mean)), 
         sp90 = 0.90*(max(norm_mean)), 
         sp95 = 0.95*(max(norm_mean)))


## get list of preferred (tested) directions:
lm_length <- nrow(lm_all_cells_norm)
lm_first_dir <- seq(from = 1, to = lm_length, by = 24) 

lm_test_dir <- lm_all_cells_norm$direction[lm_first_dir]
bird_id <- lm_all_cells_norm$bird_id[lm_first_dir]

lm_direction_df <- tibble(bird_id, lm_test_dir)

## join the tested directions with the all_cells_norm data frame
## so that we can filter by preferred direction response

lm_all_cells_norm_tuning <- left_join(lm_tuning, lm_direction_df, by = "bird_id")

## this gets rid of one of the cells we might want to keep, so take a look at this later
lm_all_cells_norm_tuning <- lm_all_cells_norm_tuning %>% 
  select(-speed, -norm_sd, -n) %>% 
  filter(direction == lm_test_dir)




## CSA
csa_tuning <- csa_all_cells_norm %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  mutate(max_speed = max(norm_mean), 
         sp50 = 0.50*(max(norm_mean)), 
         sp55 = 0.55*(max(norm_mean)), 
         sp60 = 0.60*(max(norm_mean)), 
         sp65 = 0.65*(max(norm_mean)), 
         sp70 = 0.70*(max(norm_mean)), 
         sp75 = 0.75*(max(norm_mean)), 
         sp80 = 0.80*(max(norm_mean)), 
         sp85 = 0.85*(max(norm_mean)), 
         sp90 = 0.90*(max(norm_mean)), 
         sp95 = 0.95*(max(norm_mean)))


## get list of preferred (tested) directions:
csa_length <- nrow(csa_all_cells_norm)
csa_first_dir <- seq(from = 1, to = csa_length, by = 24) 

csa_test_dir <- csa_all_cells_norm$direction[csa_first_dir]
bird_id <- csa_all_cells_norm$bird_id[csa_first_dir]

csa_direction_df <- tibble(bird_id, csa_test_dir)

## join the tested directions with the all_cells_norm data frame
## so that we can filter by preferred direction response

csa_all_cells_norm_tuning <- left_join(csa_tuning, csa_direction_df, by = "bird_id")

## this gets rid of one of the cells we might want to keep, so take a look at this later
csa_all_cells_norm_tuning <- csa_all_cells_norm_tuning %>% 
  select(-speed, -norm_sd, -n) %>% 
  filter(direction == csa_test_dir)


## SSA
ssa_tuning <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  mutate(max_speed = max(norm_mean), 
         sp50 = 0.50*(max(norm_mean)), 
         sp55 = 0.55*(max(norm_mean)), 
         sp60 = 0.60*(max(norm_mean)), 
         sp65 = 0.65*(max(norm_mean)), 
         sp70 = 0.70*(max(norm_mean)), 
         sp75 = 0.75*(max(norm_mean)), 
         sp80 = 0.80*(max(norm_mean)), 
         sp85 = 0.85*(max(norm_mean)), 
         sp90 = 0.90*(max(norm_mean)), 
         sp95 = 0.95*(max(norm_mean)))


## get list of preferred (tested) directions:
ssa_length <- nrow(ssa_all_cells_norm)
ssa_first_dir <- seq(from = 1, to = ssa_length, by = 24) 

ssa_test_dir <- ssa_all_cells_norm$direction[ssa_first_dir]
bird_id <- ssa_all_cells_norm$bird_id[ssa_first_dir]

ssa_direction_df <- tibble(bird_id, ssa_test_dir)

## join the tested directions with the all_cells_norm data frame
## so that we can filter by preferred direction response

ssa_all_cells_norm_tuning <- left_join(ssa_tuning, ssa_direction_df, by = "bird_id")

## this gets rid of one of the cells we might want to keep, so take a look at this later
ssa_all_cells_norm_tuning <- ssa_all_cells_norm_tuning %>% 
  select(-speed, -norm_sd, -n) %>% 
  filter(direction == ssa_test_dir)


## tuning 50%

## LM
lm_tuning50 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp50)

lm_tuning50 <- lm_tuning50 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp50) %>% 
  rename(n_sp50 = n)%>% 
  select(bird_id, n_sp50)

## CSA
csa_tuning50 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp50)

csa_tuning50 <- csa_tuning50 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp50) %>% 
  rename(n_sp50 = n)%>% 
  select(bird_id, n_sp50)

## SSA
ssa_tuning50 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp50)

ssa_tuning50 <- ssa_tuning50 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp50) %>% 
  rename(n_sp50 = n)%>% 
  select(bird_id, n_sp50)


## tuning 55%

## LM
lm_tuning55 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp55)

lm_tuning55 <- lm_tuning55 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp55) %>% 
  rename(n_sp55 = n)%>% 
  select(bird_id, n_sp55)

## CSA
csa_tuning55 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp55)

csa_tuning55 <- csa_tuning55 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp55) %>% 
  rename(n_sp55 = n)%>% 
  select(bird_id, n_sp55)

## SSA
ssa_tuning55 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp55)

ssa_tuning55 <- ssa_tuning55 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp55) %>% 
  rename(n_sp55 = n)%>% 
  select(bird_id, n_sp55)


## tuning width at 60%
## LM
lm_tuning60 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp60)

lm_tuning60 <- lm_tuning60 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp60) %>% 
  rename(n_sp60 = n) %>% 
  select(bird_id, n_sp60)

## CSA
csa_tuning60 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp60)

csa_tuning60 <- csa_tuning60 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp60) %>% 
  rename(n_sp60 = n) %>% 
  select(bird_id, n_sp60)

## SSA
ssa_tuning60 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp60)

ssa_tuning60 <- ssa_tuning60 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp60) %>% 
  rename(n_sp60 = n) %>% 
  select(bird_id, n_sp60)

## tuning width at 65%
## LM
lm_tuning65 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp65)

lm_tuning65 <- lm_tuning65 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp65) %>% 
  rename(n_sp65 = n) %>% 
  select(bird_id, n_sp65)

## CSA
csa_tuning65 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp65)

csa_tuning65 <- csa_tuning65 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp65) %>% 
  rename(n_sp65 = n) %>% 
  select(bird_id, n_sp65)

## SSA
ssa_tuning65 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp65)

ssa_tuning65 <- ssa_tuning65 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp65) %>% 
  rename(n_sp65 = n) %>% 
  select(bird_id, n_sp65)

## tuning width at 70%
##LM
lm_tuning70 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp70)

lm_tuning70 <- lm_tuning70 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp70) %>% 
  rename(n_sp70 = n) %>% 
  select(bird_id, n_sp70)

##CSA
csa_tuning70 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp70)

csa_tuning70 <- csa_tuning70 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp70) %>% 
  rename(n_sp70 = n) %>% 
  select(bird_id, n_sp70)

##SSA
ssa_tuning70 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp70)

ssa_tuning70 <- ssa_tuning70 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp70) %>% 
  rename(n_sp70 = n) %>% 
  select(bird_id, n_sp70)

## tuning width at 75%
## LM
lm_tuning75 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp75)

lm_tuning75 <- lm_tuning75 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp75) %>% 
  rename(n_sp75 = n) %>% 
  select(bird_id, n_sp75)

## CSA
csa_tuning75 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp75)

csa_tuning75 <- csa_tuning75 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp75) %>% 
  rename(n_sp75 = n) %>% 
  select(bird_id, n_sp75)

## SSA
ssa_tuning75 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp75)

ssa_tuning75 <- ssa_tuning75 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp75) %>% 
  rename(n_sp75 = n) %>% 
  select(bird_id, n_sp75)

## tuning width at 80%
## LM
lm_tuning80 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp80)

lm_tuning80 <- lm_tuning80 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp80) %>% 
  rename(n_sp80 = n) %>% 
  select(bird_id, n_sp80)

## CSA
csa_tuning80 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp80)

csa_tuning80 <- csa_tuning80 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp80) %>% 
  rename(n_sp80 = n) %>% 
  select(bird_id, n_sp80)

## SSA
ssa_tuning80 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp80)

ssa_tuning80 <- ssa_tuning80 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp80) %>% 
  rename(n_sp80 = n) %>% 
  select(bird_id, n_sp80)

## tuning width at 85%
## LM
lm_tuning85 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp85)

lm_tuning85 <- lm_tuning85 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp85) %>% 
  rename(n_sp85 = n) %>% 
  select(bird_id, n_sp85)

## CSA
csa_tuning85 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp85)

csa_tuning85 <- csa_tuning85 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp85) %>% 
  rename(n_sp85 = n) %>% 
  select(bird_id, n_sp85)

## SSA
ssa_tuning85 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp85)

ssa_tuning85 <- ssa_tuning85 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp85) %>% 
  rename(n_sp85 = n) %>% 
  select(bird_id, n_sp85)

## tuning width at 90%
## LM
lm_tuning90 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp90)

lm_tuning90 <- lm_tuning90 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp90) %>% 
  rename(n_sp90 = n) %>% 
  select(bird_id, n_sp90)

## CSA
csa_tuning90 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp90)

csa_tuning90 <- csa_tuning90 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp90) %>% 
  rename(n_sp90 = n) %>% 
  select(bird_id, n_sp90)

## SSA
ssa_tuning90 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp90)

ssa_tuning90 <- ssa_tuning90 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp90) %>% 
  rename(n_sp90 = n) %>% 
  select(bird_id, n_sp90)

## tuning width at 95%
## LM
lm_tuning95 <- lm_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp95)

lm_tuning95 <- lm_tuning95 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp95) %>% 
  rename(n_sp95 = n) %>% 
  select(bird_id, n_sp95)

## CSA
csa_tuning95 <- csa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp95)

csa_tuning95 <- csa_tuning95 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp95) %>% 
  rename(n_sp95 = n) %>% 
  select(bird_id, n_sp95)

## SSA
ssa_tuning95 <- ssa_all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp95)

ssa_tuning95 <- ssa_tuning95 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp95) %>% 
  rename(n_sp95 = n) %>% 
  select(bird_id, n_sp95)

## join everything together in the ugliest way possible
## LM
lm_tuning_width_graph <- full_join(lm_tuning50, lm_tuning55, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning60, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning65, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning70, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning75, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning80, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning85, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning90, by = "bird_id")
lm_tuning_width_graph <- full_join(lm_tuning_width_graph, lm_tuning95, by = "bird_id")


lm_tuning_width_graph_reshape <- lm_tuning_width_graph %>% 
  gather(key = "threshold", value = "tuning_width", n_sp50, n_sp55, 
         n_sp60, n_sp65, n_sp70, n_sp75, n_sp80, n_sp85, n_sp90, n_sp95)

## CSA
csa_tuning_width_graph <- full_join(csa_tuning50, csa_tuning55, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning60, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning65, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning70, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning75, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning80, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning85, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning90, by = "bird_id")
csa_tuning_width_graph <- full_join(csa_tuning_width_graph, csa_tuning95, by = "bird_id")


csa_tuning_width_graph_reshape <- csa_tuning_width_graph %>% 
  gather(key = "threshold", value = "tuning_width", n_sp50, n_sp55, 
         n_sp60, n_sp65, n_sp70, n_sp75, n_sp80, n_sp85, n_sp90, n_sp95)

## SSA
ssa_tuning_width_graph <- full_join(ssa_tuning50, ssa_tuning55, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning60, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning65, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning70, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning75, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning80, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning85, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning90, by = "bird_id")
ssa_tuning_width_graph <- full_join(ssa_tuning_width_graph, ssa_tuning95, by = "bird_id")


ssa_tuning_width_graph_reshape <- ssa_tuning_width_graph %>% 
  gather(key = "threshold", value = "tuning_width", n_sp50, n_sp55, 
         n_sp60, n_sp65, n_sp70, n_sp75, n_sp80, n_sp85, n_sp90, n_sp95)

# "#E66B5D", "#FCC229"

#orange and yellow
# "#F79441", "#EAE72B"

## now graph it!

## CSA ONLY tuing width 
q1 <- ggplot(csa_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width)) +
  geom_boxplot(outlier.size = 0, fill = "#F79441") +
  geom_jitter(position = position_jitter(width = 0.5, height = 0.5), alpha = 1/2, size = 4, color = "#E66B5D") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, fill = "#E66B5D") +
  theme_minimal() +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold")

q1
## ggsave("csa_tuning_width.pdf", plot = q1, width = 11, height = 8.5, units = c("in"))

## SSA ONLY tuing width 
q2 <- ggplot(ssa_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width)) +
  geom_boxplot(outlier.size = 0, fill = "#EAE72B") +
  geom_jitter(position = position_jitter(width = 0.5, height = 0.5), alpha = 1/2, size = 4, color = "#FCC229") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, fill = "#FCC229") +
  theme_minimal() +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold")

q2
## ggsave("ssa_tuning_width.pdf", plot = q2, width = 11, height = 8.5, units = c("in"))

## Plot CSA vs SSA
csa_tuning_width_graph_reshape <- csa_tuning_width_graph_reshape %>% 
  mutate(activity_type = "csa")

ssa_tuning_width_graph_reshape <- ssa_tuning_width_graph_reshape %>% 
  mutate(activity_type = "ssa")


combined_tuning_width_graph_reshape <- bind_rows(csa_tuning_width_graph_reshape, ssa_tuning_width_graph_reshape)
combined_tuning_width_graph_reshape$activity_type <- as.factor(combined_tuning_width_graph_reshape$activity_type)

q3 <- ggplot(combined_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width, fill = activity_type)) +
  geom_boxplot(outlier.size = 0, position = "dodge") + #, color = "black"
  scale_fill_manual(values = alpha(c("#F79441", "#EAE72B"))) +
  geom_point(aes(color = activity_type), position = position_jitterdodge(jitter.height = 0.3), alpha = 1/1.5, size = 4) +
  scale_color_manual(values = alpha(c("#E66B5D", "#FCC229"))) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +
  ylim(0,10) +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold") +
  theme_classic()

q3
## ggsave("csa_vs_ssa_tuning_width.pdf", plot = q3, width = 11, height = 8.5, units = c("in"))

## LM ONLY tuing width 
q4 <- ggplot(lm_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width)) +
  geom_boxplot(outlier.size = 0, fill = "#4E2F91") +
  geom_jitter(position = position_jitter(width = 0.5, height = 0.5), alpha = 1/3, size = 4, color = "#A12E91") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, fill = "#A12E91") +
  theme_minimal() +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold")

q4

## ggsave("lm_tuning_width.pdf", plot = q4, width = 11, height = 8.5, units = c("in"))

csa_tuning_width_graph_reshape <- csa_tuning_width_graph_reshape %>% 
  mutate(activity_type = "csa")

ssa_tuning_width_graph_reshape <- ssa_tuning_width_graph_reshape %>% 
  mutate(activity_type = "ssa")

lm_tuning_width_graph_reshape <- lm_tuning_width_graph_reshape %>% 
  mutate(activity_type = "lm")


combined_tuning_width_graph_reshape <- bind_rows(csa_tuning_width_graph_reshape, ssa_tuning_width_graph_reshape, lm_tuning_width_graph_reshape)
combined_tuning_width_graph_reshape$activity_type <- as.factor(combined_tuning_width_graph_reshape$activity_type)


q5 <- ggplot(combined_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width, fill = activity_type)) +
  geom_boxplot(outlier.size = 0, position = "dodge") + #, color = "black"
  scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91", "#F79441"))) +
  geom_point(aes(color = activity_type), position = position_jitterdodge(jitter.height = 0.3), alpha = 1/2) +
  scale_color_manual(values = alpha(c("#FCC229", "#4E2F91", "#E66B5D"))) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +
  ylim(0,10) +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold") +
  theme_classic()

q5

## ggsave("csa_ssa_lm_tuning_width3.pdf", plot = q5, width = 11, height = 8.5, units = c("in"))

