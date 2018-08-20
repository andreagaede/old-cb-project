
## set working directory to the directory holding the folder we want to look inside
setwd("~/Documents/Projects/Flocculus-recording")
## setwd("~/Documents/Projects/Flocculus-recording/2016 - CB speed working data")


library(purrr)
library(tidyverse)
library(readr)
library(stringr)

## Come back to this...Jenny had some suggestions for manipulating file names
# cell_files1 <- set_names(list.files(cell_directory, full.names = TRUE), 
#                          str_extract(list.files(cell_directory, full.names = TRUE),
#                                      "/\d/g"))

## trying different method
cell_directory <- "2016 - CB speed working data"

cell_files <- list.files(cell_directory, full.names = TRUE)

names(cell_files) <- list.files(cell_directory) %>% 
  gsub(pattern = ".csv$", replacement = "")

## Alternative method I'd like to figure out later:
## set_names(map(list.files(cell_directory), list.files(cell_directory, full.names = TRUE)), 
## gsub(list.files(cell_directory), pattern = ".csv$", replacement = ""))


## add IDs and make giant df
all_cells <- map_df(cell_files, read_csv, col_names = FALSE,
                .id = "bird_id")
## Name columns
all_cells <- set_names(all_cells, nm = c("bird_id", "direction", "speed", paste("bin", as.character(1:20), sep = "")))

bin_mean <- all_cells %>% 
  select(-bird_id, -direction, -speed) %>% 
  rowMeans()

## fix untidy directions
all_cells$direction <- round(all_cells$direction)

## Gives you single spike rate for each 5 second sweep
all_cells_intermediate <- all_cells %>% 
  select(bird_id, direction, speed) %>% 
  mutate(mean_spike_r = bin_mean)

n_reps <- all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarize(n = n())

mean_baseline <- all_cells_intermediate %>% 
  group_by(bird_id, speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  rename(mean_base = mean, sd_base = sd) %>% 
  filter(speed == "NaN") %>% 
  ungroup() %>% 
  select(bird_id, mean_base, sd_base)

all_cells_intermediate <- left_join(all_cells_intermediate, mean_baseline, by = "bird_id")


find_max_sweep <- all_cells_intermediate %>% 
  group_by(bird_id) %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_base)) %>% 
  summarize(max_r = max(norm_spike_r)) 

all_cells_intermediate <- left_join(all_cells_intermediate, find_max_sweep, by = "bird_id")


## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
all_cells_norm <- all_cells_intermediate %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_base)/max_r)) %>% 
  select(bird_id, speed, direction, norm_spike_r) %>% 
  group_by(bird_id, speed, direction) %>%
  filter(!is.na(speed)) %>% 
  summarize_each(funs(mean, sd)) %>% 
  rename(norm_mean = mean, norm_sd = sd)


## get speed in degrees/s instead of pixels/frame
all_cells_norm$degrees <- acos(((sqrt((((all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((all_cells_norm$speed)*144*0.02768)/2)^2+30^2))^2-((all_cells_norm$speed)*144*0.02768)^2)/(2*(sqrt((((all_cells_norm$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((all_cells_norm$speed)*144*0.02768)/2)^2+30^2))))*180/pi
all_cells_norm$degrees <- round(all_cells_norm$degrees, 2)



## add number of sweeps so we can caluclate SEM
all_cells_norm <- left_join(all_cells_norm, n_reps, by = c("bird_id", "speed", "direction"))

## calculate SEM
all_cells_norm <- all_cells_norm %>%
  mutate(norm_sem = norm_sd/sqrt(n))

#### Write this to csv so it can be read into other scripts
# write_csv(all_cells_norm, "all_cells_norm.csv")
# write_csv(all_cells_intermediate, "all_cells_intermediate.csv")

## find the max speed for every cell
my_data_max <- all_cells_norm %>% 
  group_by(bird_id) %>% 
  filter(norm_mean == max(norm_mean))


# might want to find better way of turning speeds into a factor earlier on in script
my_data_max_graph <- my_data_max %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = nn)

num_cells <- sum(my_data_max_graph$num_max)

my_data_max_graph <- my_data_max_graph %>% 
  mutate(proportion_pop = num_max/num_cells)

all_speeds <- all_cells_norm %>% 
  ungroup() %>% 
  select(degrees) %>% 
  distinct()

my_data_max_graph_na <- full_join(all_speeds, my_data_max_graph)

## my_data_max_graph$degrees <- as.factor(my_data_max_graph$degrees)

my_data_max_graph_na$degrees <- as.factor(my_data_max_graph_na$degrees)


## graph for max speed
p1 <- ggplot(my_data_max_graph_na, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "dark orange", color = "black") + theme_minimal() +
  ggtitle("Proportion of population with max firing at each stimulus speed")
p1
## ggsave("max_speed.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))


## find every speed that = 80% of max firing rate for every cell
my_data_max80 <- all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.8*max(norm_mean))

## find total number of cells recorded
total_num_cells <- all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 80% max response at each speed bin
my_data_max80_graph <- my_data_max80 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max80 = n)%>% 
  mutate(proportion_pop = num_max80/total_num_cells)

my_data_max80_graph <- full_join(all_speeds, my_data_max80_graph)

my_data_max80_graph$degrees <- as.factor(my_data_max80_graph$degrees)

## graph for 80% max
p2 <- ggplot(my_data_max80_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "dark orange", color = "black") + theme_minimal() +
  ggtitle("Proportion of population with >= 80% max firing at each stimulus speed")

p2
## ggsave("max80_speed.pdf", plot = p2, width = 11, height = 8.5, units = c("in"))




## find every speed that = 70% of max firing rate for every cell
my_data_max70 <- all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.7*max(norm_mean))

## find total number of cells recorded
total_num_cells <- all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 70% max response at each speed bin
my_data_max70_graph <- my_data_max70 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max70 = n)%>% 
  mutate(proportion_pop = num_max70/total_num_cells)

my_data_max70_graph <- full_join(all_speeds, my_data_max70_graph)

my_data_max70_graph$degrees <- as.factor(my_data_max70_graph$degrees)



## graph for 70% max
p3 <- ggplot(my_data_max70_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "dark orange", color = "black") + theme_minimal() +
  ggtitle("Proportion of population with >= 70% max firing at each stimulus speed")

p3
## ggsave("max70_speed.pdf", plot = p3, width = 11, height = 8.5, units = c("in"))





## find every speed that = 90% of max firing rate for every cell
my_data_max90 <- all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.9*max(norm_mean))

## find total number of cells recorded
total_num_cells <- all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 90% max response at each speed bin
my_data_max90_graph <- my_data_max90 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max90 = n)%>% 
  mutate(proportion_pop = num_max90/total_num_cells)

my_data_max90_graph <- full_join(all_speeds, my_data_max90_graph)

my_data_max90_graph$degrees <- as.factor(my_data_max90_graph$degrees)



## graph for 90% max
p4 <- ggplot(my_data_max90_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "dark orange", color = "black") + theme_minimal() +
  ggtitle("Proportion of population with >= 90% max firing at each stimulus speed")

p4
## ggsave("max90_speed.pdf", plot = p4, width = 11, height = 8.5, units = c("in"))


## Speed tuning width

# need number of speed bins with firing above each threshold
## sp50, etc... indicates the threshold 
tuning <- all_cells_norm %>% 
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
length <- nrow(all_cells_norm)
first_dir <- seq(from = 1, to = length, by = 24) 

test_dir <- all_cells_norm$direction[first_dir]
bird_id <- all_cells_norm$bird_id[first_dir]

direction_df <- tibble(bird_id, test_dir)

## join the tested directions with the all_cells_norm data frame
## so that we can filter by preferred direction response

all_cells_norm_tuning <- left_join(tuning, direction_df, by = "bird_id")

## this gets rid of one of the cells we might want to keep, so take a look at this later
all_cells_norm_tuning <- all_cells_norm_tuning %>% 
  select(-speed, -norm_sd, -n) %>% 
  filter(direction == test_dir)

## head(all_cells_norm_tuning)

## tuning 50%
tuning50 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp50)

tuning50 <- tuning50 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp50) %>% 
  rename(n_sp50 = n)%>% 
  select(bird_id, n_sp50)

## tuning 55%
tuning55 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp55)

tuning55 <- tuning55 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp55) %>% 
  rename(n_sp55 = n)%>% 
  select(bird_id, n_sp55)

## tuning width at 60%
tuning60 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp60)

tuning60 <- tuning60 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp60) %>% 
  rename(n_sp60 = n) %>% 
  select(bird_id, n_sp60)

## tuning width at 65%
tuning65 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp65)

tuning65 <- tuning65 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp65) %>% 
  rename(n_sp65 = n) %>% 
  select(bird_id, n_sp65)

## tuning width at 70%
tuning70 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp70)

tuning70 <- tuning70 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp70) %>% 
  rename(n_sp70 = n) %>% 
  select(bird_id, n_sp70)

## tuning width at 75%
tuning75 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp75)

tuning75 <- tuning75 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp75) %>% 
  rename(n_sp75 = n) %>% 
  select(bird_id, n_sp75)

## tuning width at 80%
tuning80 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp80)

tuning80 <- tuning80 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp80) %>% 
  rename(n_sp80 = n) %>% 
  select(bird_id, n_sp80)

## tuning width at 85%
tuning85 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp85)

tuning85 <- tuning85 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp85) %>% 
  rename(n_sp85 = n) %>% 
  select(bird_id, n_sp85)

## tuning width at 90%
tuning90 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp90)

tuning90 <- tuning90 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp90) %>% 
  rename(n_sp90 = n) %>% 
  select(bird_id, n_sp90)

## tuning width at 95%
tuning95 <- all_cells_norm_tuning %>% 
  group_by(bird_id, direction) %>% 
  filter(norm_mean >= sp95)

tuning95 <- tuning95 %>% 
  ungroup() %>% 
  group_by(bird_id) %>% 
  count(sp95) %>% 
  rename(n_sp95 = n) %>% 
  select(bird_id, n_sp95)


## join everything together in the ugliest way possible

tuning_width_graph <- full_join(tuning50, tuning55, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning60, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning65, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning70, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning75, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning80, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning85, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning90, by = "bird_id")
tuning_width_graph <- full_join(tuning_width_graph, tuning95, by = "bird_id")


tuning_width_graph_reshape <- tuning_width_graph %>% 
  gather(key = "threshold", value = "tuning_width", n_sp50, n_sp55, 
         n_sp60, n_sp65, n_sp70, n_sp75, n_sp80, n_sp85, n_sp90, n_sp95)


## now graph it!
q1 <- ggplot(tuning_width_graph_reshape, aes(x = threshold, y = tuning_width)) +
  geom_boxplot(fill = "bisque") +
  geom_jitter(position = position_jitter(width = 0.5, height = 0.5), alpha = 1/2, size = 4, color = "dark orange") +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "dark orange2")

q1

## ggsave("tuning_width.pdf", plot = q1, width = 11, height = 8.5, units = c("in"))


## try re-writing so we normalize to an average maximum firing rate



