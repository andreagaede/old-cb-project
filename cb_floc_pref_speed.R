## find preferred speed

library(tidyverse)
library(readr)
library(stringr)

dir <- "~/Documents/Projects/Flocculus-recording/2016 - CB speed working data"
setwd(dir)

## This function pulls numbers out of the file name to give each cell an ID
extract_num_from_string <- function(s) {
  s.split <- strsplit(s, "cell")
  s.id <- as.numeric(unlist(strsplit(s.split[[1]][1], "[^[:digit:]]")))
  s.id <- s.id[!is.na(s.id)][1:3]
  
  s.cell <- as.numeric(unlist(strsplit(s.split[[1]][2], "[^[:digit:]]")))
  s.cell <- s.cell[!is.na(s.cell)][1]
  return(c(s.id, s.cell))
}


## Creating data frame that I need for a single cell. Then I need to figure out how to automate it so
## that it goes through all files in the directory -- use purrr? map()?
list.files()[1]
cell <- read_csv(list.files()[1], col_names = FALSE)

name <- list.files()[1]
cell_id <- extract_num_from_string(name)

cell_01 <- cell %>% 
  rename(direction = X1, speed = X2)

bins <- cell_01 %>% 
  select(-direction, -speed) %>% 
  rowMeans()

## Gives you single spike rate for each 5 second sweep
cell_01_intermediate <- cell_01 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins)

n_reps <- cell_01_intermediate %>% 
  group_by(speed, direction) %>%
  summarize(n = n())

mean_baseline <- cell_01_intermediate %>% 
  group_by(speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  filter(speed == "NaN")

mean_baseline <- as.numeric(mean_baseline[3])

find_max_sweep <- cell_01_intermediate %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_baseline)) %>% 
  summarize(max_r = max(norm_spike_r)) %>% 
  as.numeric()

## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
cell_01_mean <- cell_01 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins) %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_baseline)/find_max_sweep)) %>% 
  group_by(speed, direction) %>%
  summarize_each(funs(mean, sd)) %>% 
  round(4)


## get speed in degrees/s instead of pixels/frame
cell_01_mean$degrees <- acos(((sqrt((((cell_01_mean$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((cell_01_mean$speed)*144*0.02768)/2)^2+30^2))^2-((cell_01_mean$speed)*144*0.02768)^2)/(2*(sqrt((((cell_01_mean$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((cell_01_mean$speed)*144*0.02768)/2)^2+30^2))))*180/pi
cell_01_mean$degrees <- round(cell_01_mean$degrees, 2)

## add number of sweeps so we can caluclate SEM
cell_01_mean <- full_join(cell_01_mean, n_reps, by = c("speed", "direction"))

## calculate SEM
cell_01_mean <- cell_01_mean %>% 
  mutate(norm_sem = norm_spike_r_sd/sqrt(n))


extended_cell_01_mean <- cell_01_mean %>% 
  mutate(bird_id = cell_id[1], track = cell_id[2], site = cell_id[3], cell = cell_id[4]) %>% 
  unite(bird_id, c(bird_id, track, site, cell)) %>% 
  ungroup()

extended_cell_01_norm <- extended_cell_01_mean %>% 
  select(bird_id, degrees, direction, norm_spike_r_mean, norm_sem) %>% 
  filter(!is.na(degrees))
  
## THIS IS JUAT A TEST
## Make a test data frame with 2 cells so that I can try to find and plot pref speed

list.files()[2]
cell <- read_csv(list.files()[2], col_names = FALSE)

name <- list.files()[2]
cell_id <- extract_num_from_string(name)

cell_02 <- cell %>% 
  rename(direction = X1, speed = X2)

bins <- cell_02 %>% 
  select(-direction, -speed) %>% 
  rowMeans()

## Gives you single spike rate for each 5 second sweep
cell_02_intermediate <- cell_02 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins)

n_reps <- cell_02_intermediate %>% 
  group_by(speed, direction) %>%
  summarize(n = n())

mean_baseline <- cell_02_intermediate %>% 
  group_by(speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  filter(speed == "NaN")

mean_baseline <- as.numeric(mean_baseline[3])

find_max_sweep <- cell_02_intermediate %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_baseline)) %>% 
  summarize(max_r = max(norm_spike_r)) %>% 
  as.numeric()

## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
cell_02_mean <- cell_02 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins) %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_baseline)/find_max_sweep)) %>% 
  group_by(speed, direction) %>%
  summarize_each(funs(mean, sd)) %>% 
  round(4)


## get speed in degrees/s instead of pixels/frame
cell_02_mean$degrees <- acos(((sqrt((((cell_02_mean$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((cell_02_mean$speed)*144*0.02768)/2)^2+30^2))^2-((cell_02_mean$speed)*144*0.02768)^2)/(2*(sqrt((((cell_02_mean$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((cell_02_mean$speed)*144*0.02768)/2)^2+30^2))))*180/pi
cell_02_mean$degrees <- round(cell_02_mean$degrees, 2)

## add number of sweeps so we can caluclate SEM
cell_02_mean <- full_join(cell_02_mean, n_reps, by = c("speed", "direction"))

## calculate SEM
cell_02_mean <- cell_02_mean %>% 
  mutate(norm_sem = norm_spike_r_sd/sqrt(n))


extended_cell_02_mean <- cell_02_mean %>% 
  mutate(bird_id = cell_id[1], track = cell_id[2], site = cell_id[3], cell = cell_id[4]) %>% 
  unite(bird_id, c(bird_id, track, site, cell)) %>% 
  ungroup()

extended_cell_02_norm <- extended_cell_02_mean %>% 
  select(bird_id, degrees, direction, norm_spike_r_mean, norm_sem) %>% 
  filter(!is.na(degrees))


list.files()[3]
cell <- read_csv(list.files()[3], col_names = FALSE)

name <- list.files()[3]
cell_id <- extract_num_from_string(name)

cell_03 <- cell %>% 
  rename(direction = X1, speed = X2)

bins <- cell_03 %>% 
  select(-direction, -speed) %>% 
  rowMeans()

## Gives you single spike rate for each 5 second sweep
cell_03_intermediate <- cell_03 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins)

n_reps <- cell_03_intermediate %>% 
  group_by(speed, direction) %>%
  summarize(n = n())

mean_baseline <- cell_03_intermediate %>% 
  group_by(speed, direction) %>%
  summarise_each(funs(mean,sd)) %>% 
  filter(speed == "NaN")

mean_baseline <- as.numeric(mean_baseline[3])

find_max_sweep <- cell_03_intermediate %>% 
  mutate(norm_spike_r = (mean_spike_r - mean_baseline)) %>% 
  summarize(max_r = max(norm_spike_r)) %>% 
  as.numeric()

## Gives you a mean spike rate of 5 sweeps for given combination of direction and speed. Also gives SD
## Need to caluclate SEM
cell_03_mean <- cell_03 %>% 
  select(direction, speed) %>% 
  mutate(mean_spike_r = bins) %>% 
  mutate(norm_spike_r = ((mean_spike_r - mean_baseline)/find_max_sweep)) %>% 
  group_by(speed, direction) %>%
  summarize_each(funs(mean, sd)) %>% 
  round(4)


## get speed in degrees/s instead of pixels/frame
cell_03_mean$degrees <- acos(((sqrt((((cell_03_mean$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((cell_03_mean$speed)*144*0.02768)/2)^2+30^2))^2-((cell_03_mean$speed)*144*0.02768)^2)/(2*(sqrt((((cell_03_mean$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((cell_03_mean$speed)*144*0.02768)/2)^2+30^2))))*180/pi
cell_03_mean$degrees <- round(cell_03_mean$degrees, 2)

## add number of sweeps so we can caluclate SEM
cell_03_mean <- full_join(cell_03_mean, n_reps, by = c("speed", "direction"))

## calculate SEM
cell_03_mean <- cell_03_mean %>% 
  mutate(norm_sem = norm_spike_r_sd/sqrt(n))


extended_cell_03_mean <- cell_03_mean %>% 
  mutate(bird_id = cell_id[1], track = cell_id[2], site = cell_id[3], cell = cell_id[4]) %>% 
  unite(bird_id, c(bird_id, track, site, cell)) %>% 
  ungroup()

extended_cell_03_norm <- extended_cell_03_mean %>% 
  select(bird_id, degrees, direction, norm_spike_r_mean, norm_sem) %>% 
  filter(!is.na(degrees))






my_data <- bind_rows(extended_cell_01_norm, extended_cell_02_norm, extended_cell_03_norm)






 ## write_csv(my_data, "my_data_test2.csv")




######################
## Different option

## need to tell it that first row isn't column names
## maybe bring in data, then rename columns, take a mean for all bins in a row, put out as csv

# my_data <- dir() %>% 
#   map(read_csv) %>% 
#   map(colnames)
#            

           

## find the max speed for every cell
my_data_max <- my_data %>% 
  group_by(bird_id) %>% 
  filter(norm_spike_r_mean == max(norm_spike_r_mean))

# might want to find better way of turning speeds into a factor earlier on in script
my_data_max_graph <- my_data_max %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = n)

my_data_max_graph$degrees <- as.factor(my_data_max_graph$degrees)


## graph for max speed
p <- ggplot(my_data_max_graph, aes(x = degrees, y = num_max))
p + geom_bar(stat = "identity", fill = "orange", color = "black") + theme_minimal()



######## playing with count()
bird_id <- c("1601_1_1232_4")
degrees <- c(16.03)
direction <- c(0)
norm_spike_r_mean <- c(0.5332)
norm_sem <- c(0.14593501)
test_vector <- data.frame(bird_id, degrees, direction, norm_spike_r_mean, norm_sem)
my_data_test <- bind_rows(my_data_max, test_vector)

my_data_test <- my_data_test %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = n)

my_data_test$degrees <- as.factor(my_data_test$degrees)

q <- ggplot(my_data_test, aes(x = degrees, y = num_max))
q + geom_bar(stat = "identity", fill = "orange", color = "black") + theme_minimal()

# there's probably a better way to do this, but this is all I can think of right now
# bind_cols(my_data_max, my_data_max1)
# try a join



## find every speed that = 80% of max firing rate for every cell
my_data_max80 <- my_data %>% 
  group_by(bird_id) %>% 
  filter(norm_spike_r_mean >= 0.8*max(norm_spike_r_mean))

my_data_max80_graph <- my_data_max80 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = n)

my_data_max80_graph$degrees <- as.factor(my_data_max80_graph$degrees)

## this isn't what I need, just need to know how many total cells
total_cells <- my_data_max80 %>% 
  count(bird_id)

## maybe this:
total_cells1 <- my_data_max80 %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()
# I feel like I should be able to use n() or n_distinct() here, just need to play with it more
  


## graph for 80% max




## Speed tuning width

















