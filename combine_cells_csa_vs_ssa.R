## Split CSA and SSA


## set working directory to the directory holding the folder we want to look inside
setwd("~/Documents/Projects/Flocculus-recording")
## setwd("~/Documents/Projects/Flocculus-recording/2016 - CB speed working data")


library(purrr)
library(tidyverse)
library(readr)
library(stringr)

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
  geom_bar(stat = "identity", fill = "#EAE72B") + #  color = "black" gives black outlines
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

## find the max speed for every cell
ssa_my_data_max <- ssa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  filter(norm_mean == max(norm_mean))


# might want to find better way of turning speeds into a factor earlier on in script
ssa_my_data_max_graph <- ssa_my_data_max %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max = nn)

num_cells <- sum(ssa_my_data_max_graph$num_max)

ssa_my_data_max_graph <- ssa_my_data_max_graph %>% 
  mutate(proportion_pop = num_max/num_cells)

ssa_all_speeds <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  select(degrees) %>% 
  distinct()

ssa_my_data_max_graph_na <- full_join(ssa_all_speeds, ssa_my_data_max_graph)
ssa_my_data_max_graph_na$degrees <- as.factor(ssa_my_data_max_graph_na$degrees)

c2 <- ggplot(ssa_my_data_max_graph_na, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#F79441") + # color = "black"
  ggtitle("SSA - Proportion of population with max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")
c2
## ggsave("SSA_max_speed.pdf", plot = c2, width = 11, height = 8.5, units = c("in"))


## plot max speed CSA vs SSA

csa_my_data_max_graph_na <- csa_my_data_max_graph_na %>% 
  mutate(activity_type = "csa") %>% 
  select(degrees, proportion_pop, activity_type)

ssa_my_data_max_graph_na <- ssa_my_data_max_graph_na %>% 
  mutate(activity_type = "ssa") %>% 
  select(degrees, proportion_pop, activity_type)

combined_my_data_max_graph_na <- bind_rows(csa_my_data_max_graph_na, ssa_my_data_max_graph_na)
combined_my_data_max_graph_na$activity_type <- as.factor(combined_my_data_max_graph_na$activity_type)

c3 <- ggplot(combined_my_data_max_graph_na, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("CSA vs SSA Proportion of population with max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c3 <- c3 + scale_fill_manual(values = alpha(c("#EAE72B", "#F79441")))
c3

## ggsave("csa_vs_ssa_max_speed.pdf", plot = c3, width = 11, height = 8.5, units = c("in"))



## Add LM to CSA and SSA
zf_max_speeds <- read_csv("~/Documents/Projects/Flocculus-recording/zf_max_speeds.csv")
zf_max_speeds <- bind_cols(csa_my_data_max_graph_na, zf_max_speeds)
zf_max_speeds <- zf_max_speeds %>% 
  select(degrees,firing_bin) %>% 
  rename(proportion_pop = firing_bin) %>% 
  mutate(activity_type = "lm")
#zf_max_speeds$activity_type <- as.factor(zf_max_speeds$activity_type)

lm_csa_ssa_combined <- bind_rows(zf_max_speeds, csa_my_data_max_graph_na, ssa_my_data_max_graph_na)
lm_csa_ssa_combined$activity_type <- as.factor(lm_csa_ssa_combined$activity_type)

f1 <- ggplot(lm_csa_ssa_combined, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("LM, CSA, SSA Proportion of population with max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

f1 <- f1 + scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91","#F79441")))
f1

## ggsave("lm_csa_ssa_max_speed.pdf", plot = f1, width = 11, height = 8.5, units = c("in"))



##############################
##   80% max CSA activity   ##
##############################

## find every speed that = 80% of max firing rate for every cell
csa_my_data_max80 <- csa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.8*max(norm_mean))

## find total number of cells recorded
csa_total_num_cells <- csa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 80% max response at each speed bin
csa_my_data_max80_graph <- csa_my_data_max80 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max80 = n)%>% 
  mutate(proportion_pop = num_max80/csa_total_num_cells)

csa_my_data_max80_graph <- full_join(csa_all_speeds, csa_my_data_max80_graph)

csa_my_data_max80_graph$degrees <- as.factor(csa_my_data_max80_graph$degrees)

## graph for 80% max - CSA ONLY
c4 <- ggplot(csa_my_data_max80_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#EAE72B") + 
  ggtitle("CSA Proportion of population with >= 80% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c4
## ggsave("csa_max80_speed.pdf", plot = c4, width = 11, height = 8.5, units = c("in"))

##############################
##   80% max SSA activity   ##
##############################

## find every speed that = 80% of max firing rate for every cell
ssa_my_data_max80 <- ssa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.8*max(norm_mean))

## find total number of cells recorded
ssa_total_num_cells <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 80% max response at each speed bin
ssa_my_data_max80_graph <- ssa_my_data_max80 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max80 = n)%>% 
  mutate(proportion_pop = num_max80/ssa_total_num_cells)

ssa_my_data_max80_graph <- full_join(ssa_all_speeds, ssa_my_data_max80_graph)

ssa_my_data_max80_graph$degrees <- as.factor(ssa_my_data_max80_graph$degrees)

## graph for 80% max - ssa ONLY
c5 <- ggplot(ssa_my_data_max80_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#F79441") + 
  ggtitle("ssa Proportion of population with >= 80% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c5
## ggsave("ssa_max80_speed.pdf", plot = c5, width = 11, height = 8.5, units = c("in"))


## plot 80% max speed CSA vs SSA

csa_my_data_max80_graph <- csa_my_data_max80_graph %>% 
  mutate(activity_type = "csa") %>% 
  select(degrees, proportion_pop, activity_type)

ssa_my_data_max80_graph <- ssa_my_data_max80_graph %>% 
  mutate(activity_type = "ssa") %>% 
  select(degrees, proportion_pop, activity_type)

combined_my_data_max80_graph <- bind_rows(csa_my_data_max80_graph, ssa_my_data_max80_graph)
combined_my_data_max80_graph$activity_type <- as.factor(combined_my_data_max80_graph$activity_type)

c6 <- ggplot(combined_my_data_max80_graph, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("CSA vs SSA Proportion of population with 80% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")
  
c6 <- c6 + scale_fill_manual(values = alpha(c("#EAE72B", "#F79441")))
c6

## ggsave("csa_vs_ssa_max80_speed.pdf", plot = c6, width = 11, height = 8.5, units = c("in"))

## Add LM to CSA and SSA
zf_max80_speeds <- read_csv("~/Documents/Projects/Flocculus-recording/zf_max80_speeds.csv")
zf_max80_speeds <- bind_cols(csa_my_data_max80_graph, zf_max80_speeds)
zf_max80_speeds <- zf_max80_speeds %>% 
  select(degrees, zf.speedbins) %>% 
  rename(proportion_pop = zf.speedbins) %>% 
  mutate(activity_type = "lm")
#zf_max_speeds$activity_type <- as.factor(zf_max_speeds$activity_type)

lm_csa_ssa_combined80 <- bind_rows(zf_max80_speeds, csa_my_data_max80_graph, ssa_my_data_max80_graph)
lm_csa_ssa_combined80$activity_type <- as.factor(lm_csa_ssa_combined80$activity_type)

f4 <- ggplot(lm_csa_ssa_combined80, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("LM, CSA, SSA Proportion of population within 80% max firing for each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

f4 <- f4 + scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91","#F79441")))
f4
## ggsave("lm_csa_ssa_max80_speed.pdf", plot = f4, width = 11, height = 8.5, units = c("in"))

##############################
##   70% max CSA activity   ##
##############################

## find every speed that = 70% of max firing rate for every cell
csa_my_data_max70 <- csa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.7*max(norm_mean))

## find total number of cells recorded
csa_total_num_cells <- csa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 70% max response at each speed bin
csa_my_data_max70_graph <- csa_my_data_max70 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max70 = n)%>% 
  mutate(proportion_pop = num_max70/csa_total_num_cells)

csa_my_data_max70_graph <- full_join(csa_all_speeds, csa_my_data_max70_graph)

csa_my_data_max70_graph$degrees <- as.factor(csa_my_data_max70_graph$degrees)



## CSA ONLY graph for 70% max
c7 <- ggplot(csa_my_data_max70_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#EAE72B") + 
  ggtitle("CSA Proportion of population with >= 70% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c7
## ggsave("csa_max70_speed.pdf", plot = c7, width = 11, height = 8.5, units = c("in"))


##############################
##   70% max SSA activity   ##
##############################

## find every speed that = 70% of max firing rate for every cell
ssa_my_data_max70 <- ssa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.7*max(norm_mean))

## find total number of cells recorded
ssa_total_num_cells <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 70% max response at each speed bin
ssa_my_data_max70_graph <- ssa_my_data_max70 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max70 = n)%>% 
  mutate(proportion_pop = num_max70/ssa_total_num_cells)

ssa_my_data_max70_graph <- full_join(ssa_all_speeds, ssa_my_data_max70_graph)

ssa_my_data_max70_graph$degrees <- as.factor(ssa_my_data_max70_graph$degrees)

## SSA ONLY graph for 70% max
c8 <- ggplot(ssa_my_data_max70_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#F79441") + 
  ggtitle("SSA Proportion of population with >= 70% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c8
## ggsave("ssa_max70_speed.pdf", plot = c8, width = 11, height = 8.5, units = c("in"))


## plot 70% max speed CSA vs SSA
csa_my_data_max70_graph <- csa_my_data_max70_graph %>% 
  mutate(activity_type = "csa") %>% 
  select(degrees, proportion_pop, activity_type)

ssa_my_data_max70_graph <- ssa_my_data_max70_graph %>% 
  mutate(activity_type = "ssa") %>% 
  select(degrees, proportion_pop, activity_type)

combined_my_data_max70_graph <- bind_rows(csa_my_data_max70_graph, ssa_my_data_max70_graph)
combined_my_data_max70_graph$activity_type <- as.factor(combined_my_data_max70_graph$activity_type)

c9 <- ggplot(combined_my_data_max70_graph, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("CSA vs SSA Proportion of population with 70% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c9 <- c9 + scale_fill_manual(values = alpha(c("#EAE72B", "#F79441")))
c9

## ggsave("csa_vs_ssa_max70_speed.pdf", plot = c9, width = 11, height = 8.5, units = c("in"))


## Add LM to CSA and SSA
zf_max70_speeds <- read_csv("~/Documents/Projects/Flocculus-recording/zf_max70_speeds.csv")
zf_max70_speeds <- bind_cols(csa_my_data_max70_graph, zf_max70_speeds)
zf_max70_speeds <- zf_max70_speeds %>% 
  select(degrees, zf.speedbins) %>% 
  rename(proportion_pop = zf.speedbins) %>% 
  mutate(activity_type = "lm")
#zf_max_speeds$activity_type <- as.factor(zf_max_speeds$activity_type)

lm_csa_ssa_combined70 <- bind_rows(zf_max70_speeds, csa_my_data_max70_graph, ssa_my_data_max70_graph)
lm_csa_ssa_combined70$activity_type <- as.factor(lm_csa_ssa_combined70$activity_type)

f2 <- ggplot(lm_csa_ssa_combined70, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("LM, CSA, SSA Proportion of population within 70% max firing for each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

f2 <- f2 + scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91","#F79441")))
f2
## ggsave("lm_csa_ssa_max70_speed.pdf", plot = f2, width = 11, height = 8.5, units = c("in"))


##############################
##   90% max CSA activity   ##
##############################

## find every speed that = 90% of max firing rate for every cell
csa_my_data_max90 <- csa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.9*max(norm_mean))

## find total number of cells recorded
csa_total_num_cells <- csa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 90% max response at each speed bin
csa_my_data_max90_graph <- csa_my_data_max90 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max90 = n)%>% 
  mutate(proportion_pop = num_max90/csa_total_num_cells)

csa_my_data_max90_graph <- full_join(csa_all_speeds, csa_my_data_max90_graph)

csa_my_data_max90_graph$degrees <- as.factor(csa_my_data_max90_graph$degrees)



## CSA ONLY graph for 90% max
c10 <- ggplot(csa_my_data_max90_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#EAE72B") + 
  ggtitle("CSA Proportion of population with >= 90% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c10
## ggsave("csa_max90_speed.pdf", plot = c10, width = 11, height = 8.5, units = c("in"))

##############################
##   90% max SSA activity   ##
##############################

## find every speed that = 90% of max firing rate for every cell
ssa_my_data_max90 <- ssa_all_cells_norm %>% 
  group_by(bird_id) %>% 
  select(-n, -norm_sd, -speed) %>% 
  filter(norm_mean >= 0.9*max(norm_mean))

## find total number of cells recorded
ssa_total_num_cells <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  select(bird_id) %>% 
  distinct() %>% 
  nrow()

## find proportion of population that fires at 90% max response at each speed bin
ssa_my_data_max90_graph <- ssa_my_data_max90 %>% 
  ungroup() %>% 
  count(degrees) %>% 
  rename(num_max90 = n)%>% 
  mutate(proportion_pop = num_max90/ssa_total_num_cells)

ssa_my_data_max90_graph <- full_join(ssa_all_speeds, ssa_my_data_max90_graph)

ssa_my_data_max90_graph$degrees <- as.factor(ssa_my_data_max90_graph$degrees)

## SSA ONLY graph for 90% max
c11 <- ggplot(ssa_my_data_max90_graph, aes(x = degrees, y = proportion_pop)) + 
  geom_bar(stat = "identity", fill = "#F79441") + 
  ggtitle("SSA Proportion of population with >= 90% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c11
## ggsave("ssa_max90_speed.pdf", plot = c11, width = 11, height = 8.5, units = c("in"))

###################################
## plot 90% max speed CSA vs SSA ##
###################################
csa_my_data_max90_graph <- csa_my_data_max90_graph %>% 
  mutate(activity_type = "csa") %>% 
  select(degrees, proportion_pop, activity_type)

ssa_my_data_max90_graph <- ssa_my_data_max90_graph %>% 
  mutate(activity_type = "ssa") %>% 
  select(degrees, proportion_pop, activity_type)

combined_my_data_max90_graph <- bind_rows(csa_my_data_max90_graph, ssa_my_data_max90_graph)
combined_my_data_max90_graph$activity_type <- as.factor(combined_my_data_max90_graph$activity_type)

c12 <- ggplot(combined_my_data_max90_graph, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("CSA vs SSA Proportion of population with 90% max firing at each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

c12 <- c12 + scale_fill_manual(values = alpha(c("#EAE72B", "#F79441")))
c12

## ggsave("csa_vs_ssa_max90_speed.pdf", plot = c12, width = 11, height = 8.5, units = c("in"))


## Add LM to CSA and SSA
zf_max90_speeds <- read_csv("~/Documents/Projects/Flocculus-recording/zf_max90_speeds.csv")
zf_max90_speeds <- bind_cols(csa_my_data_max90_graph, zf_max90_speeds)
zf_max90_speeds <- zf_max90_speeds %>% 
  select(degrees, zf.speedbins) %>% 
  rename(proportion_pop = zf.speedbins) %>% 
  mutate(activity_type = "lm")
#zf_max_speeds$activity_type <- as.factor(zf_max_speeds$activity_type)

lm_csa_ssa_combined90 <- bind_rows(zf_max90_speeds, csa_my_data_max90_graph, ssa_my_data_max90_graph)
lm_csa_ssa_combined90$activity_type <- as.factor(lm_csa_ssa_combined90$activity_type)

f3 <- ggplot(lm_csa_ssa_combined90, aes(x = degrees, y = proportion_pop, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("LM, CSA, SSA Proportion of population within 90% max firing for each stimulus speed") +
  theme_minimal() +
  xlab("Speed bins (degrees/s)") +
  ylab("Proportion of population")

f3 <- f3 + scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91","#F79441")))
f3
## ggsave("lm_csa_ssa_max90_speed.pdf", plot = f3, width = 11, height = 8.5, units = c("in"))

###############################################
## Get ready for a LOT of inefficient coding ##
##                 CSA vs SSA                ##
##             Speed tuning width            ##
###############################################

# need number of speed bins with firing above each threshold
## sp50, etc... indicates the threshold 

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


q4 <- ggplot(combined_tuning_width_graph_reshape, aes(x = threshold, y = tuning_width, fill = activity_type)) +
  geom_boxplot(outlier.size = 0, position = "dodge") + #, color = "black"
  scale_fill_manual(values = alpha(c("#F79441", "#EAE72B"))) +
  geom_point(aes(color = activity_type), position = position_jitterdodge(jitter.height = 0.3), alpha = 1/1.5, size = 4) +
  scale_color_manual(values = alpha(c("#E66B5D", "#FCC229"))) +
  #stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4) +
  xlab("Percent of maximum firing rate (threshold)") + 
  ylab("Number of speed bins above threshold")

q4

## ggsave("csa_vs_ssa_tuning_width2.pdf", plot = q4, width = 11, height = 8.5, units = c("in"))


#############################################
##    AUC calculation and Quadrant plot    ##
#############################################
library(purrr)

find_trap <- function(speed, firing){
  trapezoids <- matrix(0,length(speed)-1,1)
  for(i in 2:length(speed)) {
    width <- speed[i]-speed[i-1]  
    if(firing[i]>firing[i-1]) {
      tri <- 0.5*width*(firing[i]-firing[i-1])
      rect <- width*firing[i-1]
      area <- tri+rect
    }
    else {
      tri <- 0.5*width*(firing[i-1]-firing[i])
      rect <- width*firing[i]
      area <- tri+rect
    }
    trapezoids[i-1] <- area
  }
  return(trapezoids)
}


################
##     CSA    ##
################

csa_quad <- csa_all_cells_norm %>% 
  ungroup() %>% 
  group_by(bird_id, direction) %>% 
  select(bird_id, direction, degrees, norm_mean)
#%>%  mutate(trap_norm_mean = norm_mean-norm_mean[degrees == u_degrees[1]])

dir1_position <- seq(1, nrow(csa_quad), by = 2)
dir2_position <- seq(2, nrow(csa_quad), by = 2)

csa_quad_dir1 <- csa_quad[dir1_position,]
csa_quad_dir2 <- csa_quad[dir2_position,]


jump <- seq(1,nrow(csa_quad_dir1) + 12, by = 12)
trapezoid_areas_dir1 <- matrix(0,length(jump)-1,1)

for(j in 2:length(jump)-1){
  firing_one_cell <- c(0, csa_quad_dir1$norm_mean[jump[j]:jump[j+1]-1])
  firing_one_cell <- data_frame(firing_one_cell)
  speed <- c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86)
  speed <- data_frame(speed)
  trapezoid_areas_dir1[j] <- map2(speed, firing_one_cell, find_trap)
}
  

trapezoid_areas_dir1
#norm_areas <- trapezoid_areas[[1]]*max(steps)/steps

step_fun <- function(x){
  steps <- diff(c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86))
  sum(x * max(steps)/steps)
}


csa_norm_trapezoid_areas_dir1 <- map(trapezoid_areas_dir1[], step_fun)


### areas under anti-preferred direction curves (CSA)

jump <- seq(1,nrow(csa_quad_dir2) + 12, by = 12)
trapezoid_areas_dir2 <- matrix(0,length(jump)-1,1)

for(j in 2:length(jump)-1){
  firing_one_cell <- c(0, csa_quad_dir2$norm_mean[jump[j]:jump[j+1]-1])
  firing_one_cell <- data_frame(firing_one_cell)
  speed <- c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86)
  speed <- data_frame(speed)
  trapezoid_areas_dir2[j] <- map2(speed, firing_one_cell, find_trap)
}

csa_norm_trapezoid_areas_dir2 <- map(trapezoid_areas_dir2[], step_fun)


direction_1 <- data.frame(matrix(unlist(csa_norm_trapezoid_areas_dir1)))
names(direction_1) <- "direction_1"

direction_2 <- data.frame(matrix(unlist(csa_norm_trapezoid_areas_dir2)))
names(direction_2) <- "direction_2"

csa_auc <- bind_cols(direction_1, direction_2)

####################
##     SSA AUC    ##
####################

ssa_quad <- ssa_all_cells_norm %>% 
  ungroup() %>% 
  group_by(bird_id, direction) %>% 
  select(bird_id, direction, degrees, norm_mean)

dir1_position <- seq(1, nrow(ssa_quad), by = 2)
dir2_position <- seq(2, nrow(ssa_quad), by = 2)

ssa_quad_dir1 <- ssa_quad[dir1_position,]
ssa_quad_dir2 <- ssa_quad[dir2_position,]


jump <- seq(1,nrow(ssa_quad_dir1) + 12, by = 12)
trapezoid_areas_dir1 <- matrix(0,length(jump)-1,1)

for(j in 2:length(jump)-1){
  firing_one_cell <- c(0, ssa_quad_dir1$norm_mean[jump[j]:jump[j+1]-1])
  firing_one_cell <- data_frame(firing_one_cell)
  speed <- c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86)
  speed <- data_frame(speed)
  trapezoid_areas_dir1[j] <- map2(speed, firing_one_cell, find_trap)
}


trapezoid_areas_dir1
#norm_areas <- trapezoid_areas[[1]]*max(steps)/steps

step_fun <- function(x){
  steps <- diff(c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86))
  sum(x * max(steps)/steps)
}


ssa_norm_trapezoid_areas_dir1 <- map(trapezoid_areas_dir1[], step_fun)


### areas under anti-preferred direction curves (ssa)

jump <- seq(1,nrow(ssa_quad_dir2) + 12, by = 12)
trapezoid_areas_dir2 <- matrix(0,length(jump)-1,1)

for(j in 2:length(jump)-1){
  firing_one_cell <- c(0, ssa_quad_dir2$norm_mean[jump[j]:jump[j+1]-1])
  firing_one_cell <- data_frame(firing_one_cell)
  speed <- c(0, 0.24, 0.48, 0.99, 1.98, 4.03, 7.98, 16.03, 24.00, 32.03, 47.99, 64.02, 79.86)
  speed <- data_frame(speed)
  trapezoid_areas_dir2[j] <- map2(speed, firing_one_cell, find_trap)
}

ssa_norm_trapezoid_areas_dir2 <- map(trapezoid_areas_dir2[], step_fun)


direction_1 <- data.frame(matrix(unlist(ssa_norm_trapezoid_areas_dir1)))
names(direction_1) <- "direction_1"

direction_2 <- data.frame(matrix(unlist(ssa_norm_trapezoid_areas_dir2)))
names(direction_2) <- "direction_2"

ssa_auc <- bind_cols(direction_1, direction_2)



csa_auc <- csa_auc %>% 
  mutate(activity_type = "csa")
ssa_auc <- ssa_auc %>% 
  mutate(activity_type = "ssa")


all_cell_analysis <- read_csv("~/Documents/Projects/Flocculus-recording/all.cell.analysis.csv")
#all_cell_rows <- which(is.na(all_cell_analysis$test.dir1) == FALSE)

rows <- c(1:46, 48:107)

lm_auc <- all_cell_analysis %>% 
  filter(species == 1) %>% 
  filter(is.na(test.dir1) == FALSE) %>% 
  slice(rows) %>% 
  select(dir1.area, dir2.area) %>% 
  rename(direction_1 = dir1.area, direction_2 = dir2.area) %>% 
  mutate(activity_type = "lm")

#which(lm_auc$direction_1 < 0 & lm_auc$direction_2 > 0)
  


all_auc <- bind_rows(lm_auc, csa_auc, ssa_auc)


p1 <- ggplot(all_auc, aes(x = direction_1, y = direction_2, fill = activity_type)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
  geom_point(aes(color = activity_type), shape = 21, color = "black", size = 4, alpha = 1/1.5) +
  scale_fill_manual(values = alpha(c("#EAE72B", "#A12E91", "#F79441"))) +
  ylim(-60,50) +
  xlim(-20,70)

p1

## ggsave("quadrant_plot2.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))

