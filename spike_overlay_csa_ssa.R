library(tidyverse)
library(readr)

## for SfN 2016 poster


## spike csa
spike1_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike-CSA26-550.csv")
names(spike1_average) <- c("time", "voltage", "sem")
#spike1_average <- spike1_average[1:1000,]
spike1_average <- spike1_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

p <- ggplot(spike1_average, aes(x = time, y = voltage))
p + geom_line() 

p + geom_point() +
  geom_line(aes(x = time, y = sem_max))+
  geom_line(aes(x = time, y = sem_min))

# sem_max_table <- spike1_average %>% 
#   select(time, sem_max) %>% 
#   mutate(line1 = "sem_max")
# 
# spike1 <- spike1_average %>% 
#   select(time, voltage) %>% 
#   mutate(line2 = "spike")
# 
# sem_min_table <- spike1_average %>% 
#   select(time, sem_min) %>% 
#   mutate(line3 = "sem_min")
# 
# full_table <- bind_rows(spike1,sem_max_table,sem_min_table)  

table_with_gather1 <- gather(spike1_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)


p1 <- ggplot(table_with_gather1, aes(x = time, y = volt_value)) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#EAE72B"))

p1
# ggsave("spike-CSA26-550_plot.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))


## spike_ssa
spike2_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike-SSA20-1002.csv")
spike2_average <- spike2_average[1:300,]
names(spike2_average) <- c("time", "voltage", "sem")
spike2_average <- spike2_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

table_with_gather2 <- gather(spike2_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)

p2 <- ggplot(table_with_gather2, aes(x = time, y = volt_value)) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#F79441"))
p2

# ggsave("SSA20-1002_plot2.pdf", plot = p2, width = 11, height = 8.5, units = c("in"))

## spike_16-01-1051-smcsa
spike3_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike_16-01-1051-smcsa.csv")
names(spike3_average) <- c("time", "voltage", "sem")
spike3_average <- spike3_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

table_with_gather3 <- gather(spike3_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)

p3 <- ggplot(table_with_gather3, aes(x = time, y = volt_value)) + ylim(-1.2, 0.8) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#E66B5D"))
p3

# ggsave("spike_16-01-1051-smcsa_plot.pdf", plot = p3, width = 11, height = 8.5, units = c("in"))


## spike_16-01-1051-lgcsa
spike4_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike_16-01-1051-lgcsa.csv")
names(spike4_average) <- c("time", "voltage", "sem")
spike4_average <- spike4_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

table_with_gather4 <- gather(spike4_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)

p4 <- ggplot(table_with_gather4, aes(x = time, y = volt_value)) + ylim(-1.2, 0.8) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#E66B5D"))
p4

# ggsave("spike_16-01-1051-lgcsa_plot.pdf", plot = p4, width = 11, height = 8.5, units = c("in"))


## spike_csa02-1101
spike5_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike_csa02-1101.csv")
names(spike5_average) <- c("time", "voltage", "sem")
spike5_average <- spike5_average[1:300,]
spike5_average <- spike5_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

table_with_gather5 <- gather(spike5_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)

p5 <- ggplot(table_with_gather5, aes(x = time, y = volt_value)) + ylim(-1.2, 0.8) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#EAE72B"))
p5

## ggsave("spike_csa02-1101_plot.pdf", plot = p5, width = 11, height = 8.5, units = c("in"))

## spike csa-16-01-947 CSA
spike6_average <- read_csv("~/Documents/Projects/Flocculus-recording/spike_16_01_947.csv")

names(spike6_average) <- c("time", "voltage", "sem")
spike6_average <- spike6_average %>% 
  mutate(sem_max = voltage + sem, sem_min = voltage - sem)

table_with_gather6 <- gather(spike6_average, key = "line", value = "volt_value", voltage,sem_max, sem_min)

p6 <- ggplot(table_with_gather6, aes(x = time, y = volt_value)) + 
  geom_line(aes(color = line), size = 1) +
  theme_minimal() +
  scale_color_manual(values=c("gray", "gray", "#EAE72B"))
p6
## ggsave("spike_csa16_01_947_plot.pdf", plot = p6, width = 11, height = 8.5, units = c("in"))
