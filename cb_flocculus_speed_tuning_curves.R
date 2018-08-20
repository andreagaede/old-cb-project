## Flocculus speed analysis
## A. Gaede October 22, 2016

## speed tuning curves
## distribution of preferred speeds
## compare pref speed in flocculus to pref speed in LM
## speed tuning width -- compare to LM cells

## May need to do this with Benny's programs for the poster, then rewrite analysis later using tidyverse

## Good looking graphs: 1, 2, 9, 10, 11, 14, 16, 28, 32, 33, 34, 38, 39, 52, 55, 60
## work_with_these <- data.frame(list.files()[c(1, 2, 9, 10, 11, 14, 16, 28, 32, 33, 34, 38, 39, 52, 55, 60)])
## write_csv(work_with_these, "work_with_these.csv")
## maybes: 8, 12, 21, 29, 36, 41, 45, 49, 53, 56, 58, 59, 62, 63

## Melissa's data
# setwd("~/Documents/Projects/Flocculus-recording/2016 - CB speed PSTH")
# setwd("~/Documents/Projects/Flocculus-recording/2016_CB_CSA_working_data")
# setwd("~/Documents/Projects/Flocculus-recording/2016_CB_SSA_working_data")

## Hallie's data
 setwd("~/Documents/Projects/Flocculus-recording/2016-hallie-csa")

## LM data
# setwd("~/Documents/DATA/zf hb pg ephys/2015-CALAN-PSTH-speed")
# setwd("~/Documents/DATA/zf hb pg ephys/2015-ZF-PSTH-speed")

#ZF files used: 10, 124, 23, 48, 66, 72, 70, 78, 83, 86, 104, 114, 115, 129, 35, 62, 64, 84
#CALAN files used: 1, 5, 4, 9, 14, 20, 25, 32, 38, 49, 63, 66, 73, 84, 93, 94, 97, 100
#Calan 22, 23, 24, 44 have really negative anti-preferred

## Figure 4 panels are ZF 83, 62; Calan 39


## load recquired packages
library(tidyverse)
library(readr)

fn <- 6
title=list.files()[fn] 
psth <- read_csv(list.files()[fn], col_names = FALSE)
names(psth) <- c('direction', 'speed', paste('bin', as.character(1:20), sep=""))

psth$direction <- round(psth$direction,0)

# row_means <- psth %>% 
#   mutate(spike_rate = cummean(psth[,3:22]))




row_means<-rowMeans(psth[,3:22])

baseline<-row_means[seq(2,length(row_means),by=2)]
baseline <- mean(baseline)

motion_firing <- row_means[seq(1,length(row_means), by = 2)]
firing_less_baseline <- motion_firing - baseline




my_data <- cbind(firing_less_baseline, psth[seq(1,length(row_means),by=2),1:2])
degrees <- acos(((sqrt((((my_data$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((my_data$speed)*144*0.02768)/2)^2+30^2))^2-((my_data$speed)*144*0.02768)^2)/(2*(sqrt((((my_data$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((my_data$speed)*144*0.02768)/2)^2+30^2))))*180/pi
degrees <- round(degrees,2)
my_data <- cbind(my_data,degrees)

## normalize to single maximum value for single trial
max_firing <- max(abs(my_data$firing_less_baseline))



# norm_means <- my_data %>% 
#   group_by(degrees, direction) %>% 
#   summarise(spike_rate = mean(firing_less_baseline))

## normalize to an average maximum over 5 trials
# max_firing <- max(abs(norm_means$spike_rate))

my_data <- my_data %>% 
  mutate(norm_firing = firing_less_baseline/max_firing) 

## Function for calculating SEM
myse <- function(x){
  sd(x)/sqrt(length(x))
}

norm_means <- my_data %>% 
  group_by(degrees, direction) %>% 
  summarise(spike_rate = mean(norm_firing))

se <- aggregate(my_data$norm_firing, by=list(my_data$direction,my_data$degrees), 'myse')
norm_means$se <- se$x
names(norm_means) <- c('degrees', 'direction', 'norm_spike_rate', 'SEM')

######### Graphing code:


norm_speed <- as_tibble(norm_means)
norm_speed$direction <- as.factor(norm_speed$direction)
limits <- aes(ymax = norm_spike_rate + SEM, ymin = norm_spike_rate - SEM)

# p <- ggplot(norm_speed, aes(x = degrees, y = norm_spike_rate, color = direction, group = direction)) +
#   scale_y_continuous(limits = c(-1.0,1.0))
# 
# p + geom_point(aes(shape = direction, size = 2)) +
#   scale_shape_manual( values = c(15,23)) +
#   geom_line() +
#   geom_errorbar(limits, width = 1)


max80_line <- 0.8*max(norm_speed$norm_spike_rate)

## same graph, but with x-axis scaled
p1 <- ggplot(norm_speed, aes(x = degrees, y = norm_spike_rate, color = direction, group = direction)) +
  scale_y_continuous(limits = c(-1.0,1.0)) + scale_x_log10() +
  geom_point(aes(shape = direction, size = 2), fill = "gray44") +
  scale_shape_manual( values = c(15,23)) +
  geom_line() +
  geom_errorbar(limits) +
  scale_color_manual(values=c("black", "gray44")) +
  theme_classic() +
  xlab("Stimulus velocity (degrees/s)") +
  ylab("Normalized firing rate") +
  theme(legend.position="none") +
  geom_hline(color = "gray", linetype = "dashed", yintercept = 0)

p1 <- p1 + ggtitle(title)
p1

 setwd("~/Documents/Projects/Flocculus-recording/hallie_speed_tuning_plots")
 ggsave("csa_sm_1603-303.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))

## setwd("~/Documents/Projects/Flocculus-recording/hallie-m-csa-tuning-plots")
## ggsave("csa_sm_17.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))


## setwd("~/Documents/Projects/Flocculus-recording/speed_tuning_curves")
## ggsave("csa_sm_5.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))


title
## setwd("~/Documents/Projects/Flocculus-recording/speed_tuning_curves")
## ggsave("ZF16-20-floc-trk1-1000-va-cell1.pdf", plot = p1, width = 11, height = 8.5, units = c("in"))
