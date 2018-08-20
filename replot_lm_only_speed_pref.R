## run zf calan cell match tuning.R first

########################################################################
########################################################################
get.preferred.speed <- function(file.list){
  percent <- seq(0.5,0.95,by = 0.05)
  cell.ids <- matrix(NA,length(file.list),4)
  pref.area <- matrix(NA,length(file.list),4)
  norm.pref.area <- matrix(NA,length(file.list),4)
  pref.speed <- rep(NA,length(file.list))
  areas <- matrix(NA,12,2)
  norm.areas <- matrix(NA,12,2)
  tuning.sums <- matrix(NA,length(file.list),length(percent))
  tuning.80 <- matrix(NA,length(file.list),12)
  
  for(i in 1:length(file.list)){
    cell.for.check <- NULL
    print(i)
    cell.for.check <- extract.num.from.string(file.list[i])
    if(check.match(cell.for.check,all.cell.analysis[,1:4])){
      onecell.data <- read.csv(file.list[i], header = FALSE)
      onecell.data <- cbind(apply(onecell.data[,3:22],1,quick.mean),onecell.data)
      cell.ids[i,] <- cell.for.check
      
      onecell.meanbase <- quick.mean(onecell.data[is.na(onecell.data[,2]),1])
      
      onecell.nonbase <- onecell.data[!is.na(onecell.data[,2]),1:3]
      onecell.max <- max(abs(onecell.nonbase[,1]-onecell.meanbase))
      onecell.nonbase <- cbind(onecell.nonbase,(onecell.nonbase[,1]-onecell.meanbase)/onecell.max)
      colnames(onecell.nonbase) <- c("mean.resp", "dir", "speed", "norm.to.max")
      onecell.nonbase[,"dir"] <- onecell.nonbase[,"dir"]%%360
      
      #Now get the area under the response using trapezoid.areas() then select a preferred direction
      onecell.means <- aggregate(norm.to.max ~ dir + speed, data = as.data.frame(onecell.nonbase[,2:4]), quick.mean)
      onecell.error <- aggregate(norm.to.max ~ dir + speed, data = as.data.frame(onecell.nonbase[,2:4]), quick.stdev)
      onecell.means$speed <- quick.speed(onecell.means$speed)
      onecell.error$speed <- quick.speed(onecell.error$speed)
      onecell.dirs <- unique(onecell.means$dir)
      
      #Get prefdir from the reference list
      onecell.prefdir <- get.prefdir.for.cell(cell.ids[i,])
      
      #Check if one of the tested directions is within 45 degrees of the prefdir
      diff.from.prefdir <- c(angle.diff(onecell.dirs[1], onecell.prefdir), angle.diff(onecell.dirs[2], onecell.prefdir))
      closest.testdir <- numeric(0)
      closest.testdir <- onecell.dirs[diff.from.prefdir < 45]
      
      if(length(closest.testdir) == 1){
        print("Cell was tested appropriately")
        #Rearrange the onecell.dirs variable (prefdir first, then antipref)
        ifelse(onecell.dirs[1] == closest.testdir, onecell.dirs, onecell.dirs <- c(onecell.dirs[2],onecell.dirs[1]))
        pref.speed[i] <- onecell.means$speed[onecell.means$norm.to.max == max(onecell.means$norm.to.max[onecell.means$dir == closest.testdir])]
        
        for(j in 1:length(onecell.dirs)){
          areas[,j] <- trapezoid.areas(c(0,onecell.means$speed[onecell.means$dir == onecell.dirs[j]]),
                                       c(0,onecell.means$norm.to.max[onecell.means$dir == onecell.dirs[j]]))
        }
        
        #Store the areas
        pref.area[i,] <- c(onecell.dirs[1]%%360,sum(areas[,1]),onecell.dirs[2]%%360,sum(areas[,2]))
        #Normalize the area by step size
        steps <- diff(c(0,unique(onecell.means$speed)))
        norm.areas <- areas*max(steps)/steps
        norm.pref.area[i,] <- c(onecell.dirs[1]%%360,sum(norm.areas[,1]),onecell.dirs[2]%%360,sum(norm.areas[,2]))
        
        #Test tuning
        tuning.hold <- matrix(NA,length(percent),13)
        onecell.max.mean <- max(onecell.means$norm.to.max[onecell.means$dir%%360 == pref.area[i,1]])
        for(j in 1:length(percent)){
          tuning.hold[j,] <- c(percent[j],simple.tuning(onecell.means$speed[onecell.means$dir%%360 == pref.area[i,1]],onecell.means$norm.to.max[onecell.means$dir%%360 == pref.area[i,1]],onecell.max.mean*percent[j]))
        }
        tuning.sums[i,] <- apply(tuning.hold[,2:13], 1, quick.sum)
        tuning.80[i,] <- tuning.hold[which(tuning.hold[,1] == 0.8),2:13] # changed to 0.7 for 70%, 0.9 for 90%
      }
      else{
        pref.area[i,] <- rep(NA,4)
        norm.pref.area[i,] <- rep(NA,4)
        pref.speed[i] <- NA
        tuning.80[i,] <- rep(NA,12)
      }
    }
  }
  tuning.sums[which(tuning.sums[] == 0)] <- NA 
  return(cbind(cell.ids,pref.area,pref.speed,norm.pref.area,tuning.sums,tuning.80))
}
########################################################################
########################################################################




speeds <- c(0.24,0.48,0.99,1.98,4.03,7.98,16.03,24.00,32.03,47.99,64.02,79.86)
log.speeds <- log10(speeds)
calan.speedbins <- apply(all.cell.analysis[which(all.cell.analysis$species == 2),12:23],2,quick.sum)/length(which(all.cell.analysis$species == 2 & is.na(all.cell.analysis$dir1.area) == FALSE))
zf.speedbins <- apply(all.cell.analysis[which(all.cell.analysis$species == 1),12:23],2,quick.sum)/length(which(all.cell.analysis$species == 1 & is.na(all.cell.analysis$dir1.area) == FALSE))

barplot(rbind(calan.speedbins,zf.speedbins), beside = TRUE, col = c(2,1),
        xlab = "speed bins (log speed)", ylab = "proportion of cell pop.", names.arg = round(log.speeds,3))

barplot(zf.speedbins, col = "#A12E91",
        xlab = "speed bins (log speed)", ylab = "proportion of cell pop.", main = "90% max", names.arg = round(log.speeds,3))


########################################################################
########################################################################
bin.count <- matrix(NA,2,length(speeds))
for(j in 1:length(speeds)){
  bin.count[1:2,j] <- c(length(which(all.cell.analysis$species == 2 & all.cell.analysis$pref.speed == speeds[j]))/length(which(all.cell.analysis$species == 2 & is.na(all.cell.analysis$dir1.area) == FALSE)),
                        length(which(all.cell.analysis$species == 1 & all.cell.analysis$pref.speed == speeds[j]))/length(which(all.cell.analysis$species == 1 & is.na(all.cell.analysis$dir1.area) == FALSE)))
}


barplot(bin.count[2,], col = "#A12E91",
        xlab = "speed bins (log speed)", ylab = "proportion of cell pop.", main = "pref speeds", names.arg = round(log.speeds,3))



## can work on making better plot later
library(tidyverse)

speeds_df <- data.frame(speeds)
firing_bin <- bin.count[2,]
firing_bin <- data.frame(firing_bin) 

lm_plot <- bind_cols(speeds_df, firing_bin)

ggplot(lm_plot, aes(x = speeds, y = firing_bin)) + 
  geom_bar(stat = "identity")

write_csv(lm_plot, "zf_max_speeds.csv")


############################
## plot LM and CB data together

zf.speedbins <- data.frame(zf.speedbins)
speeds <- c(0.24,0.48,0.99,1.98,4.03,7.98,16.03,24.00,32.03,47.99,64.02,79.86)
speeds <- data.frame(speeds)
zf_max90_speeds <- bind_cols(zf.speedbins,speeds)
write_csv(zf_max90_speeds, "zf_max90_speeds.csv")








