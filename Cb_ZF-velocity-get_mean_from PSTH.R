##### May 2 2016
# for csv files with no column headers 

# setwd("~/Documents/DATA/2016 - speed test PSTH")
setwd("~/Documents/Projects/Flocculus-recording/2016 - CB speed PSTH")

###################################################################

# mean spike frequencies for each speed in pref and anti-pref direction

for (i in 1:length(list.files())){
  
file.name <- list.files()[i]

  hist1<-read.csv(list.files()[i], header = FALSE) #change file list number
  
  head(hist1)
  names(hist1) <- c('dir', 'speed', paste('bin', as.character(1:20), sep=""))
  
  rowmeans<-rowMeans(hist1[,3:22])
  
  baseline<-rowmeans[seq(2,length(rowmeans),by=2)]
  meanbaseline<-mean(baseline)
  
  speedmeans<-rowmeans[seq(1,length(rowmeans),by=2)]
  
  data<-cbind(speedmeans,hist1[seq(1,length(rowmeans),by=2),1:2])
  
  
  degrees <- acos(((sqrt((((data$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))^2-((data$speed)*144*0.02768)^2)/(2*(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))))*180/pi
  
  degrees<-round(degrees,2)
  data<-cbind(data,degrees)
  
  means <- aggregate(data$speedmeans, by=list(data$dir,degrees), 'mean')
  names(means) <- c('dir', 'degrees', 'spikes/s') 
  
    myse <- function(x){
    	 sd(x)/sqrt(length(x))
     }
    
   se <- aggregate(data$speedmeans, by=list(data$dir,degrees), 'myse')
  
   means$se <- se$x
   sem <- sd(baseline)/sqrt(length(baseline))
   
  means<-cbind(means,meanbaseline,sem)
  means
  
newname<-sprintf("%s_means.csv",file.name)
# finalname<- paste("~/Documents/DATA/2015-CALAN speed test means/",newname,sep="")
finalname <- paste("~/Documents/Projects/Flocculus-recording/CB-ZF-speed-MEANS/",newname,sep="")
write.csv(means, file = finalname, row.names=FALSE)
  
}

setwd("~/Documents/Projects/Flocculus-recording/CB-ZF-speed-MEANS/")

files = list.files(pattern="*.csv")
alldata = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
head(alldata)

# write.csv(alldata, file = "CALAN-speed-data-091715.csv")
write.csv(alldata, file = "ZF-Cb-speed-data-042517.csv")
