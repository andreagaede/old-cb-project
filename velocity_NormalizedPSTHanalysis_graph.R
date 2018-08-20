#### last updated May 2 2016 - A. Gaede
# Average baseline firing is used for %change, then data is normalized against highest average firing rate

# setwd("~/Documents/DATA/2015-ZF-edited PSTH/ZF speed re-sort")
# setwd("~/Documents/DATA/2015-CALAN - PSTH - speed test")
setwd("~/Documents/DATA/2016 - speed test PSTH")

# neg quadrant Hb files 50, 47, 12, 13, 20, 21

#################################
# files that look real 2 May 2016 cerebellum ZF exp:
# 1,5,6,7,8,9,10,11,12,14,16

title=list.files()[16]     #change file list number
hist1<-read.csv(list.files()[16]) #change file list number
hist1$file <- hist1[1,1]
hist1 <- hist1[3:dim(hist1)[1],]
head(hist1)
names(hist1) <- c('dir', 'speed', paste('bin', as.character(1:20), sep=""))
hist1[dim(hist1)[1]+1,]<-hist1[dim(hist1)[1]-1,]
rownames(hist1)<-1:dim(hist1)[1]
# for(i in 1:3){hist1[,i] <- as.numeric(as.character(hist1[,i]))}

rowmeans<-rowMeans(hist1[,3:22])

baseline<-rowmeans[seq(1,length(rowmeans),by=2)]



percentchange <- (rowmeans[seq(2,length(rowmeans),by=2)] - mean(baseline))/mean(baseline) * 100

data<-cbind(percentchange, hist1[seq(2,length(rowmeans),by=2),1:2])


degrees <- acos(((sqrt((((data$speed)*144*0.02768)/2)^2+30^2))^2+(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))^2-((data$speed)*144*0.02768)^2)/(2*(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))*(sqrt((((data$speed)*144*0.02768)/2)^2+30^2))))*180/pi

degrees<-round(degrees,2)
data<-cbind(data,degrees)



############

# means <- aggregate(data$percentchange, by=list(data$dir,data$degrees), 'mean')



# means <- aggregate(data$percentchange, by=list(data$dir,degrees), 'mean')
# names(means) <- c('dir', 'degrees', 'pchange') 

# myse <- function(x){
	# sd(x)/sqrt(length(x))
# }

# se <- aggregate(data$percentchange, by=list(data$dir,degrees), 'myse')

# means$se <- se$x
# means

# max_firing <- max(means[,3])
# norm <- means[,3]/max_firing
# norm_means <- cbind(means$dir, means$degrees, norm)


######## normalized before mean #####

means <- aggregate(data$percentchange, by=list(data$dir,degrees), 'mean')
names(means) <- c('dir', 'degrees', 'pchange') 

max_firing <- max(means[,3])
norm <- data$percentchange/max_firing
normmeans <- cbind(data, norm)
normmeansf <- aggregate(normmeans$norm, by=list(data$dir, data$degrees), 'mean')

myse <- function(x){
	sd(x)/sqrt(length(x))
}

se <- aggregate(normmeans$norm, by=list(normmeans$dir,normmeans$degrees), 'myse')

normmeansf$se <- se$x

names(normmeansf) <- c('dir', 'degrees', 'norm', 'SEM')
normmeansf


norm_df = data.frame(dir = c(normmeansf[,1]), degrees = c(normmeansf[,2]), norm = c(normmeansf[,3]), sem = c(normmeansf[,4]))





pref<-norm_df[1,1]
anti<-pref+180

# #######log scale plot for small multiples#####
#  plot(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], type ="n",xlab="", ylab="", xlim=range(log10(norm_df$degrees)), ylim=range(-1,1.2), xaxt='n', yaxt='n')
# 
#  lines(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], col="green", type="b", pch=15, lwd=3)
#  lines(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti], col="blue", type="b", pch=17, lwd=3)
#  abline(0,0, col = "gray60", lty = 2, lwd = 2)




####### log scale line plot #########
plot(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], type ="n",xlab="log(Dot Speed (degrees/s))", ylab="Normalized Firing Rate", main=title, xlim=range(log10(norm_df$degrees)), 
ylim=range(norm_df$norm+norm_df$sem,norm_df$norm-norm_df$sem))

#ylim=range(-5,2))

arrows(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref]+norm_df$sem[norm_df$dir==pref], log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref]-norm_df$sem[norm_df$dir==pref], angle=90,code=3, length=0.015)
arrows(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti]+norm_df$sem[norm_df$dir==anti], log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti]-norm_df$sem[norm_df$dir==anti], angle=90,code=3, length=0.015)
lines(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], col="green", type="b", pch=15)
lines(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti], col="blue", type="b", pch=17)
#abline(0,0, col = "gray60", lty = 2, lwd = 2)
#abline(0,0.9, col = "red", lty =2, lwd =2)

######## spaghetti plot #######
# xyplot(norm_df$norm~dir,groups=degrees,means,type='l', col=c("black","red","pink","orange","gold","yellowgreen","palegreen","darkgreen","royalblue","powderblue","orchid","violet"),main=title) 
# 
# #######log scale plot for small multiples#####
# plot(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], type ="n",xlab="", ylab="", xlim=range(log10(norm_df$degrees)), ylim=range(-1,1.2), xaxt='n', yaxt='n')
# 
# lines(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], col="green", type="b", pch=15, lwd=3)
# lines(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti], col="blue", type="b", pch=17, lwd=3)
# abline(0,0, col = "gray60", lty = 2, lwd = 2)
# 
# 
# 
# 
# ####### log scale line plot #########
# plot(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], type ="n",xlab="log(Dot Speed (degrees/s))", ylab="Normalized Firing Rate", main=title, xlim=range(log10(norm_df$degrees)), ylim=range(-0.5,1.2))
# 
# #ylim=range(norm_df$norm))
# 
# arrows(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref]+norm_df$sem[norm_df$dir==pref], log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref]-norm_df$sem[norm_df$dir==pref], angle=90,code=3, length=0.015)
# arrows(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti]+norm_df$sem[norm_df$dir==anti], log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti]-norm_df$sem[norm_df$dir==anti], angle=90,code=3, length=0.015)
# lines(log10(norm_df$degrees[norm_df$dir==pref]), norm_df$norm[norm_df$dir==pref], col="green", type="b", pch=15)
# lines(log10(norm_df$degrees[norm_df$dir==anti]), norm_df$norm[norm_df$dir==anti], col="blue", type="b", pch=17) 
# abline(0,0, col = "gray60", lty = 2, lwd = 2)
# abline(0.9,0, col = "red", lty =2, lwd =2)
# 
# 
