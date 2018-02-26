#Approach 1
#stop light analysis to identify breakpoints for each criteria (i.e. identify histograms)

#Approach 2
#carry raw data distribution through to final distribution of a weighted mean, and then identify breakpoints

#Sensitivity analysis
# change each criteria by 10% to examine influence of each parameter on final mean

#uncertainty analysis
#re-sample from the log normal distrubion with known mu and sigma



#Analysis for  M. Savoca: bycatch database analysis for DOM
#S. Brodie 26 Feb 2018

#The data and code can be added to a github project later if required
#A powerpoint also summarises the results and approach here

#-----Save and load image---
# save.image('~/PROJECTS/Savoca DOM/SensAnal_Image')
# load('~/PROJECTS/Savoca DOM/SensAnal_Image')

#------Load librarys-----
require(devtools) 
install_github("HeatherWelch/beatr/beatr") 
library(beatr)

#------Load in data-------
#The data has bee cleaned by Steph in excel before importing. Primarily I gave the colours a numeric code: 1=green, 2=yellow, 3=red
data <- read.csv('~/PROJECTS/Savoca DOM/Stoplight_clean_forImport.csv', header=T)

# The names are hell gross so renaming here. This can be used to back track names if there is any confusion
colnames(data) <- c("Index","Fishery","GearType","Year","Target","Target_HMS","Overfishing_Fm",
                    "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                    "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA",
                    "CAT_Target_HMS","CAT_Overfishing_Fm","CAT_Overfishing_Bt", "CAT_TotalBycatch_lbs",
                    "CAT_TotalBycatch_inds","CAT_Bycatch_ratio","CAT_ESA_num","CAT_ESA_lbs",
                    "CAT_ESA_birdturt","CAT_IUCN_num","CAT_IUCN_lbs","CAT_IUCN_birdturt","CAT_MMPA")

#-----Get NG:G ratio-------- 
#Get sum of red, yellow, and green 
data$red <- apply(data[,19:31],1,function(x) length(which(x==3)))
data$yellow <- apply(data[,19:31],1,function(x) length(which(x==2)))
data$green <- apply(data[,19:31],1,function(x) length(which(x==1)))

#Plot histograms
quartz()
par(mfrow=c(3,1))
hist(data$red, main="", xlab="Reds") 
hist(data$yellow, main="", xlab="Yellows")
hist(data$green, main="", xlab="Greens") #interesting: if a fishery has 1 green crtieria then it is more likely to have more green criteria. 

#Estimate Ratio
data$Ratio_NG.G <- (data$red+ data$yellow) / data$green
hist(data$Ratio_NG.G, main="", xlab="Ratio NG:G")

#Find 25% and 75% data quantiles
quantiles <- quantile(data$Ratio_NG.G,probs=c(0.33,0.66), na.rm=T)

# Allocate each fishery a rank based on quantiles: bad for DOM =1, medium = 2, good for DOM = 3
data$Fishery_Rank <- ifelse(data$Ratio_NG.G<=quantiles[1],1,
                            ifelse(data$Ratio_NG.G>=quantiles[2],3,2))

#Have a quick look to check results
length(data$Fishery[data$Fishery_Rank==3])
unique(data$Fishery[data$Fishery_Rank==3]) #
data$Fishery[data$Ratio_NG.G>4] #Hot! this matches up close enough to Matt's results

#-------Sensitivity Analysis------
#Remove one criteria, repeat ranking above, and see how many times a fishery falls into the same rank

#Create new data frame to write data to
data_sim_0.1 <- as.data.frame(matrix(data = NA, nrow = 348, ncol=16))
colnames(data_sim_0.1) <- c("Index","Fishery","Rank_FullWeight",1:13)
data_sim_0.1[,1] <- data$Index
data_sim_0.1[,2] <- data$Fishery
data_sim_0.1[,3] <- data$Fishery_Rank
#Start iteration
for (s in 1:13){ #1 iteration for each 13 criteria

weights <- c(1,1,1,1,1,1,1,1,1,1,1,1,1) #original weights

#change weight on each iteration
weights[s] <- weights[s]*0 #change weight on each iteration. Here weight is a constant 0

#Multiply weights by raw data
reds <- apply(t(t(data[,19:31])*weights),1,function(x) length(which(x==3)))
yellows <- apply(t(t(data[,19:31])*weights),1,function(x) length(which(x==2)))
greens <- apply(t(t(data[,19:31])*weights),1,function(x) length(which(x==1)))

#calculate ratio
ratio <- (reds + yellows) / greens

#get quantiles of ratio
quants <- quantile(ratio,probs=c(0.25,0.75), na.rm=T)

#allocate rank 
ranks <- ifelse(ratio<=quants[1],1,ifelse(ratio>=quants[2],3,2))

#write data to dataframe
col_idx <- s+3 #to offset first to columns 
data_sim_0.1[,col_idx] <- ranks
}
#Check it out
head(data_sim_0.1)

#-----Goal 1 and 2: Explore & Visualise output of sensitivity analysis-----
#Now look at results in a few different ways
#Goal 1: plot at correlation coefficents. The lowest correlation indicates that factor has the strongest influence on results
quartz()
par(mar=c(9,4,2,2),xpd=NA)
plot(1:13,c(cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`1`),
            cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`2`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`3`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`4`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`5`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`6`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`7`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`8`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`9`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`10`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`11`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`12`),
           cor(data_sim_0.1$Rank_FullWeight,data_sim_0.1$`13`)),
     pch=19, xlab="",ylab="Correlation coefficient",xaxt="n")
axis(1,at=1:13,las=2,labels=c("Target_HMS","Overfishing_Fm",
                        "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                        "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA"))


#Goal 2: look at which fisheries switch between ranking, and which are persistent

counter=1
for (r in 1:348){
  val <-apply(data_sim_0.1[counter,4:16],1,function(x) length(which(x==data_sim_0.1[counter,3])))
  data_sim_0.1[counter,17] <- val
  counter=counter+1
}
head(data_sim_0.1)
colnames(data_sim_0.1)[17] <- "Persistence"
hist(data_sim_0.1$Persistence, main="", xlab="Number of times a fishery had the same result as raw data")

unique(data_sim_0.1$Fishery[data_sim_0.1$Persistence==2])

hist(data_sim_0.1$Persistence[data_sim_0.1$Rank_FullWeight==3])

length(data_sim_0.1[data_sim_0.1$Rank_FullWeight==3 & data_sim_0.1$Persistence<13,])

#------Goal 3: Explore sensitivity of weights using a weighted mean----
#This is more intuitive as it means the yellow category actually contributes as an interim category

#Create Mean on the raw data
data$Mean_ColourCrit <- apply(data[,19:31],1,function(x) mean(x, na.rm=T))
hist(data$Mean_ColourCrit) #looks good

#Can get quantiles and turn into rank if required
quantiles <- quantile(data$Mean_ColourCrit,probs=c(0.33,0.66), na.rm=T) #can make quantiles
data$Mean_Rank <- ifelse(data$Mean_ColourCrit<=quantiles[1],1,ifelse(data$Mean_ColourCrit>=quantiles[2],3,2))
hist(data$Mean_Rank)

#Create new data frame to write data to
data_mean_weights <- as.data.frame(matrix(data = NA, nrow = 348, ncol=6))
colnames(data_mean_weights) <- c("Index","Fishery","Mean_ColourCrit",
                                 "W1_mean","W2_mean","W3_mean")
data_mean_weights[,1] <- data$Index
data_mean_weights[,2] <- data$Fishery
data_mean_weights[,3] <- data$Mean_ColourCrit
#Start iteration
for (s in 1:3){ #1 iteration for each 13 criteria
  if (s==1){
    weights = c(10,1,1,1,10,1,1,10,10,1,1,10,1) #weighting based on sensitivity analysis
  }  
  if (s==2){
    weights = c(1,1,1,1,1,1,1,10,10,10,10,10,10) #weighting based on IUCN & ESA & MMPA importance
  } 
  if (s==3){
    weights = c(1,10,10,10,1,1,1,1,1,1,1,1,1) #Weigting based on lbs
  }
  
  mean <- apply(data[,19:31],1,function(x) weighted.mean(x, weights, na.rm=T))
  
  #write data to dataframe
  col_idx <- s+3 #to offset first to columns 
  data_mean_weights[,col_idx] <- mean
}
#Check it out
head(data_mean_weights)

#Plot pdf's of weight simulations
hist(data_mean_weights$Mean_ColourCrit)
hist(data_mean_weights$W1_mean)
hist(data_mean_weights$W2_mean)
hist(data_mean_weights$W3_mean)

orig <- dlnorm(seq(0.5,4,0.001),mean(log(data_mean_weights$Mean_ColourCrit)),sd(data_mean_weights$Mean_ColourCrit))
w1 <- dlnorm(seq(0.5,4,0.001),mean(log(data_mean_weights$W1_mean)),sd(data_mean_weights$W1_mean))
w2 <- dlnorm(seq(0.5,4,0.001),mean(log(data_mean_weights$W2_mean)),sd(data_mean_weights$W2_mean))
w3 <- dlnorm(seq(0.5,4,0.001),mean(log(data_mean_weights$W3_mean)),sd(data_mean_weights$W3_mean))

quartz()
plot(seq(0.5,4,0.001),orig, type="l",lty=1, lwd=2, ylab="Relative Frequency",yaxt="n",xlab="Mean Criteria Score")
lines(seq(0.5,4,0.001),w1, type="l",lty=2, col="blue",lwd=2)
lines(seq(0.5,4,0.001),w2, type="l",lty=3, col="red",lwd=2)
lines(seq(0.5,4,0.001),w3, type="l",lty=4, col="green",lwd=2)

legend("topright",c("Equal Weight","Key criteria Weight","Listing Weight","Fish Metric Weight"),
       lty=c(1,2,3,4),lwd=c(2,2,2,2),col=c("black","blue","red","green"))
#---end
beatr("tubular")
