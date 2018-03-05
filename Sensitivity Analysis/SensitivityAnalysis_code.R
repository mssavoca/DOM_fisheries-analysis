#Analysis for  M. Savoca: bycatch database analysis for DOM

#Analysis 1: stop light analysis to identify breakpoints for each criteria (i.e. identify histograms)
#Analysis 2: carry raw data distribution through to final distribution of a weighted mean, and then identify breakpoints
#Analysis 3: Sensitivity analysis - change each criteria to examine influence of each parameter on final mean

#-----Save and load image---
# save.image('~/PROJECTS/Savoca DOM/DOM_fisheries-analysis/Sensitivity Analysis/SensAnal_Image')
# load('~/PROJECTS/Savoca DOM/DOM_fisheries-analysis/Sensitivity Analysis/SensAnal_Image')

#------Load librarys-----
require(devtools) 
# install_github("HeatherWelch/beatr/beatr") 
library(beatr)
beatr("ecoroms") #start off the script with a banger

#------Load in data-------
#The data has been cleaned by Steph in excel before importing. Primarily I gave the colours a numeric code: 1=green, 2=yellow, 3=red
data <- read.csv('~/PROJECTS/Savoca DOM/Stoplight_clean_forImport.csv', header=T) #this is housed on Steph's computer to maintain privacy

# The names are hell gross so renaming here. This can be used to back track names if there is any confusion
colnames(data) <- c("Index","Fishery","GearType","Year","Target","Target_HMS","Overfishing_Fm",
                    "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                    "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA",
                    "CAT_Target_HMS","CAT_Overfishing_Fm","CAT_Overfishing_Bt", "CAT_TotalBycatch_lbs",
                    "CAT_TotalBycatch_inds","CAT_Bycatch_ratio","CAT_ESA_num","CAT_ESA_lbs",
                    "CAT_ESA_birdturt","CAT_IUCN_num","CAT_IUCN_lbs","CAT_IUCN_birdturt","CAT_MMPA")

#Manipulate data format for analysis. Most data is numeric (easy) but need to change 3 categories to some kind of numerical value. Overfishing (biomass and mortality metrics were changed to numeric in excel)
#Change HMS target to numeric
data$Target_HMS <- ifelse(data$Target_HMS=="N",1,
                          ifelse(data$Target_HMS=="M",2,
                                 ifelse(data$Target_HMS=="Y",3,data$Target_HMS)))

#Note that in excel I changed the values of "Overfishing_Fm" & "Overfishing_Bt" manually to be the total number of species 

#Standardise Data
data[32:44] <- scale(data[6:18],center = T,scale = T)
colnames(data)[32:44] <- c("STAND_Target_HMS","STAND_Overfishing_Fm",
                           "STAND_Overfishing_Bt", "STAND_TotalBycatch_lbs", "STAND_TotalBycatch_inds","STAND_Bycatch_ratio",
                           "STAND_ESA_num","STAND_ESA_lbs","STAND_ESA_birdturt","STAND_IUCN_num","STAND_IUCN_lbs","STAND_IUCN_birdturt","STAND_MMPA")

#----Analysis 1: identify criteria breakpoints----
#identify breakpoints for each criteria (i.e. identify histograms)
Breakpoints <- as.data.frame(matrix(NA,nrow=2,ncol=13))
colnames(Breakpoints) <- c("Target_HMS","Overfishing_Fm",
                           "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                           "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA")
rownames(Breakpoints) <- c("75%","90%")
for (b in 32:44){
  q <- quantile(data[,b], probs = c(0.5,0.75), na.rm=T)
  c_idx <- b-31
  Breakpoints[1,c_idx] <- q[1]
  Breakpoints[2,c_idx] <- q[2]
}
head(Breakpoints)

#Now plot histograms to check it out
quartz()
par(mfrow=c(4,4),mar=c(4,2,2,2))
for (b in 32:44){
  hist(data[,b],main="",ylab="",xlab=colnames(data[b])) 
  abline(v=Breakpoints[1,b-31],col="blue")
  abline(v=Breakpoints[2,b-31],col="blue")
}

#-----Analysis 2------
#Carry raw data distribution through to final distribution of a weighted mean, and then identify breakpoints
#Get mean across criteria
data$mean_criteria <- apply(data[,32:44],1,function(x) mean(x,na.rm=T))
quartz()
hist(data$mean_criteria,main="",xlab="Mean score",col="dark grey")
abline(v=quantile(data$mean_criteria, probs = c(0.75,0.90), na.rm=T)[1],col="black",lty=2)
abline(v=quantile(data$mean_criteria, probs = c(0.75,0.90), na.rm=T)[2],col="black",lty=2)
unique(data$Fishery[data$mean_criteria>0.4367157]) #list fisheries above 90% quantile

#----Analysis 3----
#Sensitivity analysis: find out what impact each covariate has on final number
#change weighted mean --> double weight of in variable in turn
sens_anal <- as.data.frame(matrix(NA,nrow=348,ncol=13))
for (i in 1:13){
  weights <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
  weights[i] = 2
  iter <-  apply(t(t(data[,32:44]*weights)),1,function(x) mean(x,na.rm=T))
  sens_anal[1:348,i] <- iter
}
head(sens_anal)
sens_anal$mean_criteria <- data$mean_criteria #append original mean_criteria to assess difference
colnames(sens_anal) <- c("Target_HMS","Overfishing_Fm",
                         "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                         "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA")
quartz()
par(mar=c(9,4,1,1))
barplot((round((1+(mean(data$mean_criteria) - (colMeans(sens_anal[,1:13])) / mean(data$mean_criteria)))*100)*-1),
        las=2,ylab="Relative impact")

beatr("tubular") #end the script with another banger
