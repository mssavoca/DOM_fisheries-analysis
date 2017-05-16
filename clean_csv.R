## File for cleaning NMFS Bycatch data for DOM fishery analysis 2.0
## step 1: download files
## step 2: save as csvs
## step 3: run this code for all files to clean

library(tidyr)
library(readxl)
library(data.table)

setwd("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/")
a=read.csv("DOM fishery analysis/Original CSVs/WC_FirstEditionUpdate2-2013Data_Fish_By_Fishery.csv", header = FALSE)
a=a[c(3:nrow(a)),] ##get rid of first two rows

rownames(a)=1:nrow(a)
head(a)

colnames(a)=as.character(unlist(a[1,])) # add column names
head(a)

a=a[-1,]
a$fishery=NA

a$`COMMON NAME`=as.character(a$`COMMON NAME`)

a$fishery=ifelse(a$`BYCATCH`=="",a$`COMMON NAME`,a$fishery) #where bycatch is blank, copy fishery name
#a$fishery=na.locf(a$fishery)
b=fill(a,fishery,.direction = "down")
b$fishery=as.factor(b$fishery)

b=b[-which(b$`YEAR`==""),] #changed from Scientific name to Year, because some sci names are missing (e.g., "Jellyfish (unidentified)")
head(b)

c=b[which(b$`YEAR`==""),] #changed from Scientific name to Year, because some sci names are missing (e.g., "Jellyfish (unidentified)")
c=b[b$YEAR=="",]
head(c)

#Trying to make a new column for Total bycatch by fishery
b$TOTAL_BYCATCH = apply(b[,c('BYCATCH', 'FISHERY')], 1, function(x) sum(x))

# This doesn't work
DT <- data.table(b, key = c("fishery"))
DT[, sum(BYCATCH), by = key(DT)]
head(DT)

write.csv(b,"b.csv")
