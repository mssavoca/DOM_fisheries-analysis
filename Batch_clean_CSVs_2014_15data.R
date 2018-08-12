##########
# Batch cleaning for  CSVs downloaded from the National Oberserver Program's Bycatch Report
##########
library(data.table)
library(dplyr)
library(tidyr)


################
# code to clean and combine the marine mammal, seabird, and sea turtle csvs
################

#first read in all CSVs and put them in an R list object
for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  #name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
}
rm(path,r,name,csv)
#csvlist=ls() #empty
csvlist=list.files(pattern="*.csv$")  ## alternative way to grab list

x = "PI_FirstEditionUpdate1-2010Data_MarineMammal_By_Fishery_23-MAY-2017.csv"

# function to clean all the marine mammal, seabird and sea turtles csvs
#setwd("") do this through Files --> settings wheel --> Set as Working Directory
clean_MM_SB_ST_csv=function(x){
  
  a=read.csv(x,header=FALSE)
  # a=get(x)
  a=a[c(3:nrow(a)),] ##get rid of first two rows
  rownames(a)=1:nrow(a)
  colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  a=a[-1,] #removes the first row
  
  a$REGION = substr(x, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY) # if BYCATCH & YEAR are blank, the 
  
  b=fill(a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery
  
  #adding summary columns to be filled in
  b$TOTAL.FISHERY.BYCATCH=NA
  b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)
  
  b=b[-which(b$`YEAR`=="" & b$`BYCATCH`==''),] #remove rows where year and bycatch are blank
  
  b$BYCATCH = as.numeric(gsub(",", "", b$BYCATCH)) #removes commas from the values in the bycatch column

  b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="Fishery Total",b$`BYCATCH`,b$TOTAL.FISHERY.BYCATCH) # if year is blank and common name is "Fishery Total", then fill in value of bycatch in "TOTAL FISHERY BYCATCH" column
  
  c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up") #fill that value up in the dataframe
  
  c=c[-which(c$`COMMON NAME`=="Fishery Total" | c$`COMMON NAME`=="Grand Total"),] #removes the summary rows
  
  return(c)
}


# for(csv in csvlist){
#   a=clean_MM_SB_ST_csv(csv)
#   assign(csv,a)
#   #write.csv(a,paste0(folder,"/",csv,".csv")) #### change folder
# }

# for loop that goes through folder and cleans each csv using above function 
for(csv in csvlist){
  if(!grepl(pattern = "PI_",csv)){  ## run for all csvs that are not fish
    print(csv)
    a=clean_MM_SB_ST_csv(csv)
    assign(csv,a)
    write.csv(a,paste0("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_SB_ST bycatch/",csv)) #### change folder
  }
}

## Read in and combine CSVs into one large data frame
load_data <- function(path) { 
  files <- dir(path = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_SB_ST bycatch/",
               pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  tables <- lapply(tables, function(df) mutate_at(df, .cols = c("YEAR"), as.factor)) #changes the YEAR column so that it's always a factor; prevents NAs
  do.call(rbind, tables)
}

# run the function above, loading in and combining all the cleaned data frames
MM_SB_ST_master_data_frame <- load_data("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_SB_ST bycatch/")

#output cleaned and combined data frame to csv
write.csv(MM_SB_ST_master_data_frame, "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Combined CSVs/MM_SB_ST_master_data_frame2.csv")

################
# code to clean and combine all the marine mammal csvs from the PI region where they have bycatch inside and outside the US EEZ
################
#first read in all CSVs and put them in an R list object
for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  #name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
}
rm(path,r,name,csv)
#csvlist=ls() #empty
csvlist=list.files(pattern="*.csv$")  ## alternative way to grab list

clean_MM_PI_csv=function(x){
  
  a=read.csv(x,header=FALSE)
  # a=get(x)
  a=a[c(3:nrow(a)),] ##get rid of first two rows
  rownames(a)=1:nrow(a)
  colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  a=a[-1,] #removes the first row
  
  a$REGION = substr(x, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH within EEZ`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY) # if BYCATCH & YEAR are blank, then take the text in the common name column and paste it in new FISHERY column 
  
  b=fill(a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery
  
  #adding summary columns to be filled in
  b$TOTAL.FISHERY.BYCATCH=NA
  b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)
  
  b=b[-which(b$`YEAR`=="" & b$`BYCATCH within EEZ`=='' & b$`BYCATCH outside EEZ`==''),] #remove rows where year and bycatch are blank
  
  b$`BYCATCH within EEZ` = as.numeric(gsub(",", "", b$`BYCATCH within EEZ`)) #removes commas from the values in the bycatch column
  b$`BYCATCH outside EEZ` = as.numeric(gsub(",", "", b$`BYCATCH outside EEZ`)) #removes commas from the values in the bycatch column
  
  b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="Fishery Total",b$`BYCATCH within EEZ`+b$`BYCATCH outside EEZ`,b$TOTAL.FISHERY.BYCATCH) # if year is blank and common name is "Fishery Total", then fill in value of bycatch in "TOTAL FISHERY BYCATCH" column
  
  c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up") #fill that value up in the dataframe
  
  c=c[-which(c$`COMMON NAME`=="Fishery Total" | c$`COMMON NAME`=="Grand Total"),] #removes the summary rows
  
  return(c)
}
# for loop that goes through folder and cleans each csv using above function 
for(csv in csvlist){
  if(grepl(pattern = "PI_",csv)){  ## run for all csvs that are not fish
    print(csv)
    a=clean_MM_PI_csv(csv)
    assign(csv,a)
    write.csv(a,paste0("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_bycatch_PI region/",csv)) #### change folder
  }
}

## Read in and combine CSVs into one large data frame
load_data <- function(path) { 
  files <- dir(path = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_bycatch_PI region/",
               pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  tables <- lapply(tables, function(df) mutate_at(df, .cols = c("YEAR"), as.factor)) #changes the YEAR column so that it's always a factor; prevents NAs
  do.call(rbind, tables)
}

# run the function above, loading in and combining all the cleaned data frames
MM_PI_master_data_frame <- load_data("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/MM_SB_ST bycatch/")

#output cleaned and combined data frame to csv
write.csv(MM_PI_master_data_frame, "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Combined CSVs/MM_PI_master_data_frame.csv")



################################################
# then clear R environment and run the following for the fish bycatch files
################################################

################
# code to clean and combine the fish and invertebrate csvs
################


#setwd("") do this through Files --> settings wheel --> Set as Working Directory

#first read in all CSVs and put them in an R list object
for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  #name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
}
# rm(path,r,name,csv)
csvlist=ls() #empty
# csvlist=list.files(pattern="*.csv$")  ## alternative way to grab list where you dont have to remove history 
# 
# #setwd("") do this through Files --> settings wheel --> Set as Working Directory
# 
 a=`NE_SecondEdition_2015Data_Fish_By_Fishery_27-FEB-2018.csv`
# for(i in 1:ncol(a)){  #----------> columns needed to be converted from factors to character
#   a[,i]=as.character(a[,i])
# }

clean_fish_csv=function(x){
  
  a=read.csv(x,header=T)
  
  for(i in 1:ncol(a)){ #------> once you've debugged, put it here
    a[,i]=as.character(a[,i])
  }
  # 
  #a=get(x)
      #colnames(a) <- c("COMMON NAME","SCIENTIFIC NAME","YEAR","BYCATCH","UNIT","CV","FOOTNOTES","NA","NA","NA","NA")

  colnames(a)=a[1,] #make the first row the column names
  
  a=a[c(2:nrow(a)),] ##get rid of first row
  #rownames(a)=1:nrow(a)
  #colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  #colnames(a) <- rep("t",11)
  #a=a[-1,] #removes the first row
  
  a$REGION = substr(x, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY) ## PI REGION DOES BYCATCH DIFFERENTLY, LIVE + DEAD, MESSES UP THIS CODE, took out the PI code for now 
  
  a$FISHERY=as.factor(a$FISHERY)
  a$FISHERY=gsub("##","",a$FISHERY)
  a$FISHERY=gsub("%%","",a$FISHERY)
  a$FISHERY=gsub("<","",a$FISHERY)
  a$FISHERY=gsub("~~","",a$FISHERY)
  a$FISHERY=gsub(" $$","",a$FISHERY)
  a$FISHERY=gsub("&","",a$FISHERY)
  a$FISHERY=gsub("%","",a$FISHERY)
  a$FISHERY=gsub("^","",a$FISHERY)
  a$FISHERY=gsub("~","",a$FISHERY)
  a$FISHERY=gsub("#","",a$FISHERY)
  a$FISHERY=gsub("+","",a$FISHERY)
  
  a$FISHERY=as.factor(a$FISHERY)
  #a=a[,c(1:7,12,13)]
  
  #a$BYCATCH = as.numeric(gsub(",", "", a$BYCATCH))
  #a$BYCATCH=as.numeric(a$BYCATCH)
  
  b=fill (a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery
  
  #adding summary columns to be filled in
  b$TOTAL.FISHERY.BYCATCH=NA
  b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)
  b$TOTAL.FISHERY.LANDINGS=NA
  b$TOTAL.CATCH=NA
  b$FISHERY.BYCATCH.RATIO=NA
  
  #b$BYCATCH=as.numeric(b$BYCATCH)
  b=b[-which(b$`YEAR`=="" & b$`BYCATCH`==''),] #remove rows where year and bycatch are blank
  
  b$BYCATCH = as.numeric(gsub(",", "", b$BYCATCH)) #removes commas from the values in the bycatch column
  
  b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="TOTAL FISHERY BYCATCH",b$`BYCATCH`,b$TOTAL.FISHERY.BYCATCH) # if year is blank and common name is "TOTAL FISHERY BYCATCH", then fill in value of bycatch in "TOTAL FISHERY BYCATCH" column
  
  c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up") #fill that value up in the dataframe
  
  c=c[-which(c$`COMMON NAME`=="TOTAL FISHERY BYCATCH"),] #removes the "TOTAL FISHERY BYCATCH" summary row
  
  ###write code for other four columns
  
  ## Now for TOTAL FISHERY LANDINGS column
  c$TOTAL.FISHERY.LANDINGS=ifelse(c$YEAR=="" & c$`COMMON NAME`=="TOTAL FISHERY LANDINGS",c$`BYCATCH`,c$TOTAL.FISHERY.LANDINGS) # if year is blank and common name is "TOTAL FISHERY LANDINGS", then fill in value of landings in "TOTAL FISHERY LANDINGS" column
  
  d=fill(c,TOTAL.FISHERY.LANDINGS,.direction = "up") #fill that value up in the dataframe
  
  d=d[-which(d$`COMMON NAME`=="TOTAL FISHERY LANDINGS"),] #removes the "TOTAL FISHERY LANDINGS" summary row
  
  
  ## Now for TOTAL CATCH column
  d$TOTAL.CATCH=ifelse(d$YEAR=="" & d$`COMMON NAME`=="TOTAL CATCH (Bycatch + Landings)",d$`BYCATCH`,c$TOTAL.CATCH) # if year is blank and common name is "TOTAL CATCH", then fill in value of total catch in "TOTAL CATCH" column
  
  e=fill(d,TOTAL.CATCH,.direction = "up") #fill that value up in the dataframe
  
  e=e[-which(e$`COMMON NAME`=="TOTAL CATCH (Bycatch + Landings)"),] #removes the "TOTAL FISHERY LANDINGS" summary row
  
  ## Now for BYCATCH RATIO column
  e$FISHERY.BYCATCH.RATIO=ifelse(e$YEAR=="" & e$`COMMON NAME`=="FISHERY BYCATCH RATIO (Bycatch/Total Catch)",e$`BYCATCH`,e$FISHERY.BYCATCH.RATIO) # if year is blank and common name is FISHERY BYCATCH RATIO, then fill in value of bycatch ratio in "FISHERY.BYCATCH.RATIO" column
  
  f=fill(e,FISHERY.BYCATCH.RATIO,.direction = "up") #fill that value up in the dataframe
  
  f=f[-which(f$`COMMON NAME`=="FISHERY BYCATCH RATIO (Bycatch/Total Catch)"),] #removes the "FISHERY.BYCATCH.RATIO" summary row
  
  return(f)
}

for(csv in csvlist){
  if(!grepl("PI_",csv)){  ## run for all csvs that are fish
    print(csv)
    a=clean_fish_csv(csv)
    assign(csv,a)
    write.csv(a,paste0("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch/2014-15_cleaned/",csv)) #### change folder
  }
}

write.csv(f,"NE_SecondEdition_2014Data_Fish_By_Fishery_27-FEB-2018.csv")

## Read in and combine CSVs into one large data frame
load_data <- function(path) { 
  files <- dir(path = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch/2014-15_cleaned/",
               pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  #tables <- lapply(tables, function(df) mutate_at(df, .cols = c("YEAR"), as.factor)) #changes the YEAR column so that it's always a factor; prevents NAs
  do.call(rbind, tables)
}

# run the function above, loading in and combining all the cleaned data frames
Fish_master_data_frame <- load_data("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch/")

#output cleaned and combined data frame to csv
write.csv(Fish_master_data_frame, "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Combined CSVs/Fish_master_data_frame2.csv")


################
# code to clean and combine the fish and invertebrate csvs from PI region where they separate live and dead bycatch
################


#setwd("") do this through Files --> settings wheel --> Set as Working Directory

#first read in all CSVs and put them in an R list object
for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  #name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
}
rm(path,r,name,csv)
#csvlist=ls() #empty
csvlist=list.files(pattern="*.csv$")  ## alternative way to grab list where you dont have to remove history 

#setwd("") do this through Files --> settings wheel --> Set as Working Directory

a = `PI_SecondEdition_2015Data_Fish_By_Fishery_22-FEB-2018.csv` #testing out on one dataframe if loop doesn't work


clean_fish_PI_csv=function(x){
  
  a=read.csv(x,header=T)
  
  for(i in 1:ncol(a)){ #------> once you've debugged, put it here
    a[,i]=as.character(a[,i])
  }
  # 
  #a=get(x)
  #colnames(a) <- c("COMMON NAME","SCIENTIFIC NAME","YEAR","BYCATCH","UNIT","CV","FOOTNOTES","NA","NA","NA","NA")
  
  colnames(a)=a[1,] #make the first row the column names
  
  a=a[c(2:nrow(a)),] ##get rid of first row
  #rownames(a)=1:nrow(a)
  #colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  #colnames(a) <- rep("t",11)
  #a=a[-1,] #removes the first row
  
  a$REGION = substr(x, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a=a[1:189,]
  a$FISHERY=NA
  
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH (LIVE+DEAD)`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY) ## PI REGION DOES BYCATCH DIFFERENTLY, LIVE + DEAD, MESSES UP THIS CODE, took out the PI code for now 
  a=a[,c(1:8,11,12)]
  
  b=fill(a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery
  
  #adding summary columns to be filled in
  b$TOTAL.FISHERY.BYCATCH=NA
  b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)
  b$TOTAL.FISHERY.LANDINGS=NA
  b$TOTAL.CATCH=NA
  b$FISHERY.BYCATCH.RATIO=NA
  
  #b$BYCATCH=as.numeric(b$BYCATCH)
  b=b[-which(b$`YEAR`=="" & b$`BYCATCH (LIVE+DEAD)`==''),] #remove rows where year and bycatch are blank
  
  b$`BYCATCH (LIVE+DEAD)` = as.numeric(gsub(",", "", b$`BYCATCH (LIVE+DEAD)`)) #removes commas from the values in the bycatch column
  b$`BYCATCH (DEAD)` = as.numeric(gsub(",", "", b$`BYCATCH (DEAD)`)) #removes commas from the values in the bycatch column
  
  b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="TOTAL FISHERY BYCATCH",b$`BYCATCH (LIVE+DEAD)`+b$`BYCATCH (DEAD)`,b$TOTAL.FISHERY.BYCATCH) # if year is blank and common name is "TOTAL FISHERY BYCATCH", then fill in value of bycatch in "TOTAL FISHERY BYCATCH" column
  
  c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up") #fill that value up in the dataframe
  
  c=c[-which(c$`COMMON NAME`=="TOTAL FISHERY BYCATCH"),] #removes the "TOTAL FISHERY BYCATCH" summary row
  
  ###code for other four columns
  
  ## Now for TOTAL FISHERY LANDINGS column
  c$TOTAL.FISHERY.LANDINGS=ifelse(c$YEAR=="" & c$`COMMON NAME`=="TOTAL FISHERY LANDINGS",c$`BYCATCH (LIVE+DEAD)`,c$TOTAL.FISHERY.LANDINGS) # if year is blank and common name is "TOTAL FISHERY LANDINGS", then fill in value of landings in "TOTAL FISHERY LANDINGS" column
  
  d=fill(c,TOTAL.FISHERY.LANDINGS,.direction = "up") #fill that value up in the dataframe
  
  d=d[-which(d$`COMMON NAME`=="TOTAL FISHERY LANDINGS"),] #removes the "TOTAL FISHERY LANDINGS" summary row
  
  
  ## Now for TOTAL CATCH column
  d$TOTAL.CATCH=ifelse(d$YEAR=="" & d$`COMMON NAME`=="TOTAL CATCH (Bycatch + Landings)",d$`BYCATCH (LIVE+DEAD)`,c$TOTAL.CATCH) # if year is blank and common name is "TOTAL CATCH", then fill in value of total catch in "TOTAL CATCH" column
  
  e=fill(d,TOTAL.CATCH,.direction = "up") #fill that value up in the dataframe
  
  e=e[-which(e$`COMMON NAME`=="TOTAL CATCH (Bycatch + Landings)"),] #removes the "TOTAL FISHERY LANDINGS" summary row
  
  ## Now for BYCATCH RATIO column
  e$FISHERY.BYCATCH.RATIO=ifelse(e$YEAR=="" & e$`COMMON NAME`=="FISHERY BYCATCH RATIO (Bycatch/Total Catch)",e$`BYCATCH (LIVE+DEAD)`,e$FISHERY.BYCATCH.RATIO) # if year is blank and common name is FISHERY BYCATCH RATIO, then fill in value of bycatch ratio in "FISHERY.BYCATCH.RATIO" column
  
  f=fill(e,FISHERY.BYCATCH.RATIO,.direction = "up") #fill that value up in the dataframe
  
  f=f[-which(f$`COMMON NAME`=="FISHERY BYCATCH RATIO (Bycatch/Total Catch)"),] #removes the "FISHERY.BYCATCH.RATIO" summary row
  
  return(f)
}

for(csv in csvlist){
  if(grepl("PI_",csv)){  ## run for all csvs that are fish
    print(csv)
    a=clean_fish_PI_csv(csv)
    assign(csv,a)
    write.csv(a,paste0("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch_PI region/2014_15/",csv)) #### change folder
  }
}

## Read in and combine CSVs into one large data frame
load_data <- function(path) { 
  files <- dir(path = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch_PI region/",
               pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  #tables <- lapply(tables, function(df) mutate_at(df, .cols = c("YEAR"), as.factor)) #changes the YEAR column so that it's always a factor; prevents NAs
  do.call(rbind, tables)
}

# run the function above, loading in and combining all the cleaned data frames
Fish_PI_master_data_frame <- load_data("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch_PI region/2014_15/")

#output cleaned and combined data frame to csv
write.csv(Fish_PI_master_data_frame, "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Combined CSVs/Fish_PI_master_data_frame.csv")
