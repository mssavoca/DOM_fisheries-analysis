##########
# Batch cleaning for  CSVs downloaded from the National Oberserver Program's Bycatch Report
##########
library(data.table)
library(dplyr)


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
write.csv(MM_SB_ST_master_data_frame, "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Combined CSVs/MM_SB_ST_master_data_frame.csv")

################################################
# then clear R environment and run the following for the fish bycatch files
################################################

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

#x="AK_FirstEditionUpdate1-2010Data_Fish_By_Fishery_25-APR-2017.csv"

clean_fish_csv=function(x){
  
  a=read.csv(x,header=FALSE)
  # a=get(x)
  a=a[c(3:nrow(a)),] ##get rid of first two rows
  rownames(a)=1:nrow(a)
  colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  a=a[-1,] #removes the first row
  
  a$REGION = substr(x, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY) ## PI REGION DOES BYCATCH DIFFERENTLY, LIVE + DEAD, MESSES UP THIS CODE, took out the PI code for now 
  
  #a$BYCATCH = as.numeric(gsub(",", "", a$BYCATCH))
  #a$BYCATCH=as.numeric(a$BYCATCH)
  
  b=fill(a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery
  
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
    write.csv(a,paste0("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Cleaned CSVs/Fish bycatch/",csv)) #### change folder
  }
}
