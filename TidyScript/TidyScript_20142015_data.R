#### tidy script to clean 2014-2015 data for quarterly review
library(tidyverse)

### functions ####
clean_MM_SB_ST_csv=function(x,csvDir){
  
  a=read.csv(x,header=FALSE)
  a=a[c(3:nrow(a)),] ##get rid of first two rows
  rownames(a)=1:nrow(a)
  colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  a=a[-1,] #removes the first row
  
  name=gsub(csvDir,"",x) %>% strsplit(.,"/") %>% .[[1]] %>% .[3]
  
  a$REGION = substr(name, start = 1, stop = 2) # creates a new column REGION and populates it with the first two characters of the filename
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  
  if("BYCATCH within EEZ" %in% colnames(a) ){ ### dealing with PI data
    a=a %>% rename(`BYCATCH`=`BYCATCH within EEZ`) %>% select(-c(`BYCATCH outside EEZ`,`CV outside EEZ`)) %>% rename(`CV`=`CV within EEZ`) ## only considering bycatch within EEZ
  }
  
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
  
  c$GROUP=NA
  species=name %>% strsplit(.,"_") %>% .[[1]] %>% .[3]
  c$GROUP=species
  
  return(c)
}

### run function ####
csvDir="/Users/heatherwelch/Dropbox/Fisheries Analysis Data - FIS-NOP DOM project/2014-2015 raw data from nbr"
master=list.files(csvDir,full.names = T,pattern = ".csv",recursive = T) 
a=lapply(master,FUN = clean_MM_SB_ST_csv,csvDir = csvDir)
master=do.call("rbind",a)






