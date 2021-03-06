library(tidyr)
library(readxl)
library(data.table)
library(gdata)
library(xlsx)
installXLSXsupport()


read.xls("AK_SecondEdition-2014Data_Fish_By_Fishery", sheet=1, verbose=FALSE, blank.lines.skip=TRUE, perl="perl")


setwd("")

clean_MM_SB_ST_csv=function(x){

a=get(x)
a=a[c(3:nrow(a)),] ##get rid of first two rows
rownames(a)=1:nrow(a)
colnames(a)=as.character(unlist(a[1,])) # makes first row the header
a=a[-1,] #removes the first row

a$FISHERY=NA
a$`COMMON NAME`=as.character(a$`COMMON NAME`)
a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY)

b=fill(a,FISHERY,.direction = "down") #fill in the fishery name; somehow knows to stop and restart with each new fishery

#adding summary columns to be filled in
b$TOTAL.FISHERY.BYCATCH=NA
b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)

b=b[-which(b$`YEAR`=="" & b$`BYCATCH`==''),] #remove rows where year and bycatch are blank

b$BYCATCH = as.numeric(gsub(",", "", b$BYCATCH)) #removes commas from the values in the bycatch column

b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="Fishery Total",b$`BYCATCH`,b$TOTAL.FISHERY.BYCATCH) # if year is blank and common name is "Fishery Total", then fill in value of bycatch in "TOTAL FISHERY BYCATCH" column

c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up") #fill that value up in the dataframe

c=c[-which(c$`COMMON NAME`=="Fishery Total" | c$`COMMON NAME`=="Grand Total"),] #removes the summary rows

return(f)
}


clean_fish_csv=function(x){
  
  a=get("AK_SecondEdition-2014Data_Fish_By_Fishery_13-OCT-2017.csv")
  a=a[c(2:nrow(a)),] ##get rid of first two rows
  rownames(a)=1:nrow(a)
  colnames(a)=as.character(unlist(a[1,])) # makes first row the header
  a=a[-1,] #removes the first row
  
  a$FISHERY=NA
  a$`COMMON NAME`=as.character(a$`COMMON NAME`)
  a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY)
  
  a$BYCATCH = as.numeric(gsub(",", "", a$BYCATCH)) #takes out the commas and makes changes the character type from factor to numeric
  a$BYCATCH=as.numeric(a$BYCATCH)
  
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
  


for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
  
    a=clean_fish_csv(csv)
    assign(csv,a)
}

rm(path,r,name,csv)
csvlist=ls() #empty

for(csv in csvlist){
  a=clean_csv(csv)
  assign(csv,a)
}

# 
# lapply(write.csv)