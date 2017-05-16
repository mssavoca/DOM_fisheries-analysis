setwd("where ever your csvs are")

for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  #name=gsub(".csv","",csv)
  assign(csv,r)
  print(csv)
}
rm(path,r,name,csv)
csvlist=ls() #empty

for(csv in csvlist){
  a=clean_csv(csv)
  assign(csv,a)
}

# 
# lapply(write.csv)