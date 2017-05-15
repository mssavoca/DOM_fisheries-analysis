setwd("where ever your csvs are")

for (csv in list.files(pattern="*.csv$",recursive = TRUE)){
  path=paste(getwd(),"/",csv,sep="")
  r=read.csv(path)
  name=gsub(".csv","",csv)
  assign(name,r)
  print(csv)
}