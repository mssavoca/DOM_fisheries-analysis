

a=read.csv("NE_FirstEditionUpdate1-2010Data_Fish_By_Fishery_15-MAY-2017.csv",header=FALSE)
a=a[c(3:nrow(a)),] ##get rid of first two rows
rownames(a)=1:nrow(a)
colnames(a)=as.character(unlist(a[1,]))
a=a[-1,]

a$FISHERY=NA
a$`COMMON NAME`=as.character(a$`COMMON NAME`)
a$FISHERY=ifelse(a$`BYCATCH`==""& a$YEAR=="",a$`COMMON NAME`,a$FISHERY)

#a$BYCATCH = as.numeric(gsub(",", "", a$BYCATCH))
#a$BYCATCH=as.numeric(a$BYCATCH)

b=fill(a,FISHERY,.direction = "down")
b$TOTAL.FISHERY.BYCATCH=NA
b$TOTAL.FISHERY.BYCATCH=as.numeric(b$TOTAL.FISHERY.BYCATCH)
b$TOTAL.FISHERY.LANDINGS=NA
b$TOTAL.CATCH=NA
b$FISHERY.BYCATCH.RATIO=NA

#b$BYCATCH=as.numeric(b$BYCATCH)
b=b[-which(b$`YEAR`=="" & b$`BYCATCH`==''),]

b$BYCATCH = as.numeric(gsub(",", "", b$BYCATCH))

b$TOTAL.FISHERY.BYCATCH=ifelse(b$YEAR=="" & b$`COMMON NAME`=="TOTAL FISHERY BYCATCH",b$`BYCATCH`,b$TOTAL.FISHERY.BYCATCH)

c=fill(b,TOTAL.FISHERY.BYCATCH,.direction = "up")

###write code for other four columns

c=c[-which(c$`COMMON NAME`=="TOTAL FISHERY BYCATCH"),]


