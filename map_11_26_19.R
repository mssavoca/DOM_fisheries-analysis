install.packages("scatterpie")
devtools::install_github("wmurphyrd/fiftystater")


library(ggplot2)
library(mapproj)
library(fiftystater)
library(tidyverse)
library(scatterpie)

### original ####
bycatch=read.csv("/Volumes/SeaGate/Matt_Savoca_DOM_fisheries/DOM_fisheries-analysis/SummaryData_November2019_AnalysisExport_AllFisheryYears.csv") %>% select(c(X,GearType_general,Region)) %>% group_by(GearType_general,Region) %>% summarise(count=n()) %>% spread(GearType_general,count) %>% 
  mutate(x=c(-117.5,-68,-105,-74,-130),y=c(19,36,19,23,45)) 
bycatch[is.na(bycatch)]<-0


# p <- ggplot(aes(map_id = state)) + 
#   geom_map(aes(fill = state), map = fifty_states) + 
#   expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#   coord_map()
# p


crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
NE=c("maine","new hampshire","maryland","massachusetts","connecticut", "rhode island","new jersey", "new york", "west virginia","delaware","virginia")
SE=c("south carolina", "north carolina","georgia","florida","alabama","louisiana","texas","mississippi")
WC=c("washington","oregon","california")
AK=c("alaska")
PI="hawaii"

a=crimes %>% subset(.,state %in% NE) %>% mutate(Region="NE")
b=crimes %>% subset(.,state %in% SE) %>% mutate(Region="SE")
c=crimes %>% subset(.,state %in% WC) %>% mutate(Region="WC")
d=crimes %>% subset(.,state %in% AK) %>% mutate(Region="AK")
e=crimes %>% subset(.,state %in% PI) %>% mutate(Region="PI")

all=do.call("rbind",list(a,b,c,d,e))

master=left_join(crimes,all) 
master$Region[is.na(master$Region)]<-"Other"

# pie=a %>% select(c(state,Murder)) %>% spread(state,Murder) %>% mutate(x=-100,y=40)

p <- ggplot(master, aes(map_id = state)) + 
  geom_map(aes(fill=Region), map = fifty_states,show.legend = FALSE) #+scale_fill_manual("",values=c("AK"="#8da38e","NE"="#3c4d63","PI"="#4d543d","SE"="#557e83","WC"="#69494f","Other"="grey"),guide='none')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()
  # annotate("rect",xmin = -122, xmax = -113, ymin = 13.3, ymax = 23.5, alpha = .4,color="black",size=.2,fill="lightgrey")+
  # annotate("rect",xmin = -109.5, xmax = -100.5, ymin = 13.3, ymax = 23.5, alpha = .15,color="black",size=.2,fill="lightgrey")+
  # annotate("rect",xmin = -78.5, xmax = -69.5, ymin = 39.3, ymax = 49.5, alpha = .15,color="black",size=.2,fill="lightgrey")+
  # annotate("rect",xmin = -78.5, xmax = -69.5, ymin = 39.3, ymax = 49.5, alpha = .15,color="black",size=.2,fill="lightgrey")
p=p+  geom_scatterpie(aes(x=x,y=y,r = 4,),data=bycatch,cols=colnames(bycatch)[2:9],color=NA)+coord_fixed() +
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = NA,fill=NA),legend.key.size = unit(.5,'lines'))+
  scale_fill_manual(breaks=c("dredge","gillnet","line","longline","pots and traps","purse seine","trawl","troll"),
                    values=c("AK"="#8da38e","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","NE"="#3c4d63","Other"="grey","PI"="#4d543d","pots and traps"="#59663e","purse seine"="#ffca33","SE"="#557e83","trawl"="#c5703f","troll"="#4d304b","WC"="#69494f"))+
    guides(fill=guide_legend(title="Gear types"))+theme(legend.position=c(.18,.5),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=6),legend.title = element_text(size=6))+
  annotate("text",x=-117.5,y=14,label="Alaska",size=2,color="#555555")+annotate("text",x=-68,y=31,label="Northeast",size=2,color="#555555")+
  annotate("text",x=-105,y=14,label="Pacific Islands",size=2,color="#555555")+annotate("text",x=-74,y=18,label="Southeast",size=2,color="#555555")+
  annotate("text",x=-130,y=40,label="West Coast",size=2,color="#555555")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
  #ggtitle("Percentage of fisheries in each gear type in the five bycatch reporting regions")+theme(plot.title = element_text(size=6))

p

# png("map.png",width=5, height=5, units="in", res=400)
# par(ps=10)
# par(mar=c(1,1,1,1))
# par(cex=1)
# p
# dev.off()
# 
# png("map.png",width=5, height=5, units="in", res=400)
# par(ps=10)
# par(mar=c(1,1,1,1))
# par(cex=1)
# p
# dev.off()
# 
# dev.copy2pdf(p,"map.pdf",width=5, height=5)
# 
# tiff("map.tiff", height = 4, width = 4, units = 'in',  res=300)
# p
# dev.off()
# par(mfrow = c(1,1))

dev.copy2pdf(file="map.pdf", width=10, height=6)

# 
# ggplot(master,aes(x="",y=Assault,fill=Region))+geom_bar(stat="identity",width=1)+ coord_polar("y", start=0)
# ggplot()+geom_bar(master,aes(x="",y=Assault,fill=Region),stat="identity",width=1)+ coord_polar("y", start=0)
# 
# 
# ggplot()+geom_scatterpie(aes(x=x,y=y,r = 6),data=pie,cols=c("delaware","maine","new jersey"),size=20,color=NA)+ coord_fixed()

# geom_scatterpie(aes(x=-85,y=45,r = 4),data=pie,cols=c("delaware","maine","new jersey"),color=NA)+ ##NE
# geom_scatterpie(aes(x=-90,y=25,r = 4),data=pie,cols=c("delaware","maine","new jersey"),color=NA)+ ##SE
# geom_scatterpie(aes(x=-118,y=19,r = 4),data=pie,cols=c("delaware","maine","new jersey"),color=NA)+ ##AK
# geom_scatterpie(aes(x=-105,y=19,r = 4),data=pie,cols=c("delaware","maine","new jersey"),color=NA)+ ##HI
# geom_scatterpie(aes(x=-110,y=40,r = 4),data=pie,cols=c("delaware","maine","new jersey"),color=NA)+coord_fixed() ##WC

#### scalled by number of fisheries ####

bycatch=read.csv("/Users/heatherwelch/Desktop/SummaryData_AKCombined_June2019.csv") %>% select(c(X,Gear.type_general,Region)) %>% group_by(Gear.type_general,Region) %>% summarise(count=n()) %>% spread(Gear.type_general,count) %>% 
  mutate(x=c(-117.5,-68,-105,-74,-130),y=c(19,36,19,23,45))
bycatch[is.na(bycatch)]<-0
bycatch=bycatch %>% mutate(numfish=rowSums(.[2:10]))

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
NE=c("maine","new hampshire","maryland","massachusetts","connecticut", "rhode island","new jersey", "new york","delaware","virginia")
SE=c("south carolina", "north carolina","georgia","florida","alabama","louisiana","texas","mississippi")
WC=c("washington","oregon","california")
AK=c("alaska")
PI="hawaii"

a=crimes %>% subset(.,state %in% NE) %>% mutate(Region="NE")
b=crimes %>% subset(.,state %in% SE) %>% mutate(Region="SE")
c=crimes %>% subset(.,state %in% WC) %>% mutate(Region="WC")
d=crimes %>% subset(.,state %in% AK) %>% mutate(Region="AK")
e=crimes %>% subset(.,state %in% PI) %>% mutate(Region="PI")

all=do.call("rbind",list(a,b,c,d,e))

master=left_join(crimes,all) 
master$Region[is.na(master$Region)]<-"Other"

p <- ggplot(master, aes(map_id = state)) + 
  geom_map(aes(fill=Region), map = fifty_states,show.legend = FALSE) +#scale_fill_manual("",values=c("AK"="#8da38e","NE"="#3c4d63","PI"="#4d543d","SE"="#557e83","WC"="#69494f","Other"="grey"),guide='none')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()
p=p+  geom_scatterpie(aes(x=x,y=y,r = .03*numfish,),data=bycatch,cols=colnames(bycatch)[2:10],color=NA)+coord_fixed() +
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA),legend.key.size = unit(.5,'lines'))+
  scale_fill_manual(breaks=c("combined gears","dredge","gillnet","line","longline","pots and traps","purse seine","trawl","troll"),
                    values=c("#8da38e","#9f7bb2","#7dac33","#c64f79","#93ccaf","#8e97ee","#3c4d63","grey","#4d543d","#59663e","#ffca33","#557e83","#c5703f","#4d304b","#69494f"))+
  guides(fill=guide_legend(title="Gear types"))+theme(legend.position=c(.18,.5),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=6),legend.title = element_text(size=6))+
  annotate("text",x=-117.5,y=14,label="Alaska",size=2,color="#555555")+annotate("text",x=-68,y=29,label="Northeast",size=2,color="#555555")+
  annotate("text",x=-105,y=16,label="Pacific Islands",size=2,color="#555555")+annotate("text",x=-74,y=18,label="Southeast",size=2,color="#555555")+
  annotate("text",x=-130,y=40,label="Westcoast",size=2,color="#555555")+
  ggtitle("Percentage of fisheries in each gear type in the five bycatch reporting regions, scaled by the total number of fisheries per region")+theme(plot.title = element_text(size=5))

p

png("test2.png",width=5, height=5, units="in", res=400)
par(ps=10)
par(mar=c(1,1,1,1))
par(cex=1)
p
dev.off()

