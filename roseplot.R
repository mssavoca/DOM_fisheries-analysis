library(tidyverse)
install.packages("IDPmisc")
library(IDPmisc)
library(cowplot)
bycatch=read.csv("/Users/heatherwelch/Desktop/SummaryData_AKCombined_June2019.csv") %>% mutate(Total_Catch=log10(Total.Bycatch..fish.and.inverts..lbs./Bycatch.Ratio)) %>% 
  select(c(X,Gear.type_general,Region,Total_Catch)) %>% group_by(Gear.type_general,Region)%>% summarise(count=n(),TC=log10(sum(Total_Catch,na.rm=T))) 

a=bycatch %>% group_by(Region)%>% mutate(w=cumsum(count)) %>% mutate(wm=w-count) %>% mutate(wt=wm+(w-wm)/2)

ggplot(data=bycatch,aes(x=Gear.type_general,y=count,fill=Gear.type_general))+
  geom_bar(stat="identity")+facet_wrap(~Region,scales = "free")#+ coord_polar()

x=a %>% filter(Region=="AK")
p <- ggplot(x, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w,
                          ymax = TC, fill = Gear.type_general))+facet_wrap(~Region)+coord_polar()+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+
  ylim(0,2.62)+ theme(axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank())+
  scale_fill_manual("",values=c("combined gears"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pots and traps"="#59663e","purse seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
AK=p1

x=a %>% filter(Region=="NE")
p <- ggplot(x, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w,
                        ymax = TC, fill = Gear.type_general))+facet_wrap(~Region)+coord_polar()+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+
  ylim(0,2.62)+ theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())+
  scale_fill_manual("",values=c("combined gears"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pots and traps"="#59663e","purse seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
NE=p1


x=a %>% filter(Region=="PI")
p <- ggplot(x, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w,
                        ymax = TC, fill = Gear.type_general))+facet_wrap(~Region)+coord_polar()+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+
  ylim(0,2.62)+ theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())+
  scale_fill_manual("",values=c("combined gears"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pots and traps"="#59663e","purse seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
PI=p1


x=a %>% filter(Region=="SE")
p <- ggplot(x, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w,
                        ymax = TC, fill = Gear.type_general))+facet_wrap(~Region)+coord_polar()+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+
  ylim(0,2.62)+ theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())+
  scale_fill_manual("",values=c("combined gears"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pots and traps"="#59663e","purse seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
SE=p1


x=a %>% filter(Region=="WC")
p <- ggplot(x, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w,
                        ymax = TC, fill = Gear.type_general))+facet_wrap(~Region)+coord_polar()+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+
  ylim(0,2.62)+ theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())+
  scale_fill_manual("",values=c("combined gears"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pots and traps"="#59663e","purse seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
WC=p1


png("test.png",width=36,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(AK,WC,SE,PI,NE,nrow = 2,ncol = 3)
dev.off()
