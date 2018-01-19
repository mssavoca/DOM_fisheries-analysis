################
#Script for summarizing and visualizing fisheries bycatch data
################
library(tidyr)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)

#create function for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

#read in sea turtle and seabird bycatch data
MMSBST_master10_13 = read.csv("MM_SB_ST_master_data_frame2010_2013.csv")

# filter master database for 2010-2013 data, only seabirds and sea turtles, and only where gear type is known
MMSBST_master10_13 = filter(MMSBST_master10_13, YEAR %in% c("2010" , "2011" , "2012" , "2013") & 
                              !GROUP == "marine mammal" &
                              !FISHERY.TYPE == "")

# makes a column turning bycatch levels into three discrete categories 
MMSBST_master10_13$Bycatch_level <- ifelse(MMSBST_master10_13$TOTAL.FISHERY.BYCATCH > 1000,"high (>1000)", 
                      ifelse(MMSBST_master10_13$TOTAL.FISHERY.BYCATCH > 50 & MMSBST_master10_13$TOTAL.FISHERY.BYCATCH < 1000, "moderate (50-1000)", "low (<50)"))

View(MMSBST_master10_13)


#nicer colors
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Change label to ordered
MMSBST_master10_13$Bycatch_level <- ordered(MMSBST_master10_13$Bycatch_level, c("low (<50)", "moderate (50-1000)", "high (>1000)"))


SBST <- ggplot(MMSBST_master10_13, aes(Bycatch_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of seabirds and sea turtles") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=cbPalette) +
  #facet_wrap(~YEAR) + 
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12))
SBST
