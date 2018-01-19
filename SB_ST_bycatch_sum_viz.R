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

# creating a summary table
d_MMSBST <- MMSBST_master10_13 %>%
  group_by(FISHERY, YEAR, FISHERY.TYPE, REGION, MMPA.Category)%>%
  summarize(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH),
            Total_Landings = mean(TOTAL.FISHERY.LANDINGS),
            Total_Catch = mean(TOTAL.CATCH),
            Bycatch_Ratio = mean(FISHERY.BYCATCH.RATIO)) %>%
  arrange(desc(FISHERY))

# makes a column turning bycatch levels into three discrete categories 
d1$BR_level <- ifelse(d1$Bycatch_Ratio > 0.5,"high (>0.5)", 
                      ifelse(d1$Bycatch_Ratio > 0.2 & d1$Bycatch_Ratio < 0.5, "moderate (0.2-0.5)", "low (<0.2)"))

View(d1)
