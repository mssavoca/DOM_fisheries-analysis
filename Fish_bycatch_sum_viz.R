################
#Script for summarizing and visualizing fisheries bycatch data
################
library(tidyr)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)

#create function for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

#read in fish and invertebrate bycatch data
fish_invert_master = read.csv("Fish_master_data_frame_DOM_analysis.csv")
fish_invert_master$BYCATCH = as.numeric(gsub(",", "", fish_invert_master$BYCATCH))
fish_invert_master$TOTAL.FISHERY.BYCATCH = as.numeric(gsub(",", "", fish_invert_master$TOTAL.FISHERY.BYCATCH))
fish_invert_master$TOTAL.FISHERY.LANDINGS = as.numeric(gsub(",", "", fish_invert_master$TOTAL.FISHERY.LANDINGS))

#creating a summary table
d1 <- fish_invert_master %>%
  filter(UNIT == "POUND") %>% #removes fisheries where the bycatch is by individual
  group_by(FISHERY, YEAR, FISHERY.TYPE, REGION)%>%
  summarize(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH),
            Total_Landings = mean(TOTAL.FISHERY.LANDINGS),
            Total_Catch = mean(TOTAL.CATCH),
            Bycatch_Ratio = mean(FISHERY.BYCATCH.RATIO)) %>%
  arrange(desc(FISHERY))
View(d1)

#visualizing data distributions 

#histogram for bycatch ratio by year
ggplot(data=d1, aes(Bycatch_Ratio)) + 
  geom_histogram(binwidth = 0.01)+
  geom_density(col=2) + 
  facet_wrap(~YEAR) +
  ggtitle("Histogram for Bycatch Ratio by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Bycatch Ratio", y="Count")


#histogram for landings by year
ggplot(data=d1, aes(Total_Landings, y = ..density..)) + 
  geom_histogram()+
  geom_density(col=2) + 
  facet_wrap(~YEAR) +
  xlim(0,5e+08) +
  ylim(0,1.5e-08) +
  ggtitle("Histogram for Landings by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Landings", y="Density")

#average by fishing type
d2 <- d1 %>%
  group_by(FISHERY.TYPE) %>%
  summarize(Ave_Bycatch_Ratio = mean(Bycatch_Ratio),
            SE = SE(Bycatch_Ratio)) %>%
  arrange(desc(Ave_Bycatch_Ratio))
View(d2)

#average by region
d3 <- d1 %>%
  group_by(REGION) %>%
  summarize(Ave_Bycatch_Ratio = mean(Bycatch_Ratio),
            SE = SE(Bycatch_Ratio)) %>%
  arrange(desc(Ave_Bycatch_Ratio))
View(d3)

#average by year
d4 <- d1 %>%
  group_by(YEAR) %>%
  summarize(Ave_Bycatch_Ratio = mean(Bycatch_Ratio),
            SE = SE(Bycatch_Ratio)) %>%
  arrange(desc(Ave_Bycatch_Ratio))
View(d4)

#average by fishery
d5 <- d1 %>%
  group_by(FISHERY) %>%
  summarize(Ave_Bycatch_Ratio = mean(Bycatch_Ratio),
            SE = SE(Bycatch_Ratio)) %>%
  arrange(desc(Ave_Bycatch_Ratio))
View(d5)

#average landings by fishery
d6 <- d1 %>%
  group_by(FISHERY) %>%
  summarize(Ave_Landings = mean(Total_Landings),
            SE = SE(Total_Landings)) %>%
  arrange(desc(FISHERY))
View(d6)
