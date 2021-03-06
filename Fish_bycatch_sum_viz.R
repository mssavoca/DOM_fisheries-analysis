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

#read in fish and invertebrate bycatch data
fish_invert_master = read.csv("Fish_master_data_frame_DOM_analysis.csv")
fish_invert_master$BYCATCH = as.numeric(gsub(",", "", fish_invert_master$BYCATCH))
fish_invert_master$TOTAL.FISHERY.BYCATCH = as.numeric(gsub(",", "", fish_invert_master$TOTAL.FISHERY.BYCATCH))
fish_invert_master$TOTAL.FISHERY.LANDINGS = as.numeric(gsub(",", "", fish_invert_master$TOTAL.FISHERY.LANDINGS))

# makes a column turning bycatch levels into three discrete categories 
fish_invert_master$BR_level <- ifelse(fish_invert_master$FISHERY.BYCATCH.RATIO > 0.5,"high", 
                                      ifelse(fish_invert_master$FISHERY.BYCATCH.RATIO > 0.2 & fish_invert_master$FISHERY.BYCATCH.RATIO < 0.5, "moderate", "low"))

# creating a summary table
d1 <- fish_invert_master %>%
  filter(UNIT == "POUND") %>% #removes fisheries where the bycatch is by individual
  group_by(FISHERY, YEAR, FISHERY.TYPE, REGION, MMPA.Category)%>%
  summarize(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH),
            Total_Landings = mean(TOTAL.FISHERY.LANDINGS),
            Total_Catch = mean(TOTAL.CATCH),
            Bycatch_Ratio = mean(FISHERY.BYCATCH.RATIO)) %>%
  arrange(desc(FISHERY))

# makes a column turning bycatch levels into three discrete categories 
d1$BR_level <- ifelse(d1$Bycatch_Ratio > 0.5,"high (>0.5)", 
                                      ifelse(d1$Bycatch_Ratio > 0.2 & d1$Bycatch_Ratio < 0.5, "moderate (0.2-0.5)", "low (<0.2)"))

#View(d1)

#######
# bycatch ratio categories by gear type
#######

## Change label to ordered
d1$BR_level <- ordered(d1$BR_level, c("low (<0.2)", "moderate (0.2-0.5)", "high (>0.5)"))

#remove fisheries where we dont know gear type
d1_cut <- d1[!(d1$FISHERY.TYPE==""), ]

#nicer colors
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


br_gear <- ggplot(d1_cut, aes(BR_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Bycatch ratio of fish and invertebrates") +
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
br_gear

#save output
ggsave("Preliminary figures/BR levels by gear type.pdf", br_gear)


#######
# MMPA categories by gear type
#######

#remove fisheries where we dont know gear type
d1_cut_MMPA <- d1_cut[!(d1_cut$MMPA.Category=="ND"), ]

## Change label to ordered
d1_cut_MMPA$MMPA.Category <- ordered(d1_cut_MMPA$MMPA.Category, c("III", "II", "I"))

MMPA <- ggplot(d1_cut_MMPA, aes(MMPA.Category)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Marine Mammal Protection Act Category") +
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
MMPA

#save output
ggsave("Preliminary figures/MMPA categories by gear type.pdf", MMPA)


# View each record in by bycatch ratio in descending order
d_BR <- d1 %>% arrange(desc(Bycatch_Ratio))
View(d_BR)

pie_highBR <- ggplot(d_BR, aes(FISHERY, color=FISHERY)) +
              geom_histogram(fill=FISHERY)

# creating a summary tables
d_byfish <- fish_invert_master %>%
  filter(UNIT == "POUND") %>% #removes fisheries where the bycatch is by individual
  group_by(FISHERY)%>%
  summarize(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH),
            Total_Landings = mean(TOTAL.FISHERY.LANDINGS),
            Total_Catch = mean(TOTAL.CATCH),
            Bycatch_Ratio = mean(FISHERY.BYCATCH.RATIO),
            SE_BR = SE(FISHERY.BYCATCH.RATIO)) %>%
  arrange(desc(FISHERY))
View(d_byfish)


#############
#visualizing data distributions with histograms
#############

#histogram for bycatch ratio by year
ggplot(data=d1, aes(Bycatch_Ratio)) + 
  geom_histogram(binwidth = 0.01) +
  geom_density(col=2) + 
  facet_wrap(~YEAR) +
  ggtitle("Histogram for Bycatch Ratio by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Bycatch Ratio", y="Count")


#histogram for landings by year
ggplot(data=d1, aes(Total_Landings, ..count..)) + 
  geom_histogram() +
  geom_density(aes(y = ..count..), col=2) + 
  facet_wrap(~YEAR) +
  xlim(0,5e+08) +
  #ylim(0,1.5e-08) +
  ggtitle("Histogram for Landings by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Landings", y="Count")

#histogram for MMPA category rankings by fishery
d_MMPA <- filter(d1, MMPA.Category != "ND")
ggplot(d_MMPA, aes(MMPA.Category)) +
  geom_bar() +
  facet_wrap(~YEAR) +
  ggtitle("Histogram of MMPA Category ranking") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="MMPA Category", y="Count")

#histogram for ESA listed species caught by year
threat.list = read.csv(file = "Listing_master_data_frame_DOM_analysis.csv")
ggplot(data=threat.list, aes(ESA.Listed.Sp)) + 
  geom_histogram()+
  geom_density(aes(y = ..count..), col=2) + 
  facet_wrap(~Year) +
  ggtitle("Histogram for ESA listed species bycaught by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="ESA listed species bycaught", y="Count")

#histogram for ESA listed lbs fish caught by year
threat.list = read.csv(file = "Listing_master_data_frame_DOM_analysis.csv")
ggplot(data=threat.list, aes(ESA.Listed.fish.lbs)) + 
  geom_histogram()+
  geom_density(aes(y = ..count..), col=2) + 
  facet_wrap(~Year) +
  ggtitle("Histogram for ESA listed fish (lbs.) bycaught by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="ESA listed fish bycaught (lbs.)", y="Count")

#histogram for IUCN listed species caught by year
IUCN.threat.list = read.csv(file = "IUCN listing_master_data_frame_DOM_analysis.csv")
ggplot(data=IUCN.threat.list, aes(IUCN.Listed.Sp)) + 
  geom_histogram(aes(y = ..count..))+
  geom_density(aes(y = ..count..), col=2) + 
  facet_wrap(~Year) +
  ggtitle("Histogram for IUCN NT and above bycaught by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="IUCN listed species bycaught", y="Count")

#histogram for IUCN listed fish (lbs.) caught by year
IUCN.threat.list = read.csv(file = "IUCN listing_master_data_frame_DOM_analysis.csv")
ggplot(data=IUCN.threat.list, aes(IUCN.Listed.fish.lbs)) + 
  geom_histogram(aes(y = ..count..))+
  geom_density(aes(y = ..count..), col=2) + 
  facet_wrap(~Year) +
  ggtitle("Histogram for fish IUCN NT and above bycaught (lbs.) by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="IUCN listed fish bycaught (lbs.)", y="Count")


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
  arrange(desc(Ave_Landings))
View(d6)


