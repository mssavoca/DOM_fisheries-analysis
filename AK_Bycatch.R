#Script for examining a subset of the fisheries bycatch data: Fish bycatch by fishery, Alaska region, 2010-2013

library(tidyr)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)

#create function for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Set working directory and read in data
setwd("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/")
orig_csv_fish = read.csv("DOM fishery analysis/Combined CSVs/Fish bycatch_Alaska region_2010_2013.csv")
orig_csv_mm = read.csv("DOM fishery analysis/Combined CSVs/MM bycatch_Alaska region_2010_2013.csv")
  orig_csv_mm$TOTAL.FISHERY.BYCATCH = as.factor(orig_csv_mm$TOTAL.FISHERY.BYCATCH)
orig_csv_sb = read.csv("DOM fishery analysis/Combined CSVs/Seabird bycatch_Alaska region_2010_2013.csv")

orig_csv_combined <- rbind(orig_csv_fish, orig_csv_mm, orig_csv_sb) #combine the CSVs on top of each other into one
head(orig_csv_combined)

# removing the commas and changing values to numbers instead of factors
orig_csv_combined$TOTAL.FISHERY.BYCATCH = as.numeric(gsub(",", "", orig_csv_combined$TOTAL.FISHERY.BYCATCH)) 
orig_csv_combined$BYCATCH = as.numeric(gsub(",", "", orig_csv_combined$BYCATCH))
orig_csv_combined$TOTAL.FISHERY.LANDINGS..lbs. = as.numeric(gsub(",", "", orig_csv_combined$TOTAL.FISHERY.LANDINGS..lbs.))
orig_csv_combined$TOTAL.CATCH..Bycatch...Landings. = as.numeric(gsub(",", "", orig_csv_combined$TOTAL.CATCH..Bycatch...Landings.))
orig_csv_combined$CV = as.numeric(gsub(",", "", orig_csv_combined$CV))
str(orig_csv_combined)

#change YEAR to factor and reorder it chronologically
orig_csv_combined$YEAR = as.factor(orig_csv_combined$YEAR)
orig_csv_combined$YEAR <- ordered(orig_csv_combined$YEAR, levels = c("2007-2010", "2007-2011", "2008-2012", 
                                                                     "2009-2013", "2010", "2011", "2012", "2013"))
#orig_csv_combined$YEAR <- relevel(orig_csv_combined$YEAR, ref = c("2010","2011","2012","2013"))

# remove fishery summary rows since I already have columns with those values
trim_csv = orig_csv_combined[!(is.na(orig_csv_combined$YEAR) | orig_csv_combined$YEAR==""), ] 
#head(trim_csv)


##########################
# Creating fish bycatch tables and plots
##########################

# Summary info for fish by year and fishery
d2 <- trim_csv %>% 
  filter(GROUP == "fish") %>%
  group_by(FISHERY, YEAR) %>%
  summarise(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH),
            Total_Landings = mean(TOTAL.FISHERY.LANDINGS..lbs.),
            Bycatch_Ratio = mean(FISHERY.BYCATCH.RATIO..Bycatch.Total.Catch.))
View(d2)


# plot all info on Total Bycatch by Fishery and Year stacked 
fish_by_yr <- ggplot(d2, aes(x=FISHERY, y=log10(Total_Bycatch), fill=YEAR)) + 
  geom_bar(stat="identity", position="stack") +
  ggtitle("Fish Bycatch AK Region") +
  ylab("Log total bycatch (lbs)") +
  coord_flip()
fish_by_yr

## Save
ggsave(filename = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Preliminary figures/AK_FishLogTotalByactch.pdf", 
       plot = fish_by_yr,
       width= 300,
       height=200,
       dpi = 300,
       units = "mm")

#removing only fisheries one year of data with plot 
d3 <- filter(d2, YEAR != "2013") #removing 2013 since fisheries that have 2013 data have that year only

#d3 <- d3[-which(d3$FISHERY== c("Gulf of Alaska Pacific Cod Jig",   # doesn't work, trying to remove fisheries where there's only one year of data
"Gulf of Alaska Flatfish (Deepwater Flatfish) Trawl", 
"Arrowtooth flounder - Bering Sea / Aleutian Islands")),]            

fish_by_yr2 <- ggplot(d3, aes(x=FISHERY, y=Total_Bycatch, fill=YEAR)) + 
  geom_bar(stat="identity", position="dodge")+
  coord_flip() +
  scale_y_log10()
fish_by_yr2

# plot all info on Bycatch Ratio by Fishery and Year dodged 
fish_br_yr <- ggplot(d2, aes(x=FISHERY, y=Bycatch_Ratio, fill=YEAR)) + 
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Fish Bycatch Ratios AK Region") +
  coord_flip()
fish_br_yr

## Save
ggsave(filename = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Preliminary figures/AK_FishByactchRatio.pdf", 
       plot = fish_br_yr,
       width= 300,
       height=200,
       dpi = 300,
       units = "mm")


# plot all info on Total Landings by Fishery and Year stacked 
fish_l_yr <- ggplot(d2, aes(x=FISHERY, y=log10(Total_Landings), fill=YEAR)) + 
  geom_bar(stat="identity", position="stack") +
  ggtitle("Fishery Landings AK Region") +
  ylab("Total landings (lbs)") +
  coord_flip()
fish_l_yr

## Save
ggsave(filename = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Preliminary figures/AK_FishLogLandings.pdf", 
       plot = fish_l_yr,
       width= 300,
       height=200,
       dpi = 300,
       units = "mm")


##########################
# Creating marine mammal bycatch tables and plots
##########################

# Summary info for marine mammal by year and fishery
d_mm <- trim_csv %>% 
  filter(GROUP == "marine mammal") %>%
  group_by(FISHERY, YEAR) %>%
  summarise(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH))
View(d_mm)

# plot all info on Total Bycatch by Fishery and Year stacked 
mm_by_yr <- ggplot(d_mm, aes(x=FISHERY, y=Total_Bycatch, fill=YEAR)) + 
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Marine Mammal Bycatch AK Region") +
  ylab("Total bycatch (individuals)") +
  theme(axis.text.y  = element_text(size=14)) +
  coord_flip()
mm_by_yr

## Save
ggsave(filename = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Preliminary figures/AK_MMTotalByactch.pdf", 
       plot = mm_by_yr,
       width= 300,
       height=200,
       dpi = 300,
       units = "mm")

##########################
# Creating seabird bycatch tables and plots
##########################

# Summary info for marine mammal by year and fishery
d_sb <- trim_csv %>% 
  filter(GROUP == "seabird") %>%
  group_by(FISHERY, YEAR) %>%
  summarise(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH))
View(d_sb)

# plot all info on Total Bycatch by Fishery and Year stacked 
sb_by_yr <- ggplot(d_sb, aes(x=FISHERY, y=Total_Bycatch, fill=YEAR)) + 
  geom_bar(stat="identity", position="stack") +
  ggtitle("Seabird Bycatch AK Region") +
  ylab("Total bycatch (individuals)") +
  theme(axis.text.y  = element_text(size=14)) +
  coord_flip()
sb_by_yr

## Save
ggsave(filename = "/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/Preliminary figures/AK_SBTotalByactch.pdf", 
       plot = sb_by_yr,
       width= 350,
       height=200, 
       dpi = 300,
       units = "mm")
