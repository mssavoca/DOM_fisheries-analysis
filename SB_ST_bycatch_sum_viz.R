################
#Script for summarizing and visualizing fisheries bycatch data ----
################

# Loading packages ----
library(tidyr)
library(readxl)
library(dplyr)
library(data.table)
library(ggridges)
library(viridis)
library(tidyverse)
library(plotly)



# SE fuction
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

#read in sea turtle and seabird bycatch data ----
MMSBST_master10_15 = read.csv("MM_SB_ST_master_data_frame2010_2015.csv")

# makes a column turning bycatch levels into three discrete categories 
MMSBST_master10_15$Bycatch_level <- ifelse(MMSBST_master10_15$TOTAL.FISHERY.BYCATCH > 1000,"high (>1000)", 
                                           ifelse(MMSBST_master10_15$TOTAL.FISHERY.BYCATCH > 50 & MMSBST_master10_15$TOTAL.FISHERY.BYCATCH < 1000, "moderate (50-1000)", "low (<50)"))

#View(MMSBST_master10_15)


# creating a summary table for merging with fish/invert summary table
d_SBST <- MMSBST_master10_15 %>%
  group_by(FISHERY, YEAR, FISHERY.TYPE, REGION, GROUP, BYCATCH, Bycatch_level)%>%
  summarize(Total_Bycatch = mean(TOTAL.FISHERY.BYCATCH)) %>%
  arrange(desc(FISHERY))

#View(d_SBST)

#Attempt to grab turtle only data
d_turtles <- d_SBST[d_SBST$GROUP=="sea turtle",] 

#Trying to make a new column for Total bycatch by fishery and year

#OPTION 1
d_turtles_summary <- aggregate(d_turtles$BYCATCH, by=list(d_turtles$FISHERY, d_turtles$YEAR), FUN=sum)
colnames(d_turtles_summary) <- c("FISHERY","YEAR","TOTAL_BYCATCH_YEAR_ST")
d_turtles_2 <- left_join(d_turtles,d_turtles_summary, by = c("FISHERY","YEAR"))
#View(d_turtles_2)


#Now make a seabird dataset
d_seabird <- d_SBST[d_SBST$GROUP=="seabird",] 

d_seabird_summary <- aggregate(d_seabird$BYCATCH, by=list(d_seabird$FISHERY, d_seabird$YEAR), FUN=sum)
colnames(d_seabird_summary) <- c("FISHERY","YEAR","TOTAL_BYCATCH_YEAR_SB")
d_seabird_2 <- left_join(d_seabird,d_seabird_summary, by = c("FISHERY","YEAR"))
#View(d_seabird_2)

#join sea turtle and seabird bycatch datasets
d_SBST_2 <- full_join(d_turtles_2, d_seabird_2, by = c("FISHERY","YEAR", "FISHERY.TYPE", "REGION", 
                                                       "GROUP", "BYCATCH", "Bycatch_level", "Total_Bycatch"))
#View(d_SBST_2)

d_SBST_10_13 <- filter(d_SBST_2, YEAR %in% c("2010", "2011", "2012", "2013") & !FISHERY.TYPE == "")
#View(d_SBST_10_13)




#write_csv(d_SBST_10_13, "d_SBST_10_13.csv")

# to join first read in fish/invert data
poster_master_table <- full_join(d1_cut, d_SBST_10_13, 
                                  by =  c("YEAR", "REGION", "FISHERY.TYPE", "FISHERY"))
#View(poster_master_table)
write_csv(poster_master_table, "poster_master_table.csv")

#split data into terciles
quantile(poster_master_table$TOTAL_BYCATCH_YEAR_ST, probs = c(0.33, 0.66), na.rm = TRUE)  # For Sea Turtles: 31.004 61.120 
quantile(poster_master_table$TOTAL_BYCATCH_YEAR_SB, probs = c(0.33, 0.66), na.rm = TRUE)  # For Seabirds: 99.0 252.8
quantile(poster_master_table$Bycatch_Ratio, probs = c(0.33, 0.66), na.rm = TRUE)  # For Fish/Inverts: 0.1069364 0.2551906 

# nicer color palette ----
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Get joined datasets ready for seabird/sea turtle plot
poster_master_table$GROUP <- as.character(poster_master_table$GROUP)
poster_master_table$GROUP[is.na(poster_master_table$GROUP)] <- "seabird/sea turtle"
poster_master_table = filter(poster_master_table, !GROUP == "marine mammal" & !FISHERY.TYPE == "")
poster_master_table$Bycatch_level <- as.character(poster_master_table$Bycatch_level)
poster_master_table$Bycatch_level[is.na(poster_master_table$Bycatch_level)] <- "low (<50)"

poster_master_table$Bycatch_level <- ordered(poster_master_table$Bycatch_level, c("low (<50)", "moderate (50-1000)", "high (>1000)"))

write_csv(poster_master_table, "poster_master_table.csv")

SBST <- ggplot(poster_master_table, aes(Bycatch_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of seabirds and sea turtles") +
  guides(fill=guide_legend(title="gear type")) +
  #scale_fill_manual(values=cbPalette) +
  #facet_wrap(~YEAR) + 
  theme_bw()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12))
SBST


######################################################################
# Needed to correct some ofthe seabird and sea turtle numbers manually
######################################################################

# New and final plots here: 
d_poster_final <- read_csv("poster_master_table_corrected.csv")
save(d_poster_final, file = "fisheries_master_table_corrected.RData")


#split data into terciles
quantile(d_poster_final$TOTAL_BYCATCH_YEAR_ST, probs = c(0.33, 0.66), na.rm = TRUE)  # For Sea Turtles: 19.375 53.500 
quantile(d_poster_final$TOTAL_BYCATCH_YEAR_SB, probs = c(0.33, 0.66), na.rm = TRUE)  # For Seabirds: 59.75 158.00 
quantile(d_poster_final$Bycatch_Ratio, probs = c(0.33, 0.66), na.rm = TRUE)  # For Fish/Inverts: 0.08524158 0.20482308

# makes a column turning bycatch for seabirds and sea turtles levels into three discrete categories based on terciles of non-zero data
# first for sea turtles
d_poster_final$ST_Bycatch_level <- ifelse(d_poster_final$TOTAL_BYCATCH_YEAR_ST > 35,"high (>35)", 
                                           ifelse(d_poster_final$TOTAL_BYCATCH_YEAR_ST > 1 & d_poster_final$TOTAL_BYCATCH_YEAR_ST < 35, "moderate (1-35)", "none"))
d_poster_final$ST_Bycatch_level <- as.character(d_poster_final$ST_Bycatch_level)
d_poster_final$ST_Bycatch_level[is.na(d_poster_final$ST_Bycatch_level)] <- "none"

#Now turn NAs into 0s
d_poster_final$TOTAL_BYCATCH_YEAR_ST[is.na(d_poster_final$TOTAL_BYCATCH_YEAR_ST)] <- 0


#next for seabirds

#see where the breaks are if you want to be subjective: 
hist(d_poster_final$TOTAL_BYCATCH_YEAR_SB, breaks = 100, xlim=c(0,1000))

d_poster_final$SB_Bycatch_level <- ifelse(d_poster_final$TOTAL_BYCATCH_YEAR_SB > 50,"high (>50)", 
                                          ifelse(d_poster_final$TOTAL_BYCATCH_YEAR_SB > 1 & d_poster_final$TOTAL_BYCATCH_YEAR_SB < 50, "moderate (1-50)", "none"))
d_poster_final$SB_Bycatch_level <- as.character(d_poster_final$SB_Bycatch_level)
d_poster_final$SB_Bycatch_level[is.na(d_poster_final$SB_Bycatch_level)] <- "none"

#Now turn NAs into 0s
d_poster_final$TOTAL_BYCATCH_YEAR_SB[is.na(d_poster_final$TOTAL_BYCATCH_YEAR_SB)] <- 0


View(d_poster_final)


## Change categorical labeling to ordered
d_poster_final$ST_Bycatch_level <- ordered(d_poster_final$ST_Bycatch_level, c("none", "moderate (1-35)", "high (>35)"))
d_poster_final$SB_Bycatch_level <- ordered(d_poster_final$SB_Bycatch_level, c("none", "moderate (1-50)", "high (>50)"))
d_poster_final$MMPA.Category <- ordered(d_poster_final$MMPA.Category, c("III", "II", "I"))

d_poster_final2 <- filter(d_poster_final, !FISHERY.TYPE == "troll")
View(d_poster_final2)

#############
# final plots
#############

ST <- ggplot(d_poster_final2, aes(ST_Bycatch_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of sea turtles") +
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
ST
#ggplotly(ST) # WHOAAA Interactive plot_ly plot

#ggsave("Preliminary figures/ST_bycatch_byfishery.pdf", ST)


SB <- ggplot(d_poster_final2, aes(SB_Bycatch_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of seabirds") +
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
SB

#ggsave("Preliminary figures/SB_bycatch_byfishery.pdf", SB)

d_poster_final3 <- filter(d_poster_final2, !MMPA.Category %in% c(NA, "ND"))


MMPA <- ggplot(d_poster_final3, aes(MMPA.Category)) +
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

#ggsave("Preliminary figures/MMPA_Category_byfishery.pdf", MMPA)


## Change label to ordered
d_poster_final2$BR_level <- ordered(d_poster_final2$BR_level, c("low (<0.2)", "moderate (0.2-0.5)", "high (>0.5)")) # more arbitrary scale

d_poster_final4 <- filter(d_poster_final2, !is.na(BR_level))


FishInvert <- ggplot(d_poster_final4, aes(BR_level)) +
  geom_bar(aes(fill = FISHERY.TYPE)) +
  ylab("Number of fisheries") +
  xlab("Bycatch ratio of fish and invertebrates") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=cbPalette) +
  #facet_wrap(~YEAR) + 
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12),
        legend.position = "top")
  
FishInvert

#ggsave("Preliminary figures/FishInvert_bycatch_byfishery.pdf", FishInvert)






## trying some box and joy plots of the same data ----

FishInvert_BP = ggplot(d_poster_final4, aes(reorder(FISHERY.TYPE, Bycatch_Ratio, median), Bycatch_Ratio, 
                       color = FISHERY.TYPE, fill = FISHERY.TYPE)) + 
  geom_boxplot(alpha = 0.3) +   coord_flip() +
  geom_jitter(alpha = 0.5, height = 0, width = 0.3)

FishInvert_BP +
  xlab("Gear Type") +
  ylab("Bycatch ratio of fish and invertebrates") +
  theme_bw() +
  #scale_fill_discrete(values=cbPalette) +
  ylim(0,1) +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.position="none")

ggsave("Preliminary figures/FishInvert_boxplot.pdf", FishInvert_BP)

#ggplotly(FishInvert_BP)

# FishInvert_V <- ggplot(d_poster_final4, 
#                       aes(x=Bycatch_Ratio, y=fct_reorder(FISHERY.TYPE, Bycatch_Ratio), 
#                           color = FISHERY.TYPE, fill = FISHERY.TYPE)) +
#   geom_jitter(alpha = 0.5, height = 0.55)


# Joy plot, now a density ridge plot ----
FishInvert_Joy <- ggplot(d_poster_final4, 
                         aes(x=Bycatch_Ratio, y=reorder(FISHERY.TYPE, Bycatch_Ratio, mean), 
                             color = FISHERY.TYPE, fill = FISHERY.TYPE)) +
  geom_point(aes(), alpha = 0.5, position=position_dodge(0.9)) +
  geom_density_ridges(scale = 1, alpha = 0.5, rel_min_height = 0.01) 

#run separately for whatever dumb reason
FishInvert_Joy +
  ylab("Gear Type") +
  xlab("Bycatch ratio of fish and invertebrates") +
  theme_bw() +
  #scale_fill_manual(values=cbPalette) +
  xlim(0,1) +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.position="none")

ggsave("Preliminary figures/FishInvert_joyplot.pdf", FishInvert_Joy)

# 
# fct_relevel(FISHERY.TYPE, 
#             "jig", "seine", "line", "pot", "dredge", 
#             "gillnet", "longline", "trawl")

ggplotly(FishInvert_Joy)





############
# Junk code ----
############

# THIS IS PRODUCING SOME REAL SCREWY RESULTS RN
fish_invert_master = read.csv("Fish_master_data_frame_DOM_analysis.csv")

master_table <- dplyr::outer_join(fish_invert_master, MMSBST_master10_15, 
                                  by =  c("COMMON.NAME", "SCIENTIFIC.NAME", "GROUP", "ESA.STATUS", "IUCN.STATUS",  
                                          "YEAR","UNIT", "FOOTNOTE.S.", "CV", "REGION", "FISHERY.TYPE", "FISHERY"))
View(master_table)
# "BYCATCH","TOTAL.FISHERY.BYCATCH" 

poster_master_table$Total_Bycatch.y[is.na(poster_master_table$Total_Bycatch.y)] <- 0
View(poster_master_table)

# Doesn't work because - Warning message:position_dodge requires non-overlapping x intervals
# FishInvert_V = ggplot(d_poster_final4, aes(reorder(FISHERY.TYPE, Bycatch_Ratio, median), Bycatch_Ratio, 
#                                             color = FISHERY.TYPE, fill = FISHERY.TYPE)) + 
#   geom_violin(alpha = 0.5, width = 5) +   coord_flip() + ylim(0,0.25)
#   geom_jitter(alpha = 0.5, height = 0, width = 0.3)
# 
# FishInvert_V
 
# d_poster_final4$FISHERY.TYPE <- as.factor(d_poster_final4$FISHERY.TYPE)
# # does any violin plot work?
# V_try <-  ggplot(aes(x=FISHERY.TYPE, y=Bycatch_Ratio), data = d_poster_final4) +
#                   geom_violin()+
#                   coord_flip()
# 
# ML_V = ggplot(Mass_loss, aes(x = Stage, y = log(Mass.loss..g.), fill = Stage, colour = Stage)) +
#   geom_violin(alpha = 0.3)



geom_histogram(alpha = 0.5) +
  ylab("Gear Type") +
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
FishInvert_Joy
