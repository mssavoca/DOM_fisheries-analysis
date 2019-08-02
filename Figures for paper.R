#########################################
# Script for figures to include in paper ----
#########################################

# load packages and data ----
library(tidyverse)
library(readxl)
library(ggjoy)
library(viridis)
library(ggpubr)

# formula for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

# fisheries summary data
d1 <- read_csv("SummaryData_July2019_AnalysisExport_AllFisheryYears.csv")
d1$Year <- as.factor(d1$Year)
d1$Region <- as.factor(d1$Region)

d_raw_fish <- read_excel("Fish_master_data_frame_DOM_analysis_all_data.xlsx")
d_raw_fish$YEAR <- as.factor(d_raw_fish$YEAR)
d_raw_fish$REGION <- as.factor(d_raw_fish$REGION)

d_raw_SBST <- read_excel("MM_SB_ST_master_data_frame2010_2015.xlsx")

# summary tables and statistics
d_summ_full <- d_raw_fish %>% 
  filter(UNIT == "POUND") %>% 
  group_by(FISHERY) %>% 
  summarize(num.fisheries = n_distinct(FISHERY),
            total.fish.bycatch = sum(TOTAL.FISHERY.BYCATCH),
            total.target.catch = sum(TOTAL.FISHERY.LANDINGS),
            total.catch = sum(TOTAL.CATCH),
            total.BR = total.fish.bycatch/total.catch)

d_summ_SBST <- d_raw_SBST %>% 
  filter(GROUP %in% c("sea turtle", "seabird")) %>% 
  group_by(FISHERY, GROUP, YEAR) %>% 
  summarize(total.SBST.bycatch = sum(BYCATCH))


d_summ_summ <- d1 %>% 
  filter(TotalBycatch_lbs !="NA") %>% 
  group_by(Region) %>% 
  summarize(num.fisheries = n_distinct(Fishery),
            total.SBST.bycatch = sum(TotalBycatch_inds),
            total.fish.bycatch = sum(TotalBycatch_lbs),
            bycatch.ratio = Bycatch_ratio,
            total.fish.catch = TotalBycatch_lbs/Bycatch_ratio,
            total.target.catch = total.fish.catch-total.fish.bycatch)


a=d1 %>% group_by(Fishery) %>% select(GearType_general, GearType_specific, MMPA, Year)

d_crit_summ <- d1 %>% 
  #filter(GearType_general == "trawl") %>% 
  group_by(Fishery) %>% 
  summarise(avg.score = mean(mean_criteria),
            SE.score = SE(mean_criteria))

# log transform response to get normal distribution for frequentist stats, or do MCMCglmm


# figures ----

########################
# Figure 1 Rose plots
########################

d_summ_fig1 <- d1 %>% 
  
  na.omit(TotalBycatch_lbs) %>% 
  mutate(total.fish.catch = TotalBycatch_lbs/Bycatch_ratio) %>% 
  #na.omit(total.fish.catch) %>% 
  group_by(Region, GearType_general) %>% 
  summarise(num.fisheries = n_distinct(Fishery),
            sum.landings = sum(total.fish.catch),
            log.landings = log10(sum(total.fish.catch)))

fisheries_rose <- ggplot(d1, 
                         aes(x = GearType_general, 
                             y = log10(TotalBycatch_lbs/Bycatch_ratio), 
                             fill = GearType_general)) +
  geom_bar(position = "stack", stat="identity") +
  facet_wrap(.~Region) +
  coord_polar()
fisheries_rose


ggplot(data=data3,aes(x=variable,y=value,fill=group))+
  geom_bar(stat="identity")+
  coord_polar()+
  scale_fill_brewer(palette="Greens")+xlab("")+ylab("")


#################
# Figure 2 ----
#################

# get silhouette images for figures
imgshark <- png::readPNG("./shark.png")
rastshark <- grid::rasterGrob(imgshark, interpolate = T)
imgcrab <- png::readPNG("./crab.png")
rastcrab <- grid::rasterGrob(imgcrab, interpolate = T)
imgjelly <- png::readPNG("./jelly.png")
rastjelly <- grid::rasterGrob(imgjelly, interpolate = T)
imgdolphin <- png::readPNG("./dolphin.png")
rastdolphin <- grid::rasterGrob(imgdolphin, interpolate = T)
imgpinniped <- png::readPNG("./pinniped.png")
rastpinniped <- grid::rasterGrob(imgpinniped, interpolate = T)
imgseaturtle <- png::readPNG("./seaturtle.png")
rastseaturtle <- grid::rasterGrob(imgseaturtle, interpolate = T)
imgfulmar <- png::readPNG("./fulmar.png")
rastfulmar <- grid::rasterGrob(imgfulmar, interpolate = T)
imgalbatross <- png::readPNG("./albatross.png")
rastalbatross <- grid::rasterGrob(imgalbatross, interpolate = T)


# identifying quantiles for break points
quantile(d1$Bycatch_ratio, probs = c(0.5, 0.75), na.rm = TRUE)
quantile(d1$TotalBycatch_inds, probs = c(0.5, 0.75), na.rm = TRUE)

#adding columms changing continuous values to discrete for figure
d1 <- d1 %>% mutate(
  BR_ratio_cat = cut(Bycatch_ratio, breaks=c(-Inf, 0.1480491, 0.3037714, Inf), 
                     labels=c("low (<0.15)","moderate (0.15-0.30)","high (>0.30)")),
  TotalBycatch_SBST_cat = cut(TotalBycatch_inds, breaks=c(-Inf, 0, 26.875, Inf),
                              labels=c("none","moderate (1-26)","high (>26)")),
  MMPA_cat = case_when(MMPA == 1 ~ "III",
                       MMPA == 2 ~ "II",
                       MMPA == 3 ~ "I"))

# defining the color palette
HW_palette <- c("#9f7bb2", "#7dac33","#c64f79","#93ccaf","#8e97ee", "#59663e","#ffca33", "#c5703f","#4d304b")

BR_gear <- ggplot(filter(d1, BR_ratio_cat != "NA"), 
                         aes(BR_ratio_cat)) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Bycatch ratio of fish and invertebrates") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
   annotation_custom(rastshark, ymin = 160, ymax = 190, xmin = 0, xmax = 5) +
  annotation_custom(rastcrab, ymin = 140, ymax = 160, xmin = 1) +
  annotation_custom(rastjelly, ymin = 140, ymax = 160, xmin = 2) 
BR_gear


B_indSBST_gear <- ggplot(filter(d1, TotalBycatch_SBST_cat != "NA"), 
                  aes(TotalBycatch_SBST_cat)) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of seabirds and sea turtles") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
  annotation_custom(rastfulmar, ymin = 220, ymax = 250, xmin = 2) +
  annotation_custom(rastseaturtle, ymin = 170, ymax = 215, xmin = 1.7)
B_indSBST_gear 


MMPA_gear <- ggplot(filter(d1, MMPA_cat != "NA"),
                    aes(fct_relevel(MMPA_cat, "III", "II", "I"))) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Marine Mammal Protection Act Category") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
  annotation_custom(rastpinniped, ymin = 230, ymax = 270, xmin = 2) +
  annotation_custom(rastdolphin, ymin = 175, ymax = 215, xmin = 1.5)
MMPA_gear

ggarrange(BR_gear, B_indSBST_gear, MMPA_gear,
          labels = c("A", "B", "C"), # THIS IS SO COOL!!
          common.legend = TRUE, legend="top",
          ncol = 3, nrow = 1)


#################
# Figure 3 ----
#################

# Get 50 and 75% quantile breaks for all fishery-years scores
quantile(d1$mean_criteria, probs = c(0.5, 0.75), na.rm = TRUE)


# overall histogram of mean criteria score
mean_score_hist <- ggplot(d1, aes(mean_criteria)) +
  geom_histogram(binwidth = 0.05, color="black", fill="gray80") +
  xlab("Relative Bycatch Index (RBI)") +
  ylim(-10,160) +
  # annotate("text", x = c(0.05, 0.3), y= -7, 
  #          label = c("better performing", "worse performing")) +
  #geom_density(alpha=.2, fill="#FF6666") +
  #facet_wrap(.~GearType_general) +
  geom_vline(xintercept = c(0.07810263, 0.14722190), color = "blue", linetype = "dashed") +
  theme_classic()
mean_score_hist 


#density plot by gear type
dens_by_GT <- ggplot(d1, aes(mean_criteria, fct_reorder(GearType_general, mean_criteria, .desc = TRUE), 
                             fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  ylab("Gear Type") +
  # annotate("text", x = c(0.05, 0.3), y= 0.65, 
  #          label = c("better performing", "worse performing")) +
  scale_fill_viridis(name = "mean_criteria", option = "B") +
  xlab("RBI") +
  theme_classic()
dens_by_GT

#density plot by region
#d1$Region <- ordered(d1$Region, levels = c("PI", "SE", "NE", "WC", "AK"))

dens_by_region <- ggplot(d1, aes(mean_criteria, fct_reorder(Region, mean_criteria, .desc = TRUE), 
                                 fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.05, height = 0),
                          point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                          show.legend = FALSE) +
  ylab("Region") +
  # annotate("text", x = c(0.05, 0.3), y= 0.65,
  #          label = c("better performing", "worse performing")) +
  scale_fill_viridis(name = "mean_criteria", option = "B") +
  xlab("RBI") +
  theme_classic()
dens_by_region + guides(size = FALSE)


#density plot by year
dens_by_year <- ggplot(d1, aes(mean_criteria, fct_relevel(Year, "2015", "2014", "2013", "2012", "2011", "2010"), 
                                 fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  ylab("Year") +
  # annotate("text", x = c(0.05, 0.3), y= 0.65, 
  #          label = c("better performing", "worse performing")) +
  scale_fill_viridis(name = "mean_criteria", option = "B") +
  xlab("RBI") +
  theme_classic()
dens_by_year



#combine plots into one mega figure
ggarrange(mean_score_hist,                                        # First row with scatter plot
          ggarrange(dens_by_GT, dens_by_region, dens_by_year, 
                    ncol = 3, labels = c("B", "C", "D")), # Second row with box and dot plots
                      nrow = 2, labels = "A" )                    # Labels of the scatter plot


hist_BR <- ggplot(d1, aes(mean_criteria)) +
  geom_histogram(binwidth = 0.05, color="black", fill="gray80")
