#########################################
# Script for figures to include in paper ----
#########################################

# load packages and data ----
library(tidyverse)
library(readxl)
library(ggjoy)
library(viridis)

# fisheries summary data
d1 <- read_csv("SummaryData_June2019_AnalysisExport_AllFisheryYears.csv")
d1$Year <- as.factor(d1$Year)
d1$Region <- as.factor(d1$Region)


# figures ----

# overall histogram of mean criteria score
mean_score_hist <- ggplot(d1, aes(mean_criteria)) +
  geom_histogram(binwidth = 0.05, color="black", fill="white") +
  xlab("Overall fishery score (average of all criteria)") +
  ylim(-10,160) +
  annotate("text", x = c(0.05, 0.3), y= -7, 
           label = c("better performing", "worse performing")) +
  #geom_density(alpha=.2, fill="#FF6666") +
  #facet_wrap(.~GearType_general) +
  theme_classic()
mean_score_hist 

#density plot by region
d1$Region = relevel(d1$Region, ref = c("PI", "SE", "NE"))

dens_by_region <- ggplot(d1, aes(mean_criteria, Region, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.9,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.05, height = 0),
                          point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,) +
  annotate("text", x = c(0.05, 0.3), y= 0.65,
           label = c("better performing", "worse performing")) +
  scale_fill_viridis(name = "mean_criteria", option = "B") +
  xlab("Overall fishery score (average of all criteria)") +
  theme_classic()
dens_by_region

#density plot by year
d1$Year = reorder(d1$Year, new.order=c("2010", "2011", "2012", "2013", "2014", "2015"))

dens_by_region <- ggplot(d1, aes(mean_criteria, fct_relevel(Year, "2015", "2014", "2013", "2012", "2011", "2010"), 
                                 fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.9,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,) +
  annotate("text", x = c(0.05, 0.3), y= 0.65, 
           label = c("better performing", "worse performing")) +
  scale_fill_viridis(name = "mean_criteria", option = "B") +
  xlab("Overall fishery score (average of all criteria)") +
  theme_classic()
dens_by_region
