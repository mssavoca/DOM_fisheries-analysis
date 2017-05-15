#Script for examining a subset of the fisheries bycatch data: Fish bycatch by fishery, Northeast region, 2010-2013

library(tidyr)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)

# Set working directory and read in data
setwd("/Users/matthewsavoca/Documents/Research Data/CASG_NOAA/DOM fishery analysis/")

orig_csv_NEfish2010 = read.csv("Original CSVs/NE region/NE_FirstEditionUpdate1-2010Data_Fish_By_Fishery_15-MAY-2017.csv", header = FALSE)

names(orig_csv_NEfish2010) = unlist(orig_csv_NEfish2010[3, ]) # makes the third row the header 
orig_csv_NEfish2010 = orig_csv_NEfish2010[-(1:3),] # deletes first three rows of data frame

View(orig_csv_NEfish2010)


# remove fishery summary rows since I already have columns with those values
#trim_csv = orig_csv_combined[!(is.na(orig_csv_combined$YEAR) | orig_csv_combined$YEAR==""), ] 