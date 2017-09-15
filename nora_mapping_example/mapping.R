
install.packages("leaflet")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("sp")
install.packages("raster")
install.packages("maps")
install.packages("RgoogleMaps")
install.packages("ggsn")
install.packages("GISTools")
install.packages("maptools")
install.packages("maps")
install.packages("prettymapr")
library(leaflet)
library(rgdal)
library(tidyverse)
library(magrittr)
library(sp)
library(raster)
library(maps)
library(RgoogleMaps)
library(ggsn)
library(GISTools)
library(maptools)
library(maps)
library(prettymapr)
install.packages("ggmap")
library(ggmap)

setwd("E:/nora") 
sites=read.csv("dive_sites.csv")
sites


##### some basic spatial commands for points

########  1. reading in points
### first, read in as data.frame
setwd("E:/nora") 
records=read.csv("dive_sites.csv")

### for more information: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
location=c(71,3,74,6)

map=get_map(location=location,zoom=8,source="google",maptype="hybrid")
records=read.csv("dive_sites.csv")

pdf(paste0(getwd(),"/","map.pdf"))
a=ggmap(map)
b=a+geom_point(aes(x=Long_wgs84,y=Lat_wgs84,color=Atoll),data=records)+
coord_cartesian()+
geom_rect(ymin=2.89, ymax=3.8, xmin=71, xmax=72.12,color="black",fill="white",size=2)+
scalebar(location="bottomright",y.min=3, y.max=3.5, x.min=71, x.max=72, dist=50, height=.1,dd2km= TRUE, model='WGS84',st.dist=.1,st.size=2.5)+
theme(legend.position=c(.3,.2),legend.background = element_rect(colour = F, fill = NA),legend.key = element_rect (fill = F, colour = F))
plot.new()
b
addnortharrow(pos="bottomleft", padin = c(0.3, 0.2)) ## if this throws an error, type plot.new() and then run from ggmap(map)..
dev.off()

#north.arrow(72.71,2.8,.01,lab='N',cex.lab=1,tcol='black',col="black")














