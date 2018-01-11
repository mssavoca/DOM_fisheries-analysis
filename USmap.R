#######
# Making a map for use on poster and in paper
#######

#load packages
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(fiftystater)
library(usmap)

usa <- map_data("usa")

m <- ggplot(data = usa, map_id = state)) + 
  geom_map(aes(map_id = state, map = fifty_states))

m

  coord_fixed(1.3)

geom_map(aes(fill = Assault), map = fifty_states)


#this simple thing works believe it or not
plot_usmap()




crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
p <- ggplot(crimes, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Assault), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

p