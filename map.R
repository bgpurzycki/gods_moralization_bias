################################################################################
#
############## Figure Map
# Load Data
d<-read.csv("./Data/GPS.csv")

Latitude<-d$Lat
Longitude<-d$Long
Label<-d$Population
Voffset <-d$voffset
Hoffset <-d$hoffset


# Using ggplot, plot the Base World Map
mp <- NULL

mapWorld <- borders("world", colour="burlywood3", fill="burlywood") 

## ggplot2 map_data references maps::map as just "map", so you cant do maps and then rethinking! 

mp <- ggplot() +  mapWorld    + theme(axis.text=element_text(size=14), panel.background = element_blank(),

     axis.title=element_text(size=14))+
labs(x="Longitude",y="Latitude")  + coord_map(projection = "mercator",xlim=c(-180,180),ylim=c(-65, 75))

# Now Layer the populations on top
mp <- mp + geom_point(aes(x=Longitude, y=Latitude) ,color="black", size=2)
mp <- mp +  geom_text_repel(aes(x=Longitude, y=Latitude, label = Label), force=6,nudge_x = -1.5,color="black",size=6)
    mp

ggsave( "Map.pdf", mp, width = 10, height = 6.5)
