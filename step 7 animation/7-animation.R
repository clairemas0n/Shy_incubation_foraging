# 
# Ch3 Claire Mason PhD # 
# Animating incubating GPS with ocean colour behind # 
# 

#install.packages("installr")
library("installr")
install.Rtools()


#install.packages("move")
#install.packages("devtools")
#library(devtools)
devtools::install_github("16EAGLE/moveVis")
#install.packages("moveVis")
library(moveVis); library(move); library(tidyverse); library(randomcoloR);library(ozmaps)


# convert data to 'move' object

inc <- read.csv("xy_df_Nov2021.csv")

inc$gmt <- as.POSIXct(strptime(inc$gmt, "%Y-%m-%d %H:%M:%S", tz = "GMT"))  

# calculate extent so animations have the same extent for each year 
coordinates(inc) <- c("Longitude", "Latitude") 
proj4string(inc) <- CRS("+proj=longlat +datum=WGS84")
inc_ext <- extent(inc)


inc <- as.data.frame(inc)

# have to do via years I think for now 
inc13 <- inc %>% filter(Year =="2013")
inc14 <- inc %>% filter(Year =="2014")
inc15 <- inc %>% filter(Year =="2015")
inc16 <- inc %>% filter(Year =="2016")
inc17 <- inc %>% filter(Year =="2017")
inc18 <- inc %>% filter(Year =="2018")
inc19 <- inc %>% filter(Year =="2019")


# clipping for cuttlefish feeders

inc_cuttlefish <- inc %>% filter(Latitude < -40 & Longitude > 145 & Longitude < 147)
coordinates(inc_cuttlefish) <- c("Longitude", "Latitude") 
proj4string(inc_cuttlefish) <- CRS("+proj=longlat +datum=WGS84")
plot(inc_cuttlefish);oz(add=TRUE)


inc_cuttlefish <- as.data.frame(inc_cuttlefish)

t<- inc_cuttlefish %>% group_by(t_name) %>% tally()
View(t)

inc_cuttlefish <- inc_cuttlefish %>% 
  group_by(t_name) %>% 
  filter(n() >= 10)

# create move object 

incCuttlefish_m <- df2move(inc_cuttlefish, proj=CRS("+proj=longlat +datum=WGS84"), 
                 x="Longitude", y="Latitude", time="gmt", track_id="t_name")


#align dt stamps
incCuttlefish_m <- align_move(incCuttlefish_m, res = 600 , unit = "secs")  #1200 res for 2018
# ERROR in dimnames length of 'dimnames' [1] not equal to array extent
# SOLVED maybe?: changed res = "mean" to res = 4 



# create spatial frames

colours <- distinctColorPalette(length(unique(inc_cuttlefish$t_name)))


frames_inc_cuttlefish <- frames_spatial(incCuttlefish_m, path_colours = colours,
                                map_service="osm", map_type="toner",
                               #map_service="mapbox", 
                              # map_token= "pk.eyJ1IjoiY2xhaXJlbWFzb24yIiwiYSI6ImNqdWoxZjEybzFnc2s0OXBiZXJ4eDc1ODgifQ.k4la5gv03sUjKqyqhHrObg",
                               #map_type = "streets", 
                               #map_res= 0.2, #Error in curl_download(url = url, destfile = file) : HTTP error 404.  #solution - map_res=0.1 - 0.5
                               path_legend=FALSE, equidistant=FALSE) %>% #, 
                               #ext = extent(inc_cuttlefish)) %>%
                              add_northarrow(position="upperright" , colour = "white") %>% 
  #position = "bottomleft", colour="white") %>%
  add_scalebar(distance=20) %>% #200,
            #   colour = "white") %>% #, position = "bottomleft") %>%
  add_progress() %>%
  add_timestamps(incCuttlefish_m, type = "label") %>%
  add_labels(x = "Longitude", y = "Latitude")


frames_inc_cuttlefish[[100]]


# animate frames
animate_frames(frames_inc_cuttlefish, fps=45,
               out_file = "inc_cuttlefish_full2.mp4")


## In 2013 - it cuts to a section of 2016 1.40 in? Check in on this, and year classification/filtering? 
# hmm inc13 data doesn't have 2016 in its 'gmt' 
# maybe just have to restart R session for every one, so it doesn't reuse anything stored in the memory?? 

###Also, 2018 has lots of jumping round – because 20 min temporal resolution. Good to note for future deployment. Maybe need to slow down the fps? Half it? To 10 fps I reckon, might make another. 


###### 
# Add ocean colour behind 
# https://gist.github.com/16EAGLE/4bfb0ca589204c53041244aa705b456b

# might have to gganimate to get this working ... 

library(raadtools)

chl.pal() #Ocean colour colours for chlorophyll-a.

chlafiles() #Chlorophyll-a

coastmap() #Coast map


