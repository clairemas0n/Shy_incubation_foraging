
##########
# Animating incubation GPS tracks
# Basic script to get a animation of all GPS years (2013-2019)
# Next step is to add oceanography data behind
# Then, to have them all years going, with all birds from the same year, a different colour e.g. http://animove.org/wp-content/uploads/2019/04/Daniel_Palacios_animate_moveVis.html
# claire.mason@utas.edu.au



library(devtools)
devtools::install_github("16EAGLE/moveVis")

library(move); library(moveVis); library(dplyr); library(raster)


xy_df <- readRDS("xy_df_clean_min20locs.R")


# create move object 
?df2move()

### work through this on subsets 

xy_df2018 <- subset(xy_df, Year==2018, droplevels=TRUE)

inc_m_2018 <- df2move(xy_df2018, proj=CRS("+proj=longlat +datum=WGS84"), 
                 x="Longitude", y="Latitude", time="gmt", track_id="NewID") 
                  # ,  data=xy_df$trip_id)


#align dt stamps
inc_m_2018 <- align_move(inc_m_2018) # took out the res and uni arguments for 2016 because it was whizzing around!! , res = "mean", unit = "secs")


# create spatial frames

colours <- rainbow(length(unique(inc16_dup_p1$ID)))
frames_inc16 <- frames_spatial(inc16_m, path_colours = colours,
                             
                             # map_service="osm", map_type="toner",
                             
                             map_service="mapbox", map_token= "pk.eyJ1IjoiY2xhaXJlbWFzb24yIiwiYSI6ImNqdWoxZjEybzFnc2s0OXBiZXJ4eDc1ODgifQ.k4la5gv03sUjKqyqhHrObg",
                             map_type = "satellite", #map_res= 0.2, 
                             #Error in curl_download(url = url, destfile = file) : HTTP error 404.  #solution - map_res=0.1 - 0.5
                            tail_length = 2, # has to be whole number, bigger number = shorter tail ?? 
                             path_legend=FALSE, equidistant=FALSE)


frames_inc16[[50]] # preview one of the frames

#add other map features

frames_extras16 <- frames_inc16 %>%
                  add_northarrow(colour = "white") %>% #position = "bottomleft", colour="white") %>%
                  add_scalebar(distance=200, colour = "white") %>% #, position = "bottomleft") %>%
                  add_progress() %>%
                  add_timestamps(inc16_m, type = "label") %>%
                  add_labels(x = "Longitude", y = "Latitude")


frames_extras16[[100]]


# animate frames

animate_frames(frames_extras16, 
               #fps=9,
                out_file = "inc16_p1_1.mp4")




# save all data from a session 
#save R data saveRDS(my_precious, here("results", "my_precious.rds"))
library(here)

saveRDS(inc13_dup, here("inc13_dup", "inc13_dup.rds"))
saveRDS(frames_inc13, here("frames_inc13", "frames_inc13.rds"))


# and to read back in 
my_precious <- readRDS(here("results", "my_precious.rds"))

############################
### stage 2 : adding live oceanography backgrounds 
## I need to be on the server for this I think!! 

# need raadtools package
install.packages("Rtools")
install.packages("raadtools") # from github??


# send mike 
# EXTENT 
# DATES
# VARIABLES 

##gganimate - animation with background?? 
  sst <- readsst("2018-09-16", xylim = extent(100, 150, -50, -30))
                                                         



