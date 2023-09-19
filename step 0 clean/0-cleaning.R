# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #

# I am exploring seven years of GPS data from incubating shy albatross from Albatross Island

# Script no 0
# The output of this first script is cleaning data, defining a 'trip', 
# It is a separate script because I needed to clean some trip starts and ends in excel which I do between script no. 0 and script no.1. 


#########################################################
# import, format and clean raw data #
#########################################################

#load packages 
library(tidyverse); library(sp); library(trip); library(geosphere); library(tidyr); library(tibble); library(sf); library(mapview); library(oz); library(geodist); library(gridExtra); library(lubridate)

#import dataset
#before you do - in excel concatenate and format 'dt' column to be y-m-d h:m:s
xy <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/Master_datafiles/MASTER_all_IncGPS_2021_11_09_added750-4_2019.csv") # all date times in GMT

#format new date-time columns
xy$gmt <- as.POSIXct(strptime(xy$dt, "%Y-%m-%d %H:%M:%S", tz = "GMT"))  



#
# plot xy data
#par(mfrow=c(1,1))
#coordinates(xy) <- c("Longitude", "Latitude") 
#proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
#plot(xy);oz(add=TRUE) # looks like only one track to hobart?? so not too bad really 

# remove duplicate date-times for each ID 
xy <- as.data.frame(xy) 
nrow(xy)
xy2 <- xy %>% group_by(NewID) %>% distinct(gmt, .keep_all=TRUE) 
nrow(xy2) # removed 10 rows, no worries!

# take that onboard and rename 
xy_df <- xy %>% group_by(NewID) %>% distinct(gmt, .keep_all=TRUE) 


#########################################################
# trip definition #
#########################################################

# create new column with distance to colony 
# convert xy data to spatial object first
coordinates(xy_df) <- c("Longitude", "Latitude") 
proj4string(xy_df) <- CRS("+proj=longlat +datum=WGS84")

# calculate distance from Albatross Island for each point , default output is meters
xy_df$disthome <- distHaversine(xy_df, c(144.6557384, -40.3780953)) # used google earth to get super precise coordinate within south study 

# convert from spatial object back to dataframe 
xy_df <- as.data.frame(xy_df)

# assign each point as either "colony" or "not"
# colony = 0
# at sea = 1 
# these values live in a column called "colonyYN"
xy_df <- xy_df %>% mutate(colonyYN = if_else(disthome>400, 1, 0))# play with this disthome>x value  
## from eyeballing data (in detail and in excel) - 400 is precise and good! Just need to explore the birds that don't have equal start and ends of trips 


# explore how this changes the classification of land vs sea 
#plot(xy_df$colonyYN[2000:3000])
#summary(is.na(xy_df$colonyYN))  # check for NAs, none using 2000 , none using 100


# create a new column called "new" to identify start and end of a trip 
# start of a trip will be tagged as '-1' because going from land (0) to sea (1) aka 0-1=-1
# end of a trip will be tagged as '1' because going from sea (1) to land (0) aka 1-0=1 
# all other locations will be tagged as 0 aka no change 

# had some issues with IDs starting with -1 .. maybe carryover from ID before them and ending at sea?? 
# but - group_by(NewID) gives warning and 139 NAs .. so this is great, it starts afresh at each new ID... just need to assign all these NAs as 0 and assume they start on colony - then just explore any that start at sea manually 
xy_df <- xy_df%>% group_by(NewID) %>% 
  mutate(new = colonyYN-lag(colonyYN))

summary(is.na(xy_df$new))  # when I first ran this there was 139 NAs, very strange but has somehow fixed itself. 
# Do you know what this is?!?! It is the first value for each new ID, this is perfect.. because they there isn't a carryover from the last bird (1-1) or anything 

#It's actually 140 now - because we have added in 750_4 from 2019

# for the 1 NA at the start, (can't subtract from nothing) convert it to zero 
xy_df$new[is.na(xy_df$new)] <- 0


# now to explore the data and any bugs, we want to look at how many '-1's and '1's we have. If we are capturing all the trips, the numbers should be equal (same number of start trips and end trips)
xy_df %>% group_by(new) %>% summarise(n = n())

# for disthome>400 trips,and with no filtering deploy/retrieve, we have 429 starts '1's and 436 ends '-1's 
# actually (Nov 2021) we have 436 ends (-1s) and 430 (1s) starts
# makes sense - the trip 750_4 from 2019 has added a start, but didn't end (only 2 days of data)

tripnos <- xy_df %>% group_by(NewID, new) %>% summarise(n = n()) %>% filter(new!=0)
#View(tripnos) ##hmm lots of mismatches, maybe have to up to 200m to clean this. 
## WOW! WHen not filtering to deploy-retrieve, this looks heaps better and less mismatches 
# take into excel and clean up the trips further down when 1s and -1s assigned 

write.csv(tripnos, "tripnos-400m_raw_script0-output.csv")

write.csv(xy_df, "xy_trips_400m_raw_script0-output.csv")


#########################################################
# tidy in excel #
# if trip 'started at sea' then flagged first location as the 'start' of a trip i.e. new=1. We can assume bird left immediately after tagging. if trip 'ended at sea' then the whole trip i.e. string of new=1 was discarded. either unfinished foraging trip, or tag still on as we left island. Did this manually in excel. 
#########################################################

# coming back to this Nov 2021
# just appended ID750-4 2019 onto the end , didn't do it from scratch again 
# 750_4 (2019) didn't complete a trip - last recorded location was King Island

