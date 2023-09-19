

# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #

# Script no 1

# I am exploring seven years of GPS data from incubating shy albatross from Albatross Island

# The output of this script is further defining trips and calculating trip metrics. A summary table of ALL metrics for each trip is the final output. 

# THINGS TO FIX / EXPLORE # 

#1# for analysis also want to look at variation - not just mean. As Patrick et al 2021 (GlobChgBiol) shows 
#2# think about the pause in tracks that we can see from the animations. using lunar cycle to see if it is just them resting at night...Or is it lulls in the wind? 
#3# do I need an anchor at the colony for the start and end of all trips ?? - yes i reckon so... to make durations and distances accurate. Do this ... 


library(tidyverse); library(sp); library(trip); library(geosphere); library(tidyr); library(tibble); library(sf); library(mapview); library(oz); library(geodist); library(gridExtra); library(lubridate); library(circular); library(agricolae); library(ggpubr)


#########################################################
# import data from excel where I cleaned the trip starts/ends manually #
#########################################################

xy_df <- read.csv("C:/Users/mas211/OneDrive - CSIRO/2 PhD cont/R files Ch3 dwld OneDrive 7 July 23/Ch3/Master_datafiles/xy_trips_400m_edits_Nov2021_latest.csv")

#summary tables for Table1. of MS 
xy_df %>% group_by(Year) %>% distinct(NewID) %>% count()

# reformat date-time
xy_df$gmt <- as.POSIXct(strptime(paste(xy_df$Date, xy_df$Time), "%d/%m/%Y %H:%M:%S", tz = "GMT")) 


#########################################################
# identify each trip #
#########################################################
# create trip_id column 
xy_df$trip_id <- ifelse(xy_df$new==1, 1, 0)  # carry over 1 to trip_id to flag trip start
xy_df$trip_id <- ifelse(xy_df$new==1, cumsum(xy_df$trip_id),NA) # convert 1s in trip_id to a number sequence instead of repeated 1, seems to need NA as the alternative, doesn't work with 0
xy_df$trip_id <- ifelse(xy_df$new==-1, 0, xy_df$trip_id) # flag end of the trip (new=-1) in trip_id column by carrying over a zero, keep everything else the same

xy_df <- xy_df %>%  fill(trip_id) # fill the trip_id value down 
xy_df$trip_id[is.na(xy_df$trip_id)] <- 0 # convert NAs at start back to zeros 

xy_df$trip_id <- as.integer(xy_df$trip_id)


#########################################################
# remove trips with too few locations #
#########################################################
# visual exploration of the trip shape with different n of locations 
# these trips with 1-3/4 locs are just error from GPS, bouncing off island a few 100 meters 
# from exploring these - the shape of points that once you get to 6 dots, you can see a bit of a sitting on water or some pattern. 5 or less, seems more random. 
# 10 locations is a good buffer - also deletes lots of locations where max distance is greater than total distance 

#### Methods summary stats 
length(unique(xy_df$trip_id)) # 438 trips

#remove trips with 10 or less locations 
xy_df <- xy_df %>% 
  group_by(trip_id) %>% 
  filter(n() >= 10)

# now only 322 trips - remember one trip (id=0) is just land locations

# yes happy with this, rename and reassign trips so they're in order 

xy_df$trip_id2 <- ifelse(xy_df$new==1, 1, 0)  # carry over 1 to trip_id to flag trip start
xy_df$trip_id2 <- ifelse(xy_df$new==1, cumsum(xy_df$trip_id2),NA) # convert 1s in trip_id to a number sequence instead of repeated 1, seems to need NA as the alternative, doesn't work with 0
xy_df$trip_id2 <- ifelse(xy_df$new==-1, 0, xy_df$trip_id2) # flag end of the trip (new=-1) in trip_id column byt carrying over a zero, keep everything else the same

xy_df <- xy_df %>%  fill(trip_id2) # fill the trip_id value down 
xy_df$trip_id2[is.na(xy_df$trip_id2)] <- 0 # convert NAs at start back to zeros 

xy_df$trip_id2 <- as.integer(xy_df$trip_id2)

xy_df$trip_id <- NULL # delete old trip id column 

xy_df <- xy_df%>% 
  rename(trip_id = trip_id2) # rename new trip id column 

length(unique(xy_df$trip_id)) ## 322 new. only 14 trips with between 6-10 locations. OLD- 333 trips  (this still includes all the trips labelled id=0 aka on land locations)

# remove land locations 
xy_df <- xy_df %>% filter(trip_id !=0) # and remove all trip id =0 (i.e. the 'on land' locations)




### THIS IS WHERE i lose a NewID deployment in 2016 and 2017 and 2019 !!! - SORTED NOW
#xy_df %>% filter(Year=="2016") %>% select(NewID) %>% ungroup() %>% distinct(NewID) # 15 IDs before trip definition and cutting to trips less than 10 locs
#xy_df %>% filter(Year=="2017") %>% select(NewID) %>% ungroup() %>% distinct(NewID)  # same for 2017. 15 IDS before 
#xy_df %>% filter(Year=="2019") %>% select(NewID) %>% ungroup() %>% distinct(NewID) 

# in 2016 is is ID28 that goes missing
# in 2017 also ID28 that goes missing 
# in 2019 it is ID30 that goes missing
# lets plot 
# back to start, load raw data
#coordinates(xy_df) <- c("Longitude", "Latitude") 
#proj4string(xy_df) <- CRS("+proj=longlat +datum=WGS84")
# just one 
#plot(xy_df[xy_df$NewID=="ID28_2016", ]);oz(add=TRUE)
#plot(xy_df[xy_df$NewID=="ID28_2017", ]);oz(add=TRUE)
#plot(xy_df[xy_df$NewID=="ID30_2019", ]);oz(add=TRUE)
# WOW all huge interesting trips 
# go back to excel. These two started 'at-sea' so just flagged the first location "at-sea" as the start of the trip. didn't add new row. like all the others. don't have COLONY locations for any trips. so just first and last location at sea, no anchor. 


# make new column for trip id and BirdID - so easy to see year and bird name etc on plots by labelling with this new ID 
xy_df$t_name <- paste(xy_df$NewID, xy_df$trip_id)

######## HERE I NEED TO KEEP ONE LAND LOCATION AT THE START AND END 

# LAST FIX : add ten minutes and colony location 
xy_df <- xy_df %>%
  group_by(t_name) %>%
              summarise(gmt = last(gmt)) %>%
              mutate(Longitude = 144.6557384, Latitude =  -40.3780953, gmt= (gmt + minutes(10)) ) %>% 
              bind_rows(xy_df, .) %>% 
              arrange(t_name)
            

# FIRST FIX: add ten minutes and colony location 
xy_df <- xy_df %>%
  group_by(t_name) %>%
  summarise(gmt = first(gmt)) %>%
  mutate(Longitude = 144.6557384, Latitude =  -40.3780953, gmt= (gmt - minutes(10)) ) %>% 
  bind_rows(xy_df, .) %>% 
  arrange(t_name)


# running through and checking summary.tb again - still 13 weird ones where A > 0.5 

# so export without trips for Birdlife 

#########################################################
# plot each trip #
#########################################################
#coordinates(xy_df) <- c("Longitude", "Latitude") 
#proj4string(xy_df) <- CRS("+proj=longlat +datum=WGS84")
# just one 
#plot(xy_df[xy_df$trip_id=="127", ]);oz(add=TRUE)


## for loops to run all plots together 
#par(mfrow=c(5,5))
#par(mar = c(1.5, 1.5, 1.5, 1.5))
#ids <- unique(xy_df$t_name)

#for (i in 326:332){  # do in blocks of 12? 
#  plot(xy_df[xy_df$trip_id==i, ], xlim= c(139, 149) , 
#       ylim= c(-43, -37), 
#       main=ids[i]);oz(add=TRUE)
#}


#########################################################
# calculate trip metrics #
#########################################################

# back to df from spatial object
#xy_df <- as.data.frame(xy_df)

##### total distance - cumulative sum along track #####

# calculate distances between every point in the dataset - i.e. assuming one big long track?? don't seperate with ID
xy_df$distances <- geodist::geodist(dplyr::select(xy_df, Latitude, Longitude), sequential=TRUE, pad=TRUE, measure= "haversine")
    # ERRORS # I've worked through - 
        #dplyr::select - helps with error: unable to determine latitude and longitude columns consider renaming
       # have to include 'measure=' argument in geodist() as the "cheap" default is extremely inaccurate past 100km


# summing NA gives NA, so have to convert to zero value . but the only NA is the very first one of the dataset aka ID2013_1 
#but there are plenty of 0s ??? is this duplicate date times? 
xy_df$distances[is.na(xy_df$distances)] = 0

# sum for each ID
## hmm this one isn't quite right, can't just sum for each id from here because we need to make the first value for each ID as zero 
# converting 1st row of each trip to zero in the distances column hhmmmmm not sure about this. it worked I think? But I think I have to remove all the ON LAND locations first?? 
xy_df <- xy_df %>%
  group_by(t_name) %>%
  mutate(distances = case_when(row_number() == 1 ~ 0,    #replace 1st row value of each group to 0
                       TRUE ~ distances))

## need to make sure it is ordered in date-time stamp?? nah shouldn't have to worry. does reordering columns in View() change the actual order of the data, don't think so 


### Add bathymetry data ### 
# export for R server and bathymetry 
#write.csv(xy_df, "xy_df_Nov2021.csv")

# read back in 
xy_df_bathy <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/Master_datafiles/xy_df_bathy.csv")

# just cut to joining columns and the bathy column, don't want duplicates of all the other columns 
xy_df_bathy <- xy_df_bathy %>% select("t_name", "Latitude", "Longitude", "gmt", "bathy")
xy_df_bathy$gmt <- as.POSIXct(strptime(xy_df_bathy$gmt, "%Y-%m-%d %H:%M:%S", tz = "GMT"))  # make sure gmt as a joining column is formatted properly 


xy_df <- full_join(xy_df, xy_df_bathy, by= c("t_name", "Latitude", "Longitude", "gmt"))

summary(is.na(xy_df)) # there are 3 NAs in bathy ... these are from the albatross island coordinates I added as new rows for those 3 at-sea starts I missed (and fixed Nov2021) - so just remove these NAs for now. Will fix istself if I redo the bathy extraction with the updated xy data 
xy_df <- na.omit(xy_df)


xy_df <- xy_df %>% mutate(bathy_category = ifelse(bathy >= -1000 & bathy <= -200 , "shelfbreak", 
                                                          ifelse(bathy <= -800, "pelagic", "shelf")))


# summary stats bathy 

xy_df %>% group_by(t_name) %>% summarise(minb = min(bathy)) %>% count(minb >= -200) #is the deepest they go DEEPER than 200 m 


##### big summary table with everything #####

# all the variables to add per trip

summary.tb <- xy_df %>% 
  group_by(t_name) %>% 
  summarise(year= first(Year), 
            bird_id=unique(NewID), 
            band=first(Band), 
            sex=first(Sex), 
            max_dist= max(disthome), 
            total_dist= sum(distances), 
            start_date= min(gmt), 
            end_date= max(gmt), 
            duration=difftime(max(gmt), min(gmt), units="days"), 
            bathyAvg = mean(bathy), 
            bathyMax = min(bathy), 
            bathyVar = var(bathy)
            ) %>% 
            mutate(A = max_dist / total_dist)

class(summary.tb$duration) <- "numeric"     
class(summary.tb$year) <- "character"  # keep 'year' as character for all the joining, then make ordered factor at the end


# error checking 
#summary.tb %>% filter(A > 0.5) %>% tally() # 13 trips have max dist greater than total - further exploration of these at the bottom of this script. 

# add max range coordinate 

maxpt <-  xy_df %>% group_by(t_name) %>% 
  filter(disthome == max(disthome)) 
#nrow(maxpt) # 321 yep

maxpt <- maxpt[,c("t_name", "Latitude", "Longitude", "gmt")]  # didn't keep disthome value, as that's already in summary.tb
colnames(maxpt) <- c("t_name", "max_lat", "max_long", "max_gmt")

summary.tb <- left_join(summary.tb, maxpt, by= "t_name")


# Calculate Tout and Tback 
# start_date, end_date, and max_gmt all need to be in "POSIXct" 
summary.tb <- summary.tb %>% mutate(Tout = difftime(max_gmt, start_date, units = "days")) %>% 
  mutate(Tback = difftime(end_date, max_gmt, units= "days"))   #

# check these are right, that Tout + Tback = duration 
#summary.tb %>% mutate(durcheck= (Tout + Tback) - duration) %>% mutate(check=if_else(durcheck == 0 , "same", "error")) %>% count(check) # 56 errors hmmmmm
# this just means the Tout and Tback aren't super robust ... come back to this if I use them 


# extra info on number of trips by individuals 
# number of trips in each deployment (unique bird/band and year combo)
summary.tb <- summary.tb %>% 
  group_by(bird_id) %>% 
  mutate(Yno.trips=(n_distinct(t_name))) 

# create column for number of unique trips taken by an individual
summary.tb <- summary.tb %>% 
  group_by(band) %>%  # need this to be BAND# not bird_id which has a Year component. 
  mutate(Tno.trips=(n_distinct(t_name))) # That worked, but need to have UB as NA 

summary.tb$Tno.trips[which(summary.tb$band == "UB")] <- NA # perfect, all UB now have NA value for this metric 

# WHAT ABOUT NUMBER OF YEARS an individual was tagged ,  use BAND HERE, not bird_id
summary.tb <- summary.tb %>% 
  group_by(band) %>%
  mutate(no.years= n_distinct(year))  # but need to get rid of UB

  summary.tb$no.years[which(summary.tb$band == "UB")] <- NA # perfect


# split up 2018 into late winter and incubation 
summary.tb <- transform(summary.tb, year = ifelse(start_date >= "2018-08-23 00:00:00" &  start_date <= "2018-09-22 00:00:00" , "latewinter2018", summary.tb$year))


# join wind as yearly value - don't want wind data for latewinter2018 so do the year categorisation before this so latewinter2018 wind values are NA
wind <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/Master_datafiles/year_wind_direction.csv")
class(wind$year) <- "character" 
summary.tb <- left_join(summary.tb, wind, by="year")


### Add demography data ### 
demog <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/Master_datafiles/demography_gps.csv")
# join the demography inforation to the trip metrics already created by year and band .. need year as well because 'age' changes 
summary.tb <- left_join(summary.tb, demog, by= c("year", "band"))

#summary.tb %>% filter(age_chicks >0) %>% count(t_name) %>% tally() # so we have age information for 222 trips?? 


### add bearing data ###  
# has to be after we've added max dist point, not for xy_df or even summary.tb

# add home lat and long column 
summary.tb$home_lat <-  -40.3780953
summary.tb$home_long <-  144.6557384

summary.tb <- mutate(summary.tb, max_bearing = # cbinding two c() makes a matrix. need a matrix here as bearing() on its own just needs two points, can't run through a bunch of rows.
                   geosphere::bearing(p1= cbind(c(summary.tb$home_long), c(summary.tb$home_lat)), 
                                      p2= cbind(c(summary.tb$max_long), c(summary.tb$max_lat)))) 

summary.tb <- summary.tb %>% mutate(hemisphere = if_else(max_bearing >= 0, "east", "west"))






#########################################################
# Updating sex #
#########################################################

# update sex based on ShyBase 

## yep this was all playing up because some F were classified as U in other years 
# do this manually 

summary.tb$sex[summary.tb$band == "13168625"] <- "F"
summary.tb$sex[summary.tb$band == "13190084"] <- "F*"
summary.tb$sex[summary.tb$band == "13193063"] <- "F"
summary.tb$sex[summary.tb$band == "13204704"] <- "F"
summary.tb$sex[summary.tb$band == "13227043"] <- "F"
summary.tb$sex[summary.tb$band == "13239898"] <- "M"
summary.tb$sex[summary.tb$band == "28005403"] <- "U" # has both M and F ??? 
summary.tb$sex[summary.tb$band == "28010664"] <- "M"
summary.tb$sex[summary.tb$band == "28010935"] <- "F"
summary.tb$sex[summary.tb$band == "28013175"] <- "M"
# new birds added Nov2021 - lost due to first loc at sea - two already correct, this one only one needing updating 
summary.tb$sex[summary.tb$band == "13168614"] <- "M*" # one was 'U' 



# Genetic vs morphometric Sex

# checking whether M* and M vs F* and F are different and then combining them 
#males <- summary.tb %>% filter(sex== "M" | sex== "M*")
#females <- summary.tb %>% filter(sex== "F" | sex== "F*")

#(duration ~ sex, data=males) #NS 
#t.test(max_dist ~ sex, data=males) #NS
#t.test(total_dist ~ sex, data=males) # * p=0.001
#t.test(A ~ sex, data=males) #NS 

#t.test(duration ~ sex, data=females) #NS
#t.test(max_dist ~ sex, data=females) #NS
#t.test(total_dist ~ sex, data=females) #NS
#t.test(A ~ sex, data=females) #p=0.002

#females %>% group_by(sex) %>% tally()

#males %>% group_by(sex) %>% tally()


# reclassifying M* and F* as M and F 
summary.tb$sex[summary.tb$sex == "M*"] <- "M"
summary.tb$sex[summary.tb$sex == "F*"] <- "F"

summary.tb %>% group_by(sex) %>% tally() # nice 




## make year a ordered factor for plotting
summary.tb$year <- factor(summary.tb$year, levels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "latewinter2018"), ordered=TRUE)

# check we've still got everything 
length(unique(summary.tb$t_name))# 321
length(unique(summary.tb$bird_id))# 139 

# need to delete the 13 trips with A > 0.5 
summary.tb <- summary.tb %>% filter(A < 0.5)

summary.tb %>% group_by(year)%>% tally()

length(unique(summary.tb$t_name))# 308
length(unique(summary.tb$bird_id))# 139 

write.csv(summary.tb, "all_trip_metrics_summary.tb_April2022.csv")


summary.tb <- read.csv("all_trip_metrics_summary.tb_April2022.csv")

######################################################################################################################################################################################
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
######################################################################################################################################################################################

#########################################################
# Table 1 of MS and in-text data summaries #
#########################################################


# number of deployments 
summary.tb %>% group_by(year) %>% 
  summarise(n_distinct(bird_id))

# number of trips
summary.tb %>% group_by(year) %>% 
  count() 

#summary deployment span 
summary.tb %>% group_by(year) %>% summarise(s=min(start_date), e=max(end_date)) %>% mutate(days=e-s)

# deployment length - calculated by hand 
# sampling resolution - from metadata/ set up notes 

# in text comment on individual birds / bands 
summary.tb %>% 
  summarise(n_distinct(band))

# tracked same bird between seasons 
summary.tb %>% #filter(year != "latewinter2018") %>% 
  group_by(year, band) %>% tally(Yno.trips > 1) %>% count( n > 1) %>% filter(`n > 1` == TRUE)



abc <- summary.tb %>% #filter(year != "latewinter2018") %>% 
  group_by(year, band) %>% count(no.years >=2)  %>% filter(`no.years >= 2` == TRUE)
length(unique(abc$band)) ## this is the number of 'bands' that were tracked more than once. Doesn't capture the number of trips captured by these individuals, i.e. if tracked over 3 years and for 12 trips. 



# duration summary
summary.tb %>% filter(year != "latewinter2018") %>% 
  summarise(max(duration), min(duration), mean(duration))
# max dist summary 
summary.tb %>% filter(year != "latewinter2018") %>% 
  summarise(max(max_dist/1000), min(max_dist/1000))
# total dist summary 
summary.tb %>% filter(year != "latewinter2018") %>% 
  summarise(max(total_dist/1000), min(total_dist/1000), mean(total_dist/1000))


#########################################################
# Figure 1 and ANOVAs #
#########################################################

# Figure 1 , boxplots by year of DURATION and A 
ab <- max_df %>% filter(year != "latewinter2018") %>% 
  ggplot(aes(year, duration)) + geom_boxplot() + 
  ylab("Duration (days)") + 
  xlab("Season") +
  theme_classic(base_size=20)

ba <- max_df %>% filter(year != "latewinter2018") %>% 
  ggplot(aes(x=year, y=A)) + geom_boxplot() + 
  ylab("A (max / total)") + 
  xlab("Season") +
  theme_classic(base_size=20)

grid.arrange(ab, ba, ncol=2)




# Summarise how wiggly tracks are 

ggplot(summary.tb) +  
  geom_histogram(aes(x=A), colour="black", fill=NA) + 
  scale_y_continuous(labels = scales::percent) +
    xlab("'S' (max / total)") +
    xlim(c(0,0.5))+ 
    theme_classic()



## regression ###
summary.ts_sanswinter <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_sanswinter$year <- factor(summary.ts_sanswinter$year, ordered=FALSE)

# ANOVA of Duration ~ Year 
m_dur <- lm(duration ~ year, data= summary.ts_sanswinter)
summary(m_dur)
anova(m_dur)

# A ~ Year
m_A <- lm(A ~ year, data= summary.ts_sanswinter)
summary(m_A)
anova(m_A)

# Total_dist ~ Year
m_td <- lm((total_dist/1000) ~ year, data= summary.ts_sanswinter)
summary(m_td)
anova(m_td)

# Max range ~ Year 
m_md <- lm((max_dist/1000) ~ year, data= summary.ts_sanswinter)
summary(m_md)
anova(m_md)

# post hoc turkey 
library(agricolae)
dur_av <- aov(m_dur)
t_dur <- HSD.test(dur_av, trt = 'year') # groups them into a ab and b 
t_dur


A_av <- aov(m_A)
t_A <- HSD.test(A_av, trt = 'year') # groups them into a ab and b 
t_A

td_av <- aov(m_td)
t_td <- HSD.test(td_av, trt = 'year') # groups them into a ab and b 
t_td

md_av <- aov(m_md)
t_md <- HSD.test(md_av, trt = 'year') # groups them into a ab and b 
t_md


par(mfrow=c(2,2))
par(mar = c(2,2,2,2))
plot(t_dur, main="duration"); plot(t_A, main="A"); plot(t_td, main="total distance"); plot(t_md, main="maximum distance")


# age 

# filter to just data we have on birds banded as chicks = known age 
summary.ts_age <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_age$year <- factor(summary.ts_age$year, ordered=FALSE)
summary.ts_age <- summary.ts_age[!is.na(summary.ts_age$age_chicks),]

summary.ts_age %>%
  ggplot(aes(x= age_chicks, y = duration)) + geom_point() + 
  theme_classic() + 
  geom_smooth(method = "lm", se = FALSE)


m_td_a <- lm(total_dist ~ 0 + age_chicks, data= summary.ts_age)
summary(m_td_a)
anova(m_td_a)

m_md_a <- lm(max_dist ~ 0 + age_chicks, data= summary.ts_age)
summary(m_td_a) 
anova(m_md_a)


m_dur_a <- lm(duration ~ 0 + age_chicks, data= summary.ts_age)
summary(m_dur_a)
anova(m_dur_a)

m_A_a <- lm(A ~ 0 + age_chicks, data= summary.ts_age)
summary(m_A_a)
anova(m_A_a)


# plot all 4 to show weak visual relationships 
par(mfrow=c(2,2))

p1 <- ggplot(data = summary.ts_age, aes(x = age_chicks, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + ggtitle("Duration vs Age")+ 
  theme_bw()
p2 <- ggplot(data = summary.ts_age, aes(x = age_chicks, y = A)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + ggtitle("A vs Age")+ 
  theme_bw()
p3 <- ggplot(data = summary.ts_age, aes(x = age_chicks, y = total_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + ggtitle("Total distance vs Age")+ 
  theme_bw()
p4 <- ggplot(data = summary.ts_age, aes(x = age_chicks, y = max_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + ggtitle("Maximum range vs Age")+ 
  theme_bw()

library(ggpubr)
ggarrange(p1,p2,p3,p4, nrow=2, ncol=2)


# sex 

# filter to just data we have on sexed birds
summary.ts_sex <- summary.tb %>% filter(year != "latewinter2018") %>% filter(sex != "U")
summary.ts_sex$year <- factor(summary.ts_sex$year, ordered=FALSE)

summary.ts_sex %>% 
  ggplot(aes(y=max_dist, x=sex)) + geom_boxplot() + 
           theme_classic()  # no big differences for the 4 metrics here 


t.test(duration ~ sex, data=summary.ts_sex) #NS
t.test(A ~ sex, data=summary.ts_sex) #NS
t.test(total_dist ~ sex, data=summary.ts_sex) #NS
t.test(max_dist ~ sex, data=summary.ts_sex) #NS


# breeding performance 

# filter to just trips we know the outcome of 
summary.ts_bs <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_bs$year <- factor(summary.ts_bs$year, ordered=FALSE)
summary.ts_bs <- summary.ts_bs[!is.na(summary.ts_bs$attempt.status),]


t.test(duration ~ attempt.status, data=summary.ts_bs) #NS
t.test(A ~ attempt.status, data=summary.ts_bs) #NS
t.test(total_dist ~ attempt.status, data=summary.ts_bs) #NS
t.test(max_dist ~ attempt.status, data=summary.ts_bs) #NS





# circular stats 

library(circular) 

# anova on max distance bearing ~ year 
summary.ts_sanswinter <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_sanswinter$year <- factor(summary.ts_sanswinter$year, ordered=FALSE)

aov.circular(circular(summary.ts_sanswinter$max_bearing, 
                      units="degrees", template="geographics"), 
             group= summary.ts_sanswinter$year, 
             method="LRT")
            # method="F.test", F.mod=TRUE)

#Circular Analysis of Variance: Likelihood Ratio Test 
#The null hypothesis being tested is that all populations also have the same mean direction.
#df:      6 
#ChiSq:   9.588 
#p.value: 0.1431 


# plot max pt ~ year 
coordinates(summary.ts_sanswinter) <- c("max_long", "max_lat") 
proj4string(summary.ts_sanswinter) <- CRS("+proj=longlat +datum=WGS84")

# run as for loop 
par(mfrow=c(2,4)); par(mar = c(1.5,1.5,1.5,1.5))

plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2013",] , xlim= c(139, 148.7) , ylim= c(-42.6, -39), main="2013", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2014",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2014", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2015",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2015", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2016",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2016", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2017",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2017", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2018",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2018", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)
plot(summary.ts_sanswinter[summary.ts_sanswinter$year=="2019",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="2019", cex.main=2);oz(add=TRUE)
points(ai, col="red", pch=13, add=TRUE)




# creating a polygon of space between these points each year - and calculating the size of the polygon 
coordinates(summary.ts_sanswinter) <- c("max_long", "max_lat") 
proj4string(summary.ts_sanswinter) <- CRS("+proj=longlat +datum=WGS84")
summary.ts_sanswinter <- as(summary.ts_sanswinter, "sf")


hull <- summary.ts_sanswinter %>% 
  st_union() %>% 
  st_convex_hull()

plot(st_geometry(hull), col = "lightgrey")
plot(st_geometry(summary.ts_sanswinter), col = "red", pch = 4, add = T);oz(add=TRUE)

st_area(hull) #292584885300 [m^2]

hull_2013 <- summary.ts_sanswinter %>% 
  filter(year=="2013") %>%
  st_union() %>% 
  st_convex_hull()

plot(st_geometry(hull_2013), col = "lightgrey")
plot(st_geometry(summary.ts_sanswinter %>% 
                   filter(year=="2013") ), col = "red", pch = 4, add = T);oz(add=TRUE)



hull_2019 <- summary.ts_sanswinter %>% 
  filter(year=="2019") %>%
  st_union() %>% 
  st_convex_hull()

plot(st_geometry(hull_2019), col = "lightgrey")
plot(st_geometry(summary.ts_sanswinter %>% 
                   filter(year=="2019") ), col = "red", pch = 4, add = T);oz(add=TRUE)


par(mfrow=c(2,1)); par(mar = c(1,1,1,1))

plot(st_geometry(hull_2016), col = "lightgrey", main="2016")
plot(st_geometry(summary.ts_sanswinter %>% 
                   filter(year=="2016") ), col = "red", pch = 4, add = T);oz(add=TRUE)

plot(st_geometry(hull_2017), col = "lightgrey", main="2017")
plot(st_geometry(summary.ts_sanswinter %>% 
                   filter(year=="2017") ), col = "red", pch = 4, add = T);oz(add=TRUE)



# bearing ~ attempt.status 
summary.ts_bs <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_bs$year <- factor(summary.ts_bs$year, ordered=FALSE)
summary.ts_bs <- summary.ts_bs[!is.na(summary.ts_bs$attempt.status),]

aov.circular(circular(summary.ts_bs$max_bearing, 
                      units="degrees", template="geographics"), 
             group= summary.ts_bs$attempt.status, 
             method="LRT")

#Circular Analysis of Variance: Likelihood Ratio Test 
#df:      1 
#ChiSq:   0.01492 
#p.value: 0.9028 



# how does variation in trip metrics correspond to variation in breeding success of tracked birds 
# summary: the variances are all unique to the units, so very hard to visualise side by side by just plotting 'variance' 
# standard deviation might be better - unified units (normal distribution) not 1000s kms of distance vs 2 days 

bs <- summary.ts_bs %>% group_by(year) %>% 
  count(as = attempt.status =="S") 

# add a row for 0% success for 2017
bs1 <- data.frame(year="2017", as=TRUE, n=0)
bs1$as <- as.logical(factor(bs1$as))
bs <- rbind(bs, bs1)

# add Year total n as column 
bsYn <- summary.ts_bs %>% group_by(year) %>% 
  count() 
bs <- left_join(bs, bsYn, by="year")
bs <- bs %>% filter(as==TRUE)

# calculate standard error of prop. success
bs <- bs %>% 
  mutate(B.S. = n.x / sum(n.y)) %>% 
  mutate(var=B.S.*(1-B.S.)) %>% #variance - nb variance isn't +/- there is one value that spans the range of data 
  mutate(se = B.S.*(1-B.S.) / n.y) # formula for standard error of a proportion

# Add whole island BS as grey dot 
ai_bs <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/Master_datafiles/ai_bs.csv")
ai_bs$year <- as.character(ai_bs$year)
bs <- left_join(bs, ai_bs, by="year")

p1 <- bs %>% group_by(year) %>%
  ggplot(aes(y=B.S., x=year) ) +
  geom_errorbar(aes(ymin= B.S.- sqrt(se), ymax=B.S.+ sqrt(se)), width=.1, position= position_dodge(0.1)) +
  #geom_line(position = position_dodge(0.1)) + 
  geom_point(position = position_dodge(0.1), size=3) + 
  geom_point(aes(y=AI_BS, x=year), color="grey") + 
  geom_errorbar(aes(ymin=AI_BS_LCI, ymax=AI_BS_UCI), width=.1) +
  ggtitle("Breeding success +/- sd") +
  xlab(NULL) + ylab("Breeding success (%)") +
  theme_classic(base_size=20)

# so standard deviation has to be for a group of values - i.e. all BS values for all years, not just one proportion. 

# boxplots \- -\ show the standard deviation 

p2 <- ggplot() + 
  geom_boxplot(data = summary.tb %>% filter(year != "latewinter2018"), #%>% filter(year != "2019"),  
             aes(x = year, y = duration)) + 
  xlab(NULL) + ylab("Duration (days)") +
   theme_classic(base_size=20) + ggtitle("Trip duration") 


p3 <- ggplot() + 
  geom_boxplot(data = summary.tb %>% filter(year != "latewinter2018"), # %>% filter(year != "2019"),  
               aes(x = year, y = A)) + 
  xlab(NULL) + ylab("'S'") +
  theme_classic(base_size=20) + ggtitle("Trip sinuosity") 

p4 <- ggplot() + 
  geom_boxplot(data = summary.tb %>% filter(year != "latewinter2018"), # %>% filter(year != "2019"),  
               aes(x = year, y = total_dist/1000)) +
  xlab(NULL) + ylab("Total distance (km)") +
  theme_classic(base_size=20) + ggtitle("Total distance") 

p5 <- ggplot() + 
  geom_boxplot(data = summary.tb %>% filter(year != "latewinter2018"), # %>% filter(year != "2019"),  
               aes(x = year, y = max_dist/1000)) + 
  xlab(NULL) + ylab("Maximum displacement (km)") +
  theme_classic(base_size=20) + ggtitle("Maximum displacement") 

ggarrange(geom_blank, p2, p3, p1, p4, p5,nrow=2, ncol=3)







# bearing ~ sex
summary.ts_sex <- summary.tb %>% filter(year != "latewinter2018") %>% filter(sex != "U")
summary.ts_sex$year <- factor(summary.ts_sex$year, ordered=FALSE)

aov.circular(circular(summary.ts_sex$max_bearing, 
                      units="degrees", template="geographics"), 
             group= summary.ts_sex$sex, 
             method="LRT")

#Circular Analysis of Variance: Likelihood Ratio Test 
#df:      1 
#ChiSq:   21.91 
#p.value: 2.863e-06 


### plot these circular sex differences 
coordinates(summary.ts_sex) <- c("max_long", "max_lat") 
proj4string(summary.ts_sex) <- CRS("+proj=longlat +datum=WGS84")

par(mfrow=c(2,2)); par(mar = c(1,1,1,1))
plot(summary.ts_sex[summary.ts_sex$sex=="M",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Male", cex.main=2);oz(add=TRUE)
plot(summary.ts_sex[summary.ts_sex$sex=="F",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Female", cex.main=2);oz(add=TRUE)

# wow 

# add circular plot with mean arrows 
summary.ts_sex <- as.data.frame(summary.ts_sex)
males <- summary.ts_sex %>% filter(sex=="M")

males_cir <- circular(males$max_bearing, units="degrees", 
                       template="geographics")
plot(males_cir)
arrows.circular(mean(males_cir), col="blue", main="Male")

females <- summary.ts_sex %>% filter(sex=="F")
females_cir <- circular(females$max_bearing, units="degrees", 
                      template="geographics")
plot(females_cir)
arrows.circular(mean(females_cir), col="red", main="Female")




# bearing ~ age - need a model for this, ANOVA just categories 
lm.circular()
aov.circular(circular(summary.tb$max_bearing, 
                      units="degrees", template="geographics"), 
             group= summary.tb$age_chicks, 
             method="LRT")

#Circular Analysis of Variance: Likelihood Ratio Test 
#df:      27 
#ChiSq:   3.065 
#p.value: 1  


# also have a look at the age of birds that visit 'pelagic areas' - bathymetry 
summary.ts_age <- summary.tb %>% filter(year != "latewinter2018")
summary.ts_age$year <- factor(summary.ts_age$year, ordered=FALSE)
summary.ts_age <- summary.ts_age[!is.na(summary.ts_age$age_chicks),]

plot(summary.ts_age$age_chicks, summary.ts_age$bathyMax) # no relationship

summary.ts_age %>%
  ggplot(aes(x= age_chicks, y = bathyMax)) + geom_point() + 
  theme_classic() + 
  geom_smooth(method = "lm", se = FALSE)

m_bathy_age <- lm(bathyMax ~ 0 + age_chicks, data=summary.ts_age)
summary(m_bathy_age)
anova(m_bathy_age)

# fit quadratic model 
summary.ts_age$age_chicks2 <- summary.ts_age$age_chicks^2
m_bathy_age_quad <- lm(bathyMax ~ 0 + age_chicks + age_chicks2, data=summary.ts_age)
summary(m_bathy_age_quad)
anova(m_bathy_age_quad)

# add quadratic line based on our model to ggplot2 
summary.ts_age %>%
  ggplot(aes(x= age_chicks, y = bathyMax)) + geom_point() + 
  theme_classic(base_size=20) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

#########################################################
# Extras - not used in MS  #
#########################################################

           
### plot trip duration based on start date ###              
# this will answer the question of whether trip duration changes can be linked to behaviour ocean, or just to do with timing / biology, any changes aren't bigger than the inherent longer trips as the season goes on 
             
max_df$starts <- paste(month(max_df$start_date), day(max_df$start_date), sep="-")    
#formatC(a, width=3, flag="0")
max_df$starts <- as.POSIXct(strptime(paste(max_df$starts), "%m-%d")) 

                   
max_df %>%
  ggplot(aes(x= starts, y = duration)) + geom_jitter() + 
  theme_classic() + 
  geom_smooth(method = "lm", se = FALSE)

   
# explore TIME OUT TIME BACK 

max_df %>% filter(year != "latewinter2018") %>% 
  ggplot(aes(as.numeric(Tout))) + geom_freqpoly(color="blue", binwidth=1) + 
  xlab("Days") + ggtitle("Blue=Time out; Green=Time back") +
  geom_freqpoly(aes(as.numeric(Tback)), color="forestgreen", binwidth=1) + 
  theme_classic() 
#You’d think that going out would be the time consuming part, and once you’ve reached the max distance, you zoom home?? 
# Maybe this has something to do with wind? 
#  You use the wind to your advantage going out? So they just use the wind to their advantage and just pick the best spot for the wind, are flexible in where they go because can’t just smack into the wind all day long to get to that one favourite spot. But coming home, you have a rigid location of where you need to get, as quickly as possible…  so it could take ageess?? # I don’t really know much about what the wind is like out there .. and whether this is a realistic idea. 

                   
                   
                   
                   




#########################################################
# check A >0.5 error. For methods text  #
#########################################################

summary.tb <- summary.tb %>% mutate(A= max_dist / total_dist)
# but should add this in up in the summary.tb creation 
summary.tb %>% filter(A >0.5) %>% nrow() # there are now 13 trips where this is the case, lets plot these 
summary.tb %>% filter(A >0.5) %>% t_name
### plot/explore these problem trips to see whats going on ###
problemids <- summary.tb %>% filter(A>0.5) %>% distinct(t_name)
b <- filter(xy_df, t_name %in% problemids$t_name) # the object after %in% needs to be a vector not a dataframe
length(unique(b$t_name)) # 13 yep good

# plot these 13 problem trips 

par(mar = c(0.1, 0.1, 0.1, 0.1))

# weird 13 ids 
coordinates(b) <- c("Longitude", "Latitude") 
proj4string(b) <- CRS("+proj=longlat +datum=WGS84")

# the colony 
ai <- data.frame("long"= 144.6557384, "lat"=-40.3780953)
coordinates(ai) <- c("long", "lat")

#just one 
problemidsB <- unique(b$trip_id)

# plot all 
par(mfrow=c(4,4))

for (i in problemidsB) {
  plot(b[b$trip_id==i, ], 
       main=problemidsB[i], 
       xlim= c(144.35249, 144.82091) , 
       ylim= c(-40.761, -40.31712));points(ai, col="red", pch=2)
}


# need to pull all points out of xy_df, not just max_df
coordinates(b) <- c("Longitude", "Latitude") 
proj4string(b) <- CRS("+proj=longlat +datum=WGS84")
plot(b)

problemids <- b %>% distinct(t_name)
par(mfrow=c(5,4)); par(mar = c(1.5, 1.5, 1.5, 1.5))


for (i in 1:18) { # making this 1:nrow(problemids) wasn't working!
  plot(
    b[b$t_name==problemids[i,],]  , 
    #xlim= c(139, 149) , # need to make these specific to each trip, not comparing plots, looking close up at each. 
    # ylim= c(-43, -37), 
    main=problemids[i,]) 
  #oz() # getting  Error: $ operator is invalid for atomic vectors
}
## hmm okay 
# step 1 - removed all trips with less than 10 locations. that fixed the funny trips that shouldn't count. 
# step 2- now the trips with A>0.5 are just sitting on water trips. good. except 2. which have 11 and 17 locations respectively. 
# step 3 - lets just cut all these. the reasons are either 1. too few locations (error blowing out and making something a trip which isn't. all under 10 locs except an n=11 and n=17 one) and 2. sitting on water trip. 


# to dos to finish this section # 
# plot and and make max distance point red 

# idea #2 
# see if the problem ids are just trips with too few locations?? but probably good call to plot first to have a look?? 
b %>% group_by(t_name) %>% count() #%>% filter(n<=10) 
# eight of them are locs less than 10... let's look overall for xy_df
xy_df %>% group_by(t_name) %>% count() %>% filter(n<=10) # only 15 trips in whole dataset have between 5-10 locs (already culled less than 5). worth culling. 


# if we cull the 15 trips with less than 10 locs, Then we have 10 problem trips left with more than 10 locs, but max > total distance. 
# would be good to have the plots of these 15 trips in supplementary to justify why we culled them. 










# visually exploring huge 5000 k trip 


id27 <- xy_df %>% filter(t_name =="ID27_2014 66")
coordinates(id27) <- c("Longitude", "Latitude") 
proj4string(id27) <- CRS("+proj=longlat +datum=WGS84")
plot(id27);oz(add=T)

summary(id27)

# animate 

library(moveVis)


all_m <- df2move(xy_df, proj=CRS("+proj=longlat +datum=WGS84"), 
                           x="Longitude", y="Latitude", time="gmt", track_id="t_name")


#align dt stamps
all_m <- align_move(all_m, res = 600 , unit = "secs")  #1200 res for 2018
# ERROR in dimnames length of 'dimnames' [1] not equal to array extent
# SOLVED maybe?: changed res = "mean" to res = 4 


get_maptypes()


## for loop for doing one each 

#frames_id27 <- frames_spatial(all_m, 
                              
                              map_service = "esri", 
                              map_type= "world_ocean_base",  ## BINGO, shows bathymetry
                             # map_service = "mapbox", map_type = "satellite",
                            #  map_token = "pk.eyJ1IjoiY2xhaXJlbWFzb24yIiwiYSI6ImNqdWoxZjEybzFnc2s0OXBiZXJ4eDc1ODgifQ.k4la5gv03sUjKqyqhHrObg", 
                              path_legend=FALSE, equidistant=FALSE) %>%

                              #path_colours = colours,
                                        #map_service="osm", map_type="toner",
                                       # map_service="mapbox", 
                                       #  map_token= "",
                                      #  map_type = "streets", # this is good, shows MPAs
                                       # map_res= 0.1, #Error in curl_download(url = url, destfile = file) : HTTP error 404.  #solution - map_res=0.1 - 0.5
                                       # path_legend=FALSE, equidistant=FALSE) %>% #, 
  #ext = extent(inc_cuttlefish)) %>%
  add_northarrow(position="upperright" , colour = "white") %>% 
  #position = "bottomleft", colour="white") %>%
  add_scalebar(distance=50) %>% #200,
  #   colour = "white") %>% #, position = "bottomleft") %>%
  add_progress() %>%
  add_timestamps(id27_m, type = "label") %>%
  add_labels(x = "Longitude", y = "Latitude")


frames_id27[[200]]


# animate frames
animate_frames(frames_id27, fps=60,
               out_file = "longest_deepest_trip_INC_id27 2014_bathy.mp4")




## for loop for doing one each 

# to do 
# add title with ID
# add extent to keep all the same /comparable 
#all_m@bbox
#library(raster) # to use extent() function 
out <- list()

for(i in 1:321) { # hmmm should be 308 - remove 13 trips that were A >0.5
   
   output1 <- frames_spatial(all_m[[i]], 
                   map_service = "esri", 
                   map_type = "world_ocean_base", 
                   path_legend=FALSE, equidistant=FALSE, 
                   ext= extent(all_m@bbox)) %>% 
             add_scalebar(distance=100, position = "bottomright") %>% add_progress() %>%
     add_timestamps(all_m[[i]], type = "label") %>%
     add_labels(x = "Longitude", y = "Latitude", title = as.character(all_m@trackId[[i]])) 
   
   out[[i]] <- output1
   
   }
 

#saveRDS(out, file = "all_frames_each_trip_seperate.rds")

# Restore the object
out <- readRDS(file = "all_frames_each_trip_seperate.rds")

out <- readRDS("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/all_frames_each_trip_seperate.rds")


for (i in 1:3) {
  animate_frames(out[[i]], fps=45, # 40 is pretty good 
                 out_file = paste0("animationnn_", i, ".mp4") ) }




 
 
 


  
