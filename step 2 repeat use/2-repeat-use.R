

# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #


# Script no 2
# repeat use /site fidelity of individuals 

# This is testing theories of individual consistency/specialisation for shy albatross. Three hypotheses we have are: 
#1. go where you always go
#2. go where your friends go 
#3. go where is good (using enviro cues)

# run separately for (i) all trips, (ii) within year (and random distribution from random pairs each year, e.g. 1000 samples, aggregated across all years, e.g. 8 x 10,000 samples), and (iii) same bird across years (vs random pairs from all years).



# To dos # 

#1# ensure G-test is the best test for our situation https://en.wikipedia.org/wiki/G-test


#########################################################
# create distance matrices #
#########################################################

library(tidyverse); library(sp); library(reshape2); library(yarrr); library(DescTools)


# simulation of movements to demonstrate process - did this with Alistair in a meeting  
#x<- 1:1000 # bird number
#y<- runif(1000, min=0, max=100) # distances 
#xy <- as.data.frame(cbind(x, y))
#View(xy)

#a <- runif(10000, min=1, max=1000) 
#b <- runif(10000, min=1, max=1000) 

#ab <- ab %>% # add new column to ab
  # use a and b as the bird id index in xy 
  # to calculate distance between bird a and bird b (using xy values )
#  as.data.frame(cbind(ceiling(xy$x), ceiling(xy$x))) # index for bird 1 and bird 2 (x in xy)
#View(ab)

## how make histogram of distances in ab column c

# that's the random distribution 
# real distances from data (come from pairs, same bird different trip or H3: different year) to plot over top and compare 
# observed if same as random - means that no site fidelity - or if very close to front - going to same spot - small distances between trips 

#hist(xy$y) # uniform 

# if site fidelity is present - first clue that they aren't randomly selecting environment 



# (i) # all trips 

# create a distance matrix between all the duplicated bands
# create second distance matrix between all possible combinations of non-duplicated bands - maybe just remove all individuals where we captured more than one trip (either within or between years) so we aren't accidently incorporating duplicates in this test

# read in data 
#max_df <- read.csv("C:/Users/cjmason2/OneDrive - University of Tasmania/PHD WORK/2 R files/Ch3/all_trip_metrics_summary.tb_Nov2021.csv")

max_df <- summary.tb

#write.csv(max_df, "max_dist_pt_data_July2021.csv")
# random comparison # 
# first, remove all duplicates from the data

# how many duplicates will we be removing 
max_df %>% summarise(dup=duplicated(max_df$band)) %>% count(dup) # 95 no repeat trips, no multiple years ; 210 repeat trips or multiple years

# so really here, do we want to just grab one trip from each ID so we have plenty to compare, or ONLY look at birds where we only captured one trip, one time. 

#


all_indivs <- max_df[!duplicated(max_df$band),]
length(unique(all_indivs$band)) # 95 yep ## 2022 update, actually 97. need to delete UB? yep, now 96

all_indivs <- all_indivs %>% filter(band !="UB")

# create matrix of distances between points 
coordinates(all_indivs) <- c("max_long", "max_lat") 
proj4string(all_indivs) <- CRS("+proj=longlat +datum=WGS84")

all_indivs_dm <- geosphere::distm(all_indivs) # create distance matrix 
View(all_indivs_dm)
hist(all_indivs_dm)

# just keep one triangle aka just unique pairs 
all_indivs_dm[lower.tri(all_indivs_dm)] <- 0
all_indivs_dm <- melt(all_indivs_dm)
all_indivs_dm <- all_indivs_dm %>% filter(value > 0)
nrow(all_indivs_dm) # 4465 comparisons here # 2022 update (with UB removed), 4560
hist(all_indivs_dm$value)



#####################################
# (ii) #  within year 
#####################################
#(& random distribution from random pairs each year, e.g. 1000 samples, aggregated across all years, e.g. 8 years times 10000 comparisons each year)
# need to do a for loop here and loop through the duplicates in each year 
# run for within each year, then aggregate all so just the one plot 

## from adding in new columns (no. of trips etc) in script1 on 22/7/21 it will be easier to filter trips into these groupings i.e. if(Tno.trips>1 ...)

# okay so step 1 - need to look at duplicate trips within each year (must be linked by same band!) and then aggregate all distances into one

# in 2013 --> for band=123 --> we have captured n=5 trips, --> calculate all pairwise comparisons here (5*5) but remove duplicates i.e. a*c vs c*a are the same

# or I need duplicate(band) %>% group_by(year) # nope this wouldn't work because some of them might be just between years there're duplicate but only one trip in a year 

# okay let's just start with doing it for one year -2013


unique(max_df$year) # has late winter 2018 as a seperate year
# 2022 update, combine 2018 winter and 2018 - will make "within year" results stronger, greater N repeat trips for each individual, but same no. of individuals 
max_df$year[max_df$year == "latewinter2018"] <- "2018"
max_df$year <- factor(max_df$year)




y2013 <- max_df %>% filter(year=="2013") %>% #61 trips 
 filter(Yno.trips >1 & band != "UB") # more than one trip captured by a deployment , 12 trips (16 if UB still there)
#n_distinct(y2013$band) # 5 birds, and 12 trips

y2014 <- max_df %>% filter(year=="2014" & Yno.trips >1 & band != "UB")
y2015 <- max_df %>% filter(year=="2015" & Yno.trips >1 & band != "UB")
y2016 <- max_df %>% filter(year=="2016" & Yno.trips >1 & band != "UB")
y2017 <- max_df %>% filter(year=="2017" & Yno.trips >1 & band != "UB")
y2018 <- max_df %>% filter(year=="2018" & Yno.trips >1 & band != "UB")
y2019 <- max_df %>% filter(year=="2019" & Yno.trips >1 & band != "UB")
# lets use lapply 

year2013 <- lapply(unique(y2013$band),FUN = function(x){
    sub.dat <- y2013[y2013$band==x,]
    coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
    proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
   y2013_dists <- geosphere::distm(sub.dat)
      return(y2013_dists)
  })
year2014 <- lapply(unique(y2014$band),FUN = function(x){
  sub.dat <- y2014[y2014$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
 
})

year2015 <- lapply(unique(y2015$band),FUN = function(x){
  sub.dat <- y2015[y2015$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
  
})
year2016 <- lapply(unique(y2016$band),FUN = function(x){
  sub.dat <- y2016[y2016$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
  
})
year2017 <- lapply(unique(y2017$band),FUN = function(x){
  sub.dat <- y2017[y2017$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
  
})
year2018 <- lapply(unique(y2018$band),FUN = function(x){
  sub.dat <- y2018[y2018$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
  
})
year2019 <- lapply(unique(y2019$band),FUN = function(x){
  sub.dat <- y2019[y2019$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
  
})

within_birds <- c(year2013, year2014, year2015, year2016, year2017, year2018, year2019) # only 33 combos for a band within a year 

within_birds <- melt(within_birds)
within_birds <- within_birds%>% filter(value > 0) %>% distinct(value)
hist(within_birds$value) 




# okay lets  try for all # 

########## DID NOT WORK !!!!!

# nope just had to do it for each year and then combine them at hte end. 
# but below is an impressive attempt at lapply to do all at once! 

# make precursors 
years<- unique(max_df$year)
aout <- list()

#for loop - create list which each years duplicated bands seperated
for (i in 1:length(years)) {

a <- paste0("year", years[i])   
a <- max_df %>% filter(year==years[i]) #61 trips 
a <- a %>% filter(Yno.trips >1 & band != "UB") # remove UB birds 

aout[[length(aout) + 1]] <- a  ## this half is working.. maybe finish the loop here and do the lapply below seperately???

}

# aout is a list of 7 - each year has its own distance matrix 

## now run the distance matrix calculation over each split level of aout (list of years)


# this lapply isn't working --- maybe it never did?? 
lapply( aout, FUN= function(x) {
  
  lapply(unique(aout[[x]]$band),
         
         FUN = function(z){
           
           bands <- aout[[x]][aout[[x]]$band==z,] #Error in aout[[x]] : invalid subscript type 'list
           coordinates(bands) <- c("max_long", "max_lat")  # make it a spatial object 
           proj4string(bands) <- CRS("+proj=longlat +datum=WGS84")
         })
           
           ## up to here
          
           d <- geosphere::distm(bands) 
           dms_within[[length(dms_within) + 1]] <- d
           #return(d)

b <- paste0("bands", years[i])
b  <- lapply(unique(a$band), 
        
      FUN = function(x){
  
      bands <- a[a$band==x,]
      coordinates(bands) <- c("max_long", "max_lat")  # make it a spatial object 
      proj4string(bands) <- CRS("+proj=longlat +datum=WGS84")
      
      
      d <- paste0("dists", years[i])
      d <- geosphere::distm(bands) 
      dms_within[[length(dms_within) + 1]] <- d
      #return(d)

   }) })





#########################################################
# (iii) same bird across years (vs random pairs from all years)

#2# BETWEEN YEARS 

##############################################################


max_df %>% count()
max_df %>% filter(no.years >1) %>% count() # wow that's lots??? 212 out of 318. but maybe just lots of trips from those IDS
# I want to calculate no.years again - with 2018 and late winter combined 
unique(max_df$year) # latewinter still in there as factor, but not level
max_df <- max_df %>% 
  group_by(band) %>%
  mutate(no.years= n_distinct(year))  # but need to get rid of UB

max_df$no.years[which(max_df$band == "UB")] <- NA # perfect



# randomly select one trip from each year for each ID 
between <- max_df %>% filter(no.years>1) %>% group_by(year, band) %>% sample_n(1) # 55, update 2022: 47

nrow(between) # 2022 update, 47 

for(i in 1:100){
  between <- between %>%
    bind_rows(max_df %>% 
                     filter(no.years>1) %>% 
                     group_by(year, band) %>% 
                     sample_n(size=1, replace=FALSE) %>%
                     mutate(bs=i) %>%
                     ungroup())
}
  


between_birds <- lapply(unique(between$band), FUN = function(x){
  sub.dat <- between[between$band==x,]
  coordinates(sub.dat) <- c("max_long", "max_lat")  # make it a spatial object 
  proj4string(sub.dat) <- CRS("+proj=longlat +datum=WGS84")
  geosphere::distm(sub.dat)
})


# yep output should be 31 unique band combos, = 31 list slots 
# but just ignore from 21 onwards where they are empty cause no.years used latewinter2018 

# update 2022, 22 list lists 

between_birds <- melt(between_birds)
between_birds <- between_birds%>% filter(value > 0) %>% distinct(value)
hist(between_birds$value)  # only 27, need to pick other random trips and do again?! bootstrap automate it??
# no matter if I bootstrap to 100 or 1000, still leaves us with 91 unquie comparisons..
# need to think about why this is... is there a minimum number of calculations 



###
# okay so we know have a big chunky histogram for SAME bands BETWEEN years
between_birds



# now to randomly compare bands between years - OR COULD WE JUST COMPARE TO THE ALL??
# yep compare to all - that's what i'm doing
#between_r <- max_df %>% #filter(year!="latewinter2018") %>% 
#  group_by(year) %>% 
#  sample_n(1, replace=TRUE) 
#summary(between_r)





# this is all the duplicated bands from all years, and also duplicates WITHIN a year. need to split this up into the two groups. 
# not sure how to go about choosing a trip from each year (just first? ) and then also do we just need to choose two trips within a year, not ALL trips a bird does in a year?? 
# have a think about this .... 

########### I think this is redundant - have got between and within distance matricies, and the ALL to compare them too... 

#dups <- max_df[duplicated(max_df$band) | duplicated(max_df$band, fromLast = TRUE), ] # got off google , not sure how it works , but it does
#dups <- dups %>% filter(band != "UB")


#dups %>% group_by(year, band) %>% sample_n(1) %>% nrow() # 43 unique bands not groupin by year,  grouping by year = 83 trips ## update 2022 (incl. late winter) top_n = get 217 (selecting by hemisphere), sample_n = get 68


#d <- dups
# make distance matrix 
#coordinates(d) <- c("max_long", "max_lat")  # make it a spatial object 
#proj4string(d) <- CRS("+proj=longlat +datum=WGS84")
#distancesm <- geosphere::distm(d)



### for loop to run all comparisons ###

#dms <- list() # create blank list to store outputs of for loop 
#dms_prop <- list() # create another for proportional outputs
# for loop - just got this from lots of trial and error and exploring individual elements in the loop (above)


#for (i in unique(dups$band))  {
##  a <- dups[dups$band== i,]
  
#  coordinates(a) <- c("max_long", "max_lat") 
#  proj4string(a) <- CRS("+proj=longlat +datum=WGS84")
#  dm <- geosphere::distm(a)
  #dms <- list(dms, dm)
#  dms[[length(dms) + 1]] <- dm
  # need to add another output in here which is proportional distances based on n of each group - NO do this at plotting level
  # b <- matrix(c(rep(nrow(a))), c(rep(nrow(a))), c(rep(nrow(a))), c(rep(nrow(a))))
  # dms_prop[[length(dms_prop) + 1]] <- dm/b
#}

# should be 43 slots
# there are 43 duplicated bands
#dms
#dms_prop # yew this works 

#head(dms)
#length(dms)
#str(dms)


### plotting frequency distributions
# and proportional frequency data

#dms #is list of matrices for each 43 bands that repeat 
#dm # distance matrix of all combos 

#reshape into vectors of unique distances 
#library(reshape2)
#dm_43 <- melt(dms, varnames = c("row", "col"))
#dm_all <- melt(dm, varnames = c("row", "col"))

#dm_43 <- unique(dm_43[3])  # is less than half of the matrix as it is removing all the 0s too 
#nrow(dm_43)

#dm_all <- unique(dm_all[3])  # is less than half of the matrix as it is removing all the 0s too 
#nrow(dm_all)


#########################################################
# plot frequency distributions #
#########################################################

# now I need to make a frequency table with bins 
library(yarrr)


# convert distance from m to km 
head(all_indivs_dm)
all_indivs_dm <- all_indivs_dm %>% mutate(value_km = value*0.001)
head(between_birds)
between_birds <- between_birds %>% mutate(value_km = value*0.001)


#########################
# BETWEEN YEARS GRAPH 

between_graph <- ggplot(all_indivs_dm, aes(x = value_km)) + 
  geom_histogram(aes(y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray70", trans.val = .5), bins = 50) + # LIGHT = RANDOM INDIVIDUAL 
  #scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(limits=c(0, 8e5)) + 
  labs( x = "Distance (km) ",
        y="", title= "Between years", tag = "A") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=20),
        plot.tag = element_text(size=30), 
        axis.text.x = element_text(color="black", size=15), 
              axis.text.y = element_text(color="black", size=15), 
        axis.title.x = element_text(size=15)) +
      # title =#        "same individuals tracked in different years (dark, n=91) vs random individuals (light, n=8930)") + 
  

  
  
  geom_histogram(data=between_birds, aes(x = value_km, y = stat(count) / sum(count)), 
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50) + #DARK = SAME individual 
  
             scale_y_continuous(limits=c(0, 0.2),labels = scales::label_percent(accuracy = 1L), expand = c(0, 0)) +
   scale_x_continuous(expand = c(0, 0)) # makes data start from axis, no white space 
  



#par(mfrow=c(1,2)); par(mar = c(2,2,2,2))


# within 



# convert distance from m to km 
#head(all_indivs_dm)
#all_indivs_dm <- all_indivs_dm %>% mutate(value_km = value*0.001)
head(between_birds)
within_birds <- within_birds %>% mutate(value_km = value*0.001)


###########################
# WITHIN YEARS GRAPH 

within_graph <- ggplot(all_indivs_dm, aes(x = value_km)) + 
  geom_histogram(aes(y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray70", trans.val = .5), bins = 50) + # LIGHT = RANDOM INDIVIDUAL 

  labs( x = "Distance (km) ",
        y="", title= "Within years", tag = "B") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=20),
        plot.tag = element_text(size=30), 
        axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15), 
        axis.title.x = element_text(size=15)) +

  
  
  geom_histogram(data=within_birds, aes(x = value_km, y = stat(count) / sum(count)), 
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50) + #DARK = SAME individual 
  
  scale_y_continuous(limits=c(0, 0.2), labels = scales::label_percent(accuracy = 1L), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) # makes data start from axis, no white space 
    








library(gridExtra)

grid.arrange(between_graph, within_graph, nrow = 1)

#########################################################
# statistics #
#########################################################

# proportional - observed vs expected 

# observed = same individual 
# expected = comparisons between random individuals 

# testing null hypothesis - are two samples independent (H0) or not (HA)

library(DescTools)
?GTest()

# needs to be count data - aka the freq tables that we plotted in ggplot 
#geom_histogram(data=between_birds, aes(x = value, y = stat(count) / sum(count)))


between_birds <- between_birds %>% mutate(percent=(value / sum(value)))

between_birds <- between_birds %>% mutate(x_bins = cut(percent, breaks = 10))
between_birds %>% count(x_bins) ## THIS IS OUR FREQ TABLE 


# make the random dataset have the same binning 
between_bins <- cut(between_birds$percent, breaks = 20)
levels(between_bins)

# between years random dataset 
#nrow(between_birds)
#between_random <- all_indivs_dm %>% sample_n(nrow(between_birds))

# keep proportions for all 8000
#all_indivs_dm  %>% mutate(x_bins = cut(percent, breaks = levels(between_bins)))

# hmm it has saved the bins funny .. might have to convert to numeric by hand .... levels(between_bins)

# okay same process for random dataset - use all 8000
between_random <- all_indivs_dm %>% mutate(percent=(value / sum(value)))
between_random <- between_random  %>% mutate(x_bins = cut(percent, breaks = c(0.000254, 0.00512, 0.00994, 0.0148, 0.0196, 0.0292, 0.034, 0.0389, 0.0437, 0.0486)))
between_random %>% count(x_bins)

between_birds %>% count(x_bins)

# random birds aren't splitting hte same, lets look at their cut 
between_bins_random  <- cut(between_random $percent, breaks = 10)
levels(between_bins_random)
min(levels(between_bins_random))
min(levels(between_bins))


###########
# should the bins be selected based on equal distance
# or by samples equally distributed
# overall, 
# cut function - should we open it on the right or left (lowest/highest) - hmmmm no?


# combine both into freq table 

freq_between1 <- between_birds %>% count(x_bins)
freq_between2 <- between_random %>% count(x_bins)

freq_between <- full_join(birds=freq_between1, random=freq_between2, by="x_bins")
colnames(freq_between) <- c("x_bins", "nbirds", "nrandom")
freq_between[is.na(freq_between)]<- 0
freq_between <- freq_between[-c(11), ]  

# one didn't fit in the 

GTest(freq_between[,c(2,3)], 
      correct="none")   



############# G TEST FOR WITHIN
#within years 



within_birds <- within_birds %>% mutate(percent=(value / sum(value)))

within_birds <- within_birds%>% mutate(x_bins = cut(percent, breaks = 10))
within_birds %>% count(x_bins) ## THIS IS OUR FREQ TABLE 


# make the random dataset have the same binning 
within_bins <- cut(within_birds$percent, breaks = 10)
levels(within_bins)



# calculate random values n=75 
nrow(within_random)
within_random <- all_indivs_dm %>% sample_n(nrow(within_birds))
within_random <- within_random %>% mutate(percent=(value / sum(value)))
within_random <- within_random %>% mutate(x_bins = cut(percent, breaks = c(0.000306, 0.00616, 0.012, 0.0178, 0.0236, 0.0294, 0.0352, 0.041, 0.0468, 0.0526, 0.0584)))
within_random %>% count(x_bins)
# hmm it has saved the bins funny .. might have to convert to numeric by hand .... levels(between_bins)

# combine both into freq table 

freq_within1 <- within_birds %>% count(x_bins)
freq_within2 <- within_random %>% count(x_bins)

freq_within <- full_join(freq_within1,freq_within2, by="x_bins")
colnames(freq_within) <- c("x_bins", "nbirds", "nrandom")
freq_within[is.na(freq_within)]<- 0


GTest(freq_within[,c(2,3)], 
      correct="none")   



# nope i've set this up wrong 
GTest(observed=within_birds$value,
      expected=within_random$value,
      correct="none")   



?GTest() # better for smaller sample sizes (i.e. if an expected or an observed value has less than 5 obs)


chisq.test(freq_within[,c(2,3)]) # 0.003
chisq.test(freq_between[,c(2,3)]) # 0.0007


##############################
# repeat use trip sinuosity 
##############################

# okay
max_df <- summary.tb

# I need A for all individuals 
head(max_df)


hist(max_df$A) # THIS IS THE RANDOM COMPARISON 
hist(withinA$A)


# WITHIN YEAR 
# now just for 


#### pick IDs where more than one trip for an individual within a year
withinA <- max_df %>% group_by(year) %>% 
  filter(Yno.trips >1 & band != "UB")  # this is 199 trips?? #check this
withinA <- as.data.frame(withinA)
# order trips in order 
class(withinA$start_date)
withinA$start_date <- as.POSIXct(strptime(withinA$start_date, "%Y-%m-%d %H:%M:%S", tz = "GMT"))  
withinA <- withinA %>% arrange(band) %>% arrange(start_date)
# now need to compare subsequent trips by calculating 'A - lag(A)'

#make blank data
within_df <- data.frame(matrix(NA, 1, ncol=22))
colnames(within_df) <- colnames(withinA)
within_df <- within_df[-c(1), ]

lwithin <- lapply(unique(withinA$band), FUN = function(x){
  dat.band <- withinA[withinA$band==x,]
  bandx.calc <- dat.band %>% mutate(diffA = lag(A)- A) 
 # bandx.calc <- bandx.calc %>% mutate(tripN = ) # WHICH TRIP NUMBER IS IT? IMPORTNAT FOR GRAPHING
  })

within_Adiffs <- data.frame(Reduce(rbind, lwithin)) # YEW BEAUTIFUL 

# now I need to number each trip within 

within_Adiffs  <- within_Adiffs %>% group_by(band) %>% mutate(tripnumber=1:n())

plot(within_Adiffs$diffA, within_Adiffs$tripnumber)
#abline(x=0)


## better vis like Alistair suggested
deltaS <- ggplot(within_Adiffs, aes(x = diffA)) + 
  geom_histogram(aes(y = ..density..), colour="grey", fill=NA) +
  geom_density() +
  geom_vline(aes(xintercept = 0),
             linetype = "dashed", size = 0.6) +
  theme_classic(base_size = 20) + 
  xlab(expression(Delta ~S)) + ylab("Density") + 
  labs(tag="A", title="Within years") +
  theme(axis.text.x = element_text(colour="black", size=15), 
        axis.text.y = element_text(colour="black", size=15), 
        plot.tag = element_text(size=40), )


lm <- lm(tripnumber ~ diffA, data=within_Adiffs)
summary(lm)
abline(6.399 , 4.690)


# ggplot 
ggplot(within_Adiffs,aes(tripnumber, diffA)) +
  geom_point() + theme_classic() + 
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)




## plot histograms overlayed 
ggplot() + 
  geom_histogram(data=max_df, aes(x=A, y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray70", trans.val = .5), bins = 50) + scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits=c(0, 8e5)) + 

geom_histogram(data=withinA, 
  aes(x = A, y = stat(count) / sum(count)), 
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50) + #DARK = SAME individual 
 # scale_y_continuous(labels = scales::percent) +
#  scale_x_continuous(limits=c(0, 8e5)) + 
  theme_classic() 
#+ 
#  labs( x = "max range / total trip distance", 
#        title ="same individuals tracked in different years (dark, #n=199) vs random individuals (light, n=305)")

nrow(withinA)



### now let's do trips between years - 
# Can we use ALL trips by an individual in a year because we didn't find a difference 
# or just use trip 1 (more similar time than later in the season aka choosing random)

## redo 'no.years' calculation so it doesn't include latewinter2018
#max_df <- read.csv("max_dist_pt_data_July2021.csv")

### update 2022, include latewinter2018 as 2018
#max_df$year[max_df$year == "latewinter2018"] <- "2018"
#max_df$year <- factor(max_df$year)


#max_df <- max_df %>%
 # group_by(band) %>% 
 # mutate(no.years= n_distinct(year))  

#max_df$no.years[which(max_df$band == "UB")] <- NA


betweenA <- max_df %>% group_by(year) %>% 
  filter(no.years >1 & band != "UB")  # 57 now , update 2022, nrow=114


#betweenA$start_date <- as.POSIXct(strptime(betweenA$start_date, "%Y-%m-%d %H:%M:%S", tz = "GMT")) 

# for each band, there should only be one trip in each year
# here we are selecting the first trip - should we select random (update 2022)
betweenA <- betweenA %>% group_by(band, year) %>%
  slice_min(start_date)  #45 - update 2022, 47


# 55 no matter what way band, year are arranged?? BUT there are still lone bands???# Looks like they are only in ONE year so the no.years calculated is flawed??  YEP based on including latewinter2018 as a different year. 

# keep only instances with more than one year 
#13250179
#13250178
#13250177
#13227082
#13210172
#13206787 # both 2018
#13194859 # both 2018
#13168614 # both 2018

betweenA <- betweenA %>% arrange(band) %>% arrange(start_date)




#make blank data
#between_df <- data.frame(matrix(NA, 1, ncol=22))
#colnames(between_df) <- colnames(betweenA)
#between_df <- between_df[-c(1), ]

 # don't need lapply? Can just do dplyr 
between_Adiffs <- betweenA %>% group_by(band) %>% arrange(start_date) %>% mutate(diffA = A - lag(A))

## 2013 isn't showing up - Adiff=NA, is it because other years are stealing the A to subtract from??


plot(between_Adiffs$year, between_Adiffs$diffA) 

#lm <- lm(year ~ diffA, data=between_Adiffs)
#summary(lm)

#ggplot(between_Adiffs,aes(year, diffA)) +
#  geom_point() + theme_classic() + 
#  stat_summary(fun.data=mean_cl_normal) + 
#  geom_smooth(method='lm', formula= y~x)


# plot A for first trips vs second trips vs third - as box plot 

betweenA2 <- betweenA %>% group_by(band) %>% arrange(start_date) %>% mutate(ordertrip= row_number())
 


 trips123<-  ggplot(betweenA2) + 
  geom_boxplot(aes(y=A, x=as.factor(ordertrip)), colour="black") + 
    theme_classic(base_size= 20) + 
    ylab("S") + xlab(NULL) +
    labs(tag="B", title="Between years")+
    scale_y_continuous(limits= c(0,0.5))+
    scale_x_discrete(labels=c("1" = "First trip", "2" = "Second trip",
                                "3" = "Third trip"))+ 
    theme(axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black"), 
          plot.tag = element_text(size=40))

  
  
  # Figure 3 
  
  library(gridExtra)
  
  grid.arrange(deltaS, trips123, nrow=1)


