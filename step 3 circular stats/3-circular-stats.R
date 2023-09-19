

# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #


# Chapter 3 - script 3

# This is the second stage. I have defined trips and calculated trip metrics in script 1. Now I am exploring the max distance point for each trip, and metrics associated with that. Including its bearing and distance from home. 

### To dos ### 
#1# need to check for bugs as heaps of duplicates in data frame I am reading in - see meeting notes with Alistair 


#required packages 
library(circular); library(tidyverse); library(geosphere);library(ozmaps);library(sf);library(gridExtra);library(sp);library(gridGraphics);library(cowplot)
#data frame to read in 
#max_df <- read.csv("max_dist_pt_data_July2021.csv")
max_df <- read.csv("all_trip_metrics_summary.tb_April2022.csv")

# explore object class=circular 
#data(fisherB13)
#glimpse(fisherB13)
#circular(fisherB13) # data from package
#plot(fisherB13$set1)
#plot.circular(fisherB13$set1)

## first need to calculate bearing and distance of each trip/max distance 
glimpse(max_df) # what are we working with here
# add home lat and long column 
max_df$home_lat <-  -40.3780953
max_df$home_long <-  144.6557384


max_df <- mutate(max_df, bearing = 
                  geosphere::bearing(p1= cbind(c(max_df$home_long), c(max_df$home_lat)), 
                   p2=cbind(c(max_df$max_long), c(max_df$max_lat))))  # cbinding two c() makes a matrix. need a matrix here as bearing() on its own just needs two points, can't run through a bunch of rows. 

#max_df$X <- NULL
#max_df$X.2 <- NULL
#max_df$X.1 <- NULL
#max_df <- distinct(max_df)


#now I have a bearing for each max dist - I can do up the circular plots?? 
max_df2 <- summary.tb %>% filter(year != "latewinter2018")

cir <- circular(max_df2$max_bearing, units="degrees", template="geographics")
head(cir)
plot(cir) #WOW 
str(cir)
mean(cir)
arrows.circular(mean(cir))### AVERAGE FOR ALL TRACKS IS DEAD ON WEST 


## FOR EACH YEAR WHAT how many trips go to NW bearing?
# tag NW locations 
max_df2 <- as.data.frame(max_df2)
max_df2$quad <- NULL

max_df2 <- max_df%>% filter(year != "latewinter2018")
max_df2 <- max_df2 %>% mutate(quad = ifelse(bearing >= -90 & bearing <= -5, "nw", 
                                          ifelse(bearing >= -5 & bearing <= 130, "e", "sw")))



# make column of colour based on quad 
#max_df2 <- as.data.frame(max_df2)
max_df2$quad <- factor(max_df2$quad)
max_df2 <- max_df2 %>% mutate(Colour = ifelse(quad== "ne","#EE6677",
                                              ifelse(quad== "nw", "#228833",
                                                     ifelse(quad=="se", "#4477AA","#CCBB44" ))))

  




# Load shapefile
bathy <- read_sf("C:/Users/mas211/OneDrive - CSIRO/2 PhD cont/R files Ch3 dwld OneDrive 7 July 23/Ch3/bathy_aus/bathy_aus/bathy_aus.shp")                          
bathy_sp <- as_Spatial(bathy)

# make smaller - gClip ??
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44') #, '#66CCEE', '#AA3377', '#BBBBBB')


coordinates(max_df2) <- c("max_long", "max_lat") 
proj4string(max_df2) <- CRS("+proj=longlat +datum=WGS84")

par(mfrow=c(1, 1))
par(mar = c(5,5,5,5))

#p<- plot(max_df2, pch=3, lwd=2, col = Tol_bright[factor(max_df2$quad)], 
 #    xlim= c(139, 148.7) , ylim= c(-42.6, -37.8));oz(add=TRUE)
#par(xaxs="i", yaxs="i")
#par(pty="s")
#b <- plot(r.marm, col = gray.colors(100, alpha = 0.5),
#          xlim=c(138.5, 149), ylim=c(-43, -37.5), xlab="Longitude",  ylab="Latitude")

# plot points - 3 sections 
p1 <- plot(bathy_sp, col=alpha("grey", 0.8), xlim=c(138.5, 149), ylim=c(-43, -37.5)) + points(max_df2, pch=3, lwd=2, col = Tol_bright[factor(max_df2$quad)]) +  # otherwise random colours assigned
  ozmap(col="#DCDCDC", add=TRUE) + 
  axis(1, at = c(138, 142, 146), labels = c("138°E", "142°E", "146°E")) +
  axis(2, at = c(-38, -40, -42), labels =  c("38°S", "40°S", "42°S") ) + 
  box() # having this later on draws over it all so solid line surrounding 



## how do i save a base plot as an object?! 
library(gridGraphics) # convert into savable object
p1 <- recordPlot()
plot.new() ## clean up device
p1 # redraw
library(cowplot)
#p2 <- grid.draw(as_grob(p1)) # convert into 'grob' for use in gridarrange 
p2 <- as_grob(p1)


# p1 <- p1 + legend(x="bottomleft", 
#                   legend= c("Southwest", "Northwest", "East"), 
#                   col= c("#228833", "#4477AA","#EE6677"), 
#                   pch=3, cex=0.75) 



# plot quadrant in each year
#Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44') #, '#66CCEE', '#AA3377', '#BBBBBB')
max_df2 <- as.data.frame(max_df2)

max_q <- max_df2 %>% group_by(year, quad) %>% summarise(n=n()) %>% mutate(freq= n/sum(n))
      
p_bp <- ggplot(data=max_q, aes(fill=quad, x=year, y=freq)) +
  geom_bar(position="stack", stat="identity") +
  xlab("")+ ylab("") + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(name="", 
                      breaks=c("e",   "nw", "sw") , 
                   labels=c("East",  "Northwest",  "Southwest"), 
                   values= c( "#EE6677",  "#228833", "#4477AA")) +
  theme_classic(base_size = 20) +
  theme(axis.text=element_text(color="black", windowsFont("Arial")))
  

                                        

# FIGURE 1 of MS 

#library(gridExtra)
#grid.arrange(p1, p_bp, ncol=2) #, #labels= c("A", "B"))
par(mfrow=c(1, 2))
p1 
p_bp 

#arrangeGrob(p2, p_bp, ncol=2)
grid.arrange(p2, p_bp,  ncol=2)


summary.tb %>% filter(total_dist > 2000000) %>% tally()

## this marmap stuffs up the extent when plotting. Just read in OGR bathy from Shp 

#library(marmap)
#raster::extent(max_df2) # when spatial 
#marm <- getNOAA.bathy(lon1 = 138.5, lon2 = 149,
                  #    lat1 = -44, lat2 = -37, resolution = 15, 
                  #    )

#r.marm <- as.raster(marm)
#plot(r.marm)
#marmP <- spTransform(r.marm, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(marm, col="grey");oz(add=TRUE)

#plot(marm, image=TRUE)
#

#deep = c(-9000, -3000, 0),
#shallow = c(-3000, -10, 0),
#step = c(1000, 1000, 0),
#lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
#col = c("lightgrey", "darkgrey", "black"),








plot(summary.tb[summary.tb$c==2,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy B", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==3,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy C", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==4,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy D", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==5,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy E", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==6,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy F", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==7,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy G", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==8,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy H", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==9,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy I", cex.main=2);oz(add=TRUE)




nw_tally <- max_df %>% group_by(year, nw) %>% tally() %>% mutate(freq = n / sum(n))
w_tally <- max_df %>% group_by(year, w) %>% tally() %>% mutate(freq = n / sum(n))

w_tally %>% group_by(year) %>% 
  filter(w=="w") %>% 
  ggplot(aes(x=year, y=freq)) + 
  geom_col(fill="grey") +
  geom_text(aes(label = round(freq, digits = 2)), vjust = -0.5) + 
  theme_classic() + 
  theme(text=element_text(size=20))





# In text summary! 
max_df %>% group_by(nw) %>% tally() %>% mutate(freq = n / sum(n))
max_df %>% group_by(w) %>% tally() %>% mutate(freq = n / sum(n))


# just relabel "2018 late winter" and "2018"
# work out the earliest date our incubation trip started - in all other years
# cut the 2018 data then. 
# then can look at 2018 winter as comparison 
# I should do this back at original data level? 

glimpse(maxdf)

maxdf %>% group_by(Year) %>% summarise(min(start_date))
maxdf %>% group_by(Year) %>% summarise(min(end_date))

#maxdf <- transform(maxdf, Year = ifelse(start_date >= "2018-08-23 00:00:00" &  start_date <= "2018-09-22 00:00:00" , "latewinter2018", maxdf$Year))

glimpse(maxdf)
View(maxdf) #beautiful 

#split the data into groups to graph seperately

cirY <- lapply(unique(max_df2$year),FUN = function(x){
  cirYx <- max_df2[max_df2$year==x,]
  cirs <- circular(cirYx$bearing, units="degrees", template="geographics")
  return(cirs)
})


# Plots for quandrant 

cirY <- lapply(unique(max_df2$year),FUN = function(x){
  cirYx <- max_df2[max_df2$year==x,]
  cirs <- circular(cirYx$bearing, units="degrees", template="geographics")
  return(cirs)
})

summary(cirY)
par(mfrow=c(2,4))
par(mar = c(2,2,2,2))
plot(cirY[[1]], main="2013"); arrows.circular(mean(cirY[[1]]))
plot(cirY[[2]], main="2014"); arrows.circular(mean(cirY[[2]]))
plot(cirY[[3]], main="2015"); arrows.circular(mean(cirY[[3]]))
plot(cirY[[4]], main="2016"); arrows.circular(mean(cirY[[4]]))
plot(cirY[[5]], main="2017"); arrows.circular(mean(cirY[[5]]))
#plot(cirY[[6]], main="latewinter2018"); arrows.circular(mean(cirY[[6]]))
plot(cirY[[6]], main="2018"); arrows.circular(mean(cirY[[6]]))
plot(cirY[[7]], main="2019"); arrows.circular(mean(cirY[[7]]))

# Need to run hypothesis tests (with distance included as well as bearing) for the following comparisons 



kuiper.test(max_df2$bearing)
rao.spacing.test(max_df2$bearing)
rayleigh.test(max_df2$bearing)
wallraff.test(cirY)


### nope the one to do is 
?aov.circular()

# need to convert to circular but keep year as 'names' 


# plot wind speed to add 
w <- read.csv("year_wind_direction.csv")
cirW <- lapply(unique(w$year),FUN = function(x){
  cirYx <- w[w$year==x,]
  cirs <- circular(cirYx$meandir, units="degrees", template="geographics")
  return(cirs)
})







#############YESS THIS IS IT ! 
# TRY REMOVING late winter and seeing if all these tests are still significant
cirYears <- circular(max_df2$bearing, units="degrees", template="geographics", names= max_df2$year)
str(cirYears)

aov.circular(circular(max_df2$bearing, units="degrees", template="geographics"), group=max_df2$year, method="LRT")
            

## okay  now some repeat use tests 
#1	By year
#2 Same bird, same year
#3 Same bird, different year



# maybe just append bearing to previous data from script no.2 
# yep done 
max_df
cir <- circular(max_df$bearing, units="degrees", template="geographics")
plot(cir)
# package:circular, distance between two bearings? 

 
dcir <- dist.circular(cir, method =    "angularseparation",
                        
                        #"geodesic", #"angularseparation" , # "chord",
                      diag=FALSE, upper=FALSE) #Distance Matrix Computation for Circular Data
summary(dcir)
hist(dcir, main="angularseparation")


# convert out of dist object
mAng <- reshape2::melt(as.matrix(dcir))
#mGeo <- melt(as.matrix(dcir))
#mCho <- melt(as.matrix(dcir))

hist(mAng$value)

# plot freq distributions as percentages not total freq

p1<- ggplot(mAng, aes(x = value)) + 
  geom_histogram(aes(y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50)+  # LIGHT = RANDOM INDIVIDUAL 
  #scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(limits=c(0, 8e5)) + 
  theme_classic() + 
  labs(x = "degrees between max distance bearing") +
       # title ="angular seperation") + 
  #scale_y_continuous(limits=c(0,0.08),labels = scales::percent) +
  #scale_x_continuous(limits=c(0, 2)) + 
  theme_classic() 

p2 <- ggplot(mGeo, aes(x = value)) + 
  geom_histogram(aes(y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50) + # LIGHT = RANDOM INDIVIDUAL 
  #scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(limits=c(0, 8e5)) + 
  theme_classic() + 
  labs( x = "distance between max distance points", 
        title =
          "geodesic") + 
  scale_y_continuous(limits=c(0,0.08),labels = scales::percent) +
  scale_x_continuous(limits=c(0, 3)) + 
  theme_classic() 



p3 <- ggplot(mCho, aes(x = value)) + 
  geom_histogram(aes(y = stat(count) / sum(count)),
                 fill=yarrr::transparent("gray14", trans.val = .5), bins = 50) + # LIGHT = RANDOM INDIVIDUAL 
  #scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(limits=c(0, 8e5)) + 
  theme_classic() + 
  labs( x = "distance between max distance points", 
        title =
          "chord") + 
  scale_y_continuous(limits=c(0,0.08), labels = scales::percent) +
  scale_x_continuous(limits=c(0, 2)) + 
  theme_classic() 


par(mfrow=c(3,1))
require(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)





devtools::install_github("ropenscilabs/ochRe")
library(ochRe)
#test
pal <- colorRampPalette(ochre_palettes[["galah"]])
#image(volcano, col = pal(20)) TEST






##############################
# repeat use # 
#############################


# need a distance matrix between all the distance (in radians?) between two trips 

# method needs to assume flat surface - cause they are all relatively close together, don't need to account for curvature of the earth 
dcir <- dist.circular(cir, method =    "geodesic", #"angularseparation" , # "chord",
                      diag=FALSE, upper=FALSE) #Distance Matrix Computation for Circular Data
summary(dcir)
hist(dcir, main="angularseparation")

# dcir - is all possible combos 


# now need to select same IDs to look at within and between years 


 # EG within a year 
y2013 <- max_df %>% filter(year=="2013") %>% #61 trips 
  filter(Yno.trips >1 & band != "UB")

















