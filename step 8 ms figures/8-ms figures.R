# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #

# Script no 8

## Code updated for Ecol Evo paper, September 2023

# figures for manuscript 


# Figure 1a and 1b 

# packages
library(circular); library(tidyverse); library(geosphere);library(ozmaps);library(sf);library(gridExtra);library(sp);library(gridGraphics);library(cowplot)

# data
max_df <- read.csv("all_trip_metrics_summary.tb_April2022.csv")


## first need to calculate bearing and distance of each trip/max distance 
glimpse(max_df) # what are we working with here
# add home lat and long column 
max_df$home_lat <-  -40.3780953
max_df$home_long <-  144.6557384


max_df <- mutate(max_df, bearing = 
                   geosphere::bearing(p1= cbind(c(max_df$home_long), c(max_df$home_lat)), 
                                      p2=cbind(c(max_df$max_long), c(max_df$max_lat))))  # cbinding two c() makes a matrix. need a matrix here as bearing() on its own just needs two points, can't run through a bunch of rows. 

max_df2 <- max_df%>% filter(year != "latewinter2018")
max_df2 <- max_df2 %>% mutate(quad = ifelse(bearing >= -90 & bearing <= -5, "nw", 
                                            ifelse(bearing >= -5 & bearing <= 130, "e", "sw")))

max_df2$quad <- factor(max_df2$quad)

# Load bathymetry shapefile
bathy <- read_sf("C:/Users/mas211/OneDrive - CSIRO/2 PhD cont/R files Ch3 dwld OneDrive 7 July 23/Ch3/bathy_aus/bathy_aus/bathy_aus.shp")                          
bathy_sp <- as_Spatial(bathy)


Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44') #, '#66CCEE', '#AA3377', '#BBBBBB')

# make spatial
coordinates(max_df2) <- c("max_long", "max_lat") 
proj4string(max_df2) <- CRS("+proj=longlat +datum=WGS84")


# add dot for albatross island
ai<- data.frame(Longitude = 144.6557384, Latitude =  -40.3780953)
coordinates(ai) <- c("Longitude", "Latitude") 
proj4string(ai) <- CRS("+proj=longlat +datum=WGS84")


# set up graphics
par(mfrow=c(1, 1))
par(mar = c(5,5,5,5))

# figure 1a 

# plot points 3 sections with ozmap boundary and bathymetry
#p1 <- 
  
p1 <- plot(bathy_sp, col=alpha("grey", 0.8), xlim=c(138.5, 149), ylim=c(-43, -37.5)) + 
  points(max_df2, pch=3, lwd=2, col = Tol_bright[factor(max_df2$quad)]) +  # otherwise random colours assigned

  axis(side=1, at = c(138, 142, 146), labels = c("138°E", "142°E", "146°E")) +
  axis(side=2, at = c(-38, -40, -42), labels =  c("38°S", "40°S", "42°S") )  +
  points(ai, pch=19, col="black", lwd = 3) +


p1;ozmap(col="#DCDCDC", add=TRUE) + box()   

# Error in FUN(left, right) : non-numeric argument to binary operator = ozmap doing this... hit and miss 
 

    

    
#p1 + points(ai, pch=19, col="black", cex = 2, lwd = 3) + box()


  # covered off by figure 1b
  #legend(x="bottomleft", 
  #                 legend= c("Southwest", "Northwest", "East"), 
  #                col= c("#228833", "#4477AA","#EE6677"), 
  #                  pch=3, cex=0.75) 
  


# Figure 1b

# plot quadrant in each year

 
# convert back from spatial to dataframe
max_df2 <- as.data.frame(max_df2) 

# summary stats for stacked bar   
max_q <- max_df2 %>% group_by(year, quad) %>% summarise(n=n()) %>% mutate(freq= n/sum(n))
  
# plot 
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
  
  



# Figure 1c
# map of all tracks 

# packages
library(tidyverse); library(sp); library(trip); library(geosphere); library(tidyr); library(tibble); library(sf); 
library(oz); library(geodist); library(gridExtra); library(lubridate);library(ozmaps)

# data used 
xymap <- read.csv("C:/Users/mas211/OneDrive - CSIRO/2 PhD cont/R files Ch3 dwld OneDrive 7 July 23/Ch3/Master_datafiles/xy_trips_400m_edits_Nov2021_latest.csv")


glimpse(xymap)

#coordinates(xymap) <- c("Longitude", "Latitude") 
#proj4string(xymap) <- CRS("+proj=longlat +datum=WGS84")

ai<- data.frame(Longitude = 144.6557384, Latitude =  -40.3780953)
coordinates(ai) <- c("Longitude", "Latitude") 
proj4string(ai) <- CRS("+proj=longlat +datum=WGS84")


plot(xymap[c("Longitude", "Latitude") ], pch=1, xlim= c(139, 148.7) , ylim= c(-42.6, -37.8));ozmap(add=TRUE)
points(ai, pch="+", col="lightgrey", cex = 2, lwd = 3)



# add all 1a 1b and 1c into Inkscape and add labels and arrange

# Figure 2 # Figure 3
# in '2-repeat-use.R' script

# Figure 4
# in '1-trip-metrics.R'

# Figure 5 
# in '4-demography-data.R' script. 






