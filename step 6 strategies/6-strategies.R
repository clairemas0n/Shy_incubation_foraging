
## Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #


# Script no 6
# dendogram for classifying foraging STRATEGIES 

library(corrplot);library(dendextend);library(ggplot2);library(ggdendro);library(psych) 


#To cluster the foraging trips into different foraging strategies, we first checked the correlation matrix of the 13 identified variables and excluded all variables that were highly correlated. The remaining variables were standardized and included in a hierarchical cluster analysis (HCA) using Euclidian distance and Ward’s method. Meaningful clusters were visually identified with the help of a dendrogram (Hennig 2015). 


# what do I want to classify my strategies, just foraging metrics, not sex or age or anything... 

# duration, A, bearing (east vs west?), total dist , max dist (but these are correlated??)
# also bathy - mean bathy and max bathy 


#max_df2013<- max_df %>% filter(year=="2013") # (year =="2016" |  year == "2015")


dendo_df <- summary.tb %>% select(c("t_name", "max_dist", "total_dist", "duration", "A", "max_bearing")) 

# add bathy 
summary.bathy <- xy_df_bathy %>% group_by(t_name) %>% summarise(avgB= mean(bathy), maxB = min(bathy), varB= var(bathy))
dendo_df <- left_join(dendo_df, summary.bathy, by = "t_name") 
is.na(dendo_df) <- NULL
nrow(dendo_df) # 318?? - 308 now 
dendo_df <- distinct(dendo_df) # 270  # update 2022,     nrow=308


## want to make a dendogram for each year... so lapply it or for loop 

#make labels as row names (don't want them as a value in clustering)
#rownames(dendo_df) <- dendo_df$t_name
#dendo_df$t_name <- NULL
#convert all categories to numeric 
#dendo_df[] <- sapply(dendo_df, as.numeric) # [] retains rownames? 
#dendo_df <- as.data.frame(dendo_df) # shit this removes the row names 
#dendo_df <- na.omit(dendo_df)

#cor(dendo_df)
#corrplot(cor(dendo_df), method="number")
#psych::pairs.panels(dendo_df) # cool vis!!!! 
# okay so max and total dist are correlated, and also with duration 
# so total dist and duration and A 
# all the bearings are correlated too 
# so just keep average bearing 



# scale all columns - mean=0 stdev=1
#dendo_df_sc <- as.data.frame(scale(dendo_df))
# east vs west category
dendo_df <- dendo_df %>% mutate(hemisphere = if_else(max_bearing >= 0, "east", "west"))
dendo_df <- dendo_df %>% mutate(NS = if_else(max_bearing >= -90 & max_bearing < 90 , "north", "south" ))

#summary.tb <- summary.tb %>% mutate(NS = if_else(max_bearing >= -90 & max_bearing < 90 , "north", "south" ))

#plot(summary.tb[summary.tb$NS == "north" ,]);oz(add=T)

###################################################################
# START HERE 
###################################################################


## i think with picking variables here, we just want to cluster trips SPATIALLY, why are they going THERE? 
# So I think picking maxbathy and maxbearing is enough 
# am going to try adding duration - to seperate long vs short trips - max distance might work better here? 
# nope adding duration ruins it 

# update 2022 - keep in late winter I think - looking at individual consistency here
#summary.tb <- summary.tb %>% filter(year != "latewinter2018")

dendo_df_hc <- dendo_df %>% select(c("t_name", "max_bearing", "maxB", "hemisphere", "NS")) #,"bathyMax", "max_dist",
rownames(dendo_df_cu) <- dendo_df_hc$t_name
dendo_df_hc$t_name <- NULL

## need to make sure now missing rows or incorrect zeros which will cluster together 
#summary(dendo_df$max_bearing)
#summary(dendo_df$bathyMax)


# need to scale these
dendo_df_hc$max_bearing <- scale(dendo_df_hc$max_bearing)
dendo_df_hc$bathyMax <- scale(dendo_df_hc$maxB)
dendo_df_hc$max_dist <- scale(dendo_df_hc$max_dist)

hc <- hclust(dist(dendo_df_hc), "complete") # other methods are "ave"  # I like the ward.D method 
plot(hc)


#dend <- as.dendrogram(hc)
#dend2 <- color_labels(dend, k = 6)
#plot(dend2)
#at height 5106.21
#dend3 <- color_branches(dend, h = 2)
#plot(dend3)
# basic option
#ggdendrogram(hc) #, theme_dendro = FALSE)
#ggdendrogram(hc, rotate=TRUE)
# Extract the data (for rectangular lines)
# Type can be "rectangle" or "triangle"
#dend_data <- dendro_data(dend, type = "rectangle")
## What contains dend_data
#names(dend_data)
#head(dend_data$labels)


# cut and plot smaller clusters 
d <- cut(as.dendrogram(hc), h=3000)
d

#dd <- dendro_data (d, type = "rectangle")


par(mfrow=c(1, 1))
plot(d$lower[[1]])
plot(d$lower[[2]])
plot(d$lower[[3]])
plot(d$lower[[4]])
plot(d$lower[[5]])
plot(d$lower[[6]])
plot(d$lower[[7]])
plot(d$lower[[8]])
plot(d$lower[[9]])
plot(d$lower[[10]])
plot(d$lower[[11]])
plot(d$lower[[12]])
plot(d$lower[[13]])
plot(d$lower[[14]])
plot(d$lower[[15]])
# do this for each year = lapply or for loop 



## so now I need to extract the t_names - and then label the groups - a/b/c/d/ etc and then add that as a "STRATEGY" column summary.tb. 
# Then can run analysis / plot the trips of different strategies 

c <-cutree(hc, h=3000) # gives each rownames/t_name a value 1:10 - the strategy
c <- as.data.frame(c)
c$t_name <- rownames(c)
rownames(c) <- NULL

summary.tb <- as.data.frame(summary.tb)
summary.tb <- left_join(summary.tb, c, by="t_name")

## okay - now to plot 

coordinates(summary.tb) <- c("max_long", "max_lat") 
proj4string(summary.tb) <- CRS("+proj=longlat +datum=WGS84")

par(mfrow=c(1, 3))
par(mar = c(1.5,1.5,1.5,1.5))

plot(summary.tb[summary.tb$c==1,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy A", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==2,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy B", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==3,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy C", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==4,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy D", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==5,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy E", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==6,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy F", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==7,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy G", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==8,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy H", cex.main=2);oz(add=TRUE)

plot(summary.tb[summary.tb$c==9,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), main="Strategy I", cex.main=2);oz(add=TRUE)




library(marmap)
marm <- getNOAA.bathy(lon1 = 139, lon2 = 148.7,
                        lat1 = -42.6, lat2 = -37.8, resolution = 10)

plot(marm, col="grey");oz(add=TRUE)

plot(marm, image=TRUE)


deep = c(-9000, -3000, 0),
shallow = c(-3000, -10, 0),
step = c(1000, 1000, 0),
lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
col = c("lightgrey", "darkgrey", "black"),




## okay not happy with the groupings of trips based on (duration, A, average bathy, bearing)

## going to try all spatial, and better representation of spatial landscape, 
# duration (for classifying trips temporally), max Bathy (better than max distance cause what shelf it is using. when I use max dist it just groups the points in circles around colony), and bearing (need better than this than degrees, it isn't picking it up properly) 

# summarise the variables of "c" 

#stacked barchart of % of strategies in each year 
summary.tb <- as.data.frame(summary.tb)
summary.tb$c[summary.tb$c == "1"] <- "A"
summary.tb$c[summary.tb$c == "2"] <- "B"
summary.tb$c[summary.tb$c == "3"] <- "C"
summary.tb$c[summary.tb$c == "4"] <- "D"
summary.tb$c[summary.tb$c == "5"] <- "E"
summary.tb$c[summary.tb$c == "6"] <- "F"
summary.tb$c[summary.tb$c == "7"] <- "G"
summary.tb$c[summary.tb$c == "8"] <- "H"
summary.tb$c[summary.tb$c == "9"] <- "I"



cTally <- summary.tb %>%
  group_by(year, c) %>% tally() %>% 
  mutate(freq = n / sum(n))

cTally$year <- factor(cTally$year, ordered=FALSE)
cTally$c <- factor(cTally$c)


# strategies/regions for success vs fail 
cTally2 <- summary.tb %>% filter(attempt.status != "NA") %>% 
  group_by(attempt.status, c) %>% tally() %>% 
  group_by(c) %>% mutate(sum= sum(n)) %>% 
  mutate(perc = n/sum)

ggplot(cTally2, aes(y=perc, x=c, fill=attempt.status)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic(base_size = 20) + 
  xlab("Strategy classification") +
  ylab("") +
  scale_fill_brewer(palette = "Spectral", name="Breeding \n attempt \n status") + 
  geom_hline(yintercept = 0.3142857)# add dashed line of average 

# average breeding success 
summary.tb %>% filter(attempt.status != "NA") %>% 
  count(attempt.status)

#dev.off() # fix for ERROR: invalid graphics state 

library(RColorBrewer)
display.brewer.all()

# by year 
ggplot(cTally, aes(y=freq, x=year, fill=c)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic(base_size = 20) + 
  scale_fill_brewer(palette = "Spectral", name="Region \n visited")


# summarise # of strategies per year 
cTally %>% group_by(year) %>% count()


## so above is spatial / habitat / location 
# and we are assuming they WANTED to go to these spots, regardless of the energy to get there. 

## now should we should look at trip shape? A and duration to tell us about energetics 



dendo_df2 <- summary.tb %>% select(c("t_name", "duration", "A")) 
rownames(dendo_df2) <- dendo_df2$t_name
dendo_df2$t_name <- NULL

## need to make sure now missing rows or incorrect zeros which will cluster together 
summary(dendo_df2$duration)
summary(dendo_df2$A)

# need to scale these
dendo_df2$duration <- scale(dendo_df2$duration)
dendo_df2$A <- scale(dendo_df2$A)


hc2 <- hclust(dist(dendo_df2), "complete") # other methods are "ave"  # I like the ward.D method 
plot(hc2)

# cut and plot smaller clusters 
d2 <- cut(as.dendrogram(hc2), h=5)
d2
plot(d2$lower[[5]])


# cut and classify trips as this strategy 
c2 <-cutree(hc2, h=5) # gives each rownames/t_name a value 1:10 - the strategy
c2 <- as.data.frame(c2)
c2$t_name <- rownames(c2)
rownames(c2) <- NULL

summary.tb <- left_join(summary.tb, c2, by="t_name")


coordinates(summary.tb) <- c("max_long", "max_lat") 
proj4string(summary.tb) <- CRS("+proj=longlat +datum=WGS84")

plot(summary.tb[summary.tb$c2==3,] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8), cex.main=2);oz(add=TRUE)


## add this back into the xy_df dataframe to plot trip shapes 

xy_df <- xy_df %>% filter(Year != "latewinter2018")

xy_df_c2 <- left_join(xy_df, c, by="t_name" )
xy_df_c2 <- xy_df_c2 %>% drop_na(c)
# for some reason it adds .x and .y and we want c2.y

coordinates(xy_df_c2) <- c("Longitude", "Latitude") 
proj4string(xy_df_c2) <- CRS("+proj=longlat +datum=WGS84")

# to add lines to points, need package 'trip' 

tr_c2 <- trip(xy_df_c2, c("gmt", "t_name", "c2" ), correct_all = TRUE)
lines(tr_c2)
plot(tr_c2, pch=".")

par(mfrow=c(1, 3))
par(mar = c(1.5,1.5,1.5,1.5))

plot(tr_c2[tr_c2$c2==1, ], pch=".", main="S.1");lines(tr_c2[tr_c2$c2==1, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==2, ], pch=".", main="S.2");lines(tr_c2[tr_c2$c2==2, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==3, ], pch=".", main="S.3");lines(tr_c2[tr_c2$c2==3, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==4, ], pch=".", main="S.4");lines(tr_c2[tr_c2$c2==4, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==5, ], pch=".", main="S.5");lines(tr_c2[tr_c2$c2==5, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==6, ], pch=".", main="S.6");lines(tr_c2[tr_c2$c2==6, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==7, ], pch=".", main="S.7");lines(tr_c2[tr_c2$c2==7, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==8, ], pch=".", main="S.8");lines(tr_c2[tr_c2$c2==8, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==9, ], pch=".", main="S.9");lines(tr_c2[tr_c2$c2==9, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==10, ], pch=".", main="S.10");lines(tr_c2[tr_c2$c2==10, ]);oz(add=T)
plot(tr_c2[tr_c2$c2==11, ], pch=".", main="S.11");lines(tr_c2[tr_c2$c2==11, ]);oz(add=T)



pd<- ggplot(summary.tb, aes(y=duration, x=factor(c2)))+ 
  geom_boxplot() + 
  theme_classic(base_size = 20)


pa <- ggplot(summary.tb, aes(y=A, x=factor(c2)))+ 
  geom_boxplot() + 
  theme_classic(base_size = 20)

ggarrange(pd, pa, nrow=1, ncol=2)

c2Tally <- summary.tb %>%
  group_by(year, c2) %>% tally() %>% 
  mutate(freq = n / sum(n))

c2Tally$year <- factor(c2Tally$year, ordered=FALSE)
c2Tally$c2 <- factor(c2Tally$c2)


ggplot(c2Tally, aes(y=freq, x=year, fill=c2)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic(base_size = 20) + 
  scale_fill_brewer(palette = "Spectral", name="Trip \n shape")
