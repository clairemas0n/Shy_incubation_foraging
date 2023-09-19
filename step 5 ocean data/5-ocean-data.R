
#########################################################
# ocean metrics #
#########################################################


# script no. 5 
###### THIS HAS TO BE DONE ON THE SERVER !!!!!


xy_df_bathy <- read.csv("xy_df_bathy.csv")
glimpse(xy_df_bathy)


xy_df_bathy <- xy_df_bathy %>% mutate(shelfbreak = ifelse(bathy >= -1000 & bathy <= -200 , "shelfbreak", 
                                           ifelse(bathy <= -800, "pelagic", "shelf")))



bathyTally <- xy_df_bathy %>% filter(Year != "latewinter2018") %>% 
                         group_by(Year, shelfbreak) %>% tally() %>% 
                         mutate(freq = n / sum(n))

bathyTally$shelfbreak <- ordered(bathyTally$shelfbreak, levels=c("shelf", "shelfbreak", "pelagic"))


  
  ggplot(bathyTally, aes(fill=shelfbreak, y=freq, x=Year)) +
    scale_fill_discrete(name = "Bathymetry") +
    geom_bar(position="stack", stat="identity") + 
    theme_classic(base_size = 15) + scale_color_brewer(palette = "PuOr")
  
  
  ### Summary stats for bathymetry results section 
 
  xy_df_bathy %>% filter(Year != "latewinter2018") %>% 
    group_by(t_name, shelfbreak) %>% tally() %>% 
    mutate(freq = n / sum(n)) %>% 
    summarise()
  

  # x % of trips visited the shelf break 
 
  
  # 318 trips WITHOUT winter 
  xy_df_bathy %>% filter(Year != "latewinter2018") %>% distinct(t_name) %>% nrow()
  
  
  xy_df_bathy %>% filter(Year != "latewinter2018") %>% filter(shelfbreak == "shelfbreak") %>% distinct(t_name) %>% nrow()
  
  
  # x % of trips visited pelagic waters 
  xy_df_bathy %>% filter(Year != "latewinter2018") %>% filter(shelfbreak == "pelagic") %>% distinct(t_name) %>% nrow()
  
  
min(xy_df_bathy$bathy)



nw_tally %>% group_by(year) %>%  
  ggplot(aes(x=year, y=freq)) + 
  geom_col(fill="grey") +
  geom_text(aes(label = round(freq, digits = 2)), vjust = -0.5) + 
  theme_classic() + 
  theme(text=element_text(size=15))









# for xy_df - all locations - make new column with read_bathy()
coordinates(xy_df) <- c("Longitude", "Latitude") 
proj4string(xy_df) <- CRS("+proj=longlat +datum=WGS84")

xy_df <- as.data.frame(xy_df)





### SST EXAMPLE 
xy_df$sst <- raster::extract(readsst, xy_df[c("Longitude", "Latitude", "gmt")] )
# what i did for georgia was exporting a raster, I need to extract point values 

plot(xy_df$gmt, xy_df$sst, pch  =".") # very cool when xy_df isn't spatial

ggplot(xy_df, aes(gmt, sst, colour = NewID)) + geom_point(pch = ".") +  guides(colour= F)

plot(xy_df$Longitude, xy_df$Latitude, col, xlim = c(138, 150), pch = ".", asp = 0.6)

ggplot(xy_df, aes(x=Longitude, y=Latitude, color=sst))