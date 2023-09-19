

library(tidyverse); library(sp); library(reshape2); library(yarrr); library(DescTools)

# read in data 
max_df <- read.csv("max_dist_pt_data_July2021.csv")




# within year 

y2013 <- max_df %>% filter(year=="2013") %>% #61 trips 
  filter(Yno.trips >1 & band != "UB") # more than one trip captured by a deployment , 12 trips (16 if UB still there)
#n_distinct(y2013$band) # 5 birds, and 12 trips

y2014 <- max_df %>% filter(year=="2014" & Yno.trips >1 & band != "UB")
y2015 <- max_df %>% filter(year=="2015" & Yno.trips >1 & band != "UB")
y2016 <- max_df %>% filter(year=="2016" & Yno.trips >1 & band != "UB")
y2017 <- max_df %>% filter(year=="2017" & Yno.trips >1 & band != "UB")
y2018 <- max_df %>% filter(year=="2018" & Yno.trips >1 & band != "UB")
y2019 <- max_df %>% filter(year=="2019" & Yno.trips >1 & band != "UB")


# compare differences in max distance 

maxdistBird1 - maxdistBird2