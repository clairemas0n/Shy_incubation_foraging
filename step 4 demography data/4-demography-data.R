

# Claire Mason's PhD : Ch3 #
# claire.mason@utas.edu.au #


# Script no 4
# adding in demography/breeding data from ShyBase for individuals that have been tracked
 
library(tidyverse);library(ggarrange)


# I created a spreadsheet in excel by hand looking at ShyBase- BirdID and AttemptID and ResightID
# I think there is a README notebook file explaining what I did. So maybe put it in here? 

# Using ShyBase version updated by Sheryl in July 2021
demog <- read.csv("Master_datafiles/demography_gps.csv")



# import demography data - by hand in excel 
max_df <- read.csv("Ch3 old files/max_dist_pt_data_September2021.csv")
glimpse(max_df)
max_df$duration<- as.numeric(max_df$duration)
max_df$max_dist<- as.numeric(max_df$max_dist)
max_df$total_dist<- as.numeric(max_df$total_dist)
max_df$A <- as.numeric(max_df$A)


# join the demography inforation to the trip metrics already created by year and band 
max_df_demog <- full_join(max_df, demog, by= c("year", "band"))


View(max_df_demog) # wow 

max_df_demog <- max_df_demog %>% filter(year!= "latewinter2018")
#df_demog <- df_demog %>% filter(year!= "2019") # don't have status info here? cause of covid19

#max_df_demog$age_chicks <- as.numeric(max_df_demog$age_chicks)

#summary(max_df_demog$age_chicks)
#length(unique(max_df_demog$age_chicks))
max_df_demog %>% filter(age_chicks >0) %>% distinct(t_name) %>% tally()

max_df_demog <- max_df_demog %>% filter(age_chicks >0)

  
plot(max_df_demog$age_chicks)
hist(max_df_demog$age_chicks)
# plot
#max_df_demog <- unique(max_df_demog)



ggplot(data= max_df_demog) +
geom_boxplot(aes(x=attempt.status, y=age_chicks)) + theme_classic() #
#stat_summary(fun.data=mean_cl_normal) + 
#  geom_smooth(method='lm', formula= y~x)

lm <- lm(attempt.status ~ age_chicks, data=max_df_demog)
summary(lm)



#scatterplot matrix 
#subset to just numeric vars
#glimpse(df_demog)
#df_demog$max_dist <- as.numeric(df_demog$max_dist)
#df_demog$max_long <- as.numeric(df_demog$max_long)
#df_demog$max_lat <- as.numeric(df_demog$max_lat)

#num.data <-df_demog[ , c(6,10,11,12,14, 23,26,27,31,32,33,38)]
#plot(num.data)

#max_df_demog$age_chicks <- as.integer(max_df_demog$age_chicks)

a <-ggplot(max_df_demog,
       aes(y=A, x=age_chicks)) +
  xlab("Age") + ylab("'S'") +
  geom_point() + theme_classic(base_size=15) +
  geom_smooth(method = "lm", level=0.95) #, se = FALSE)

b<-ggplot(max_df_demog,
       aes(y=duration, x=age_chicks)) +
  xlab("Age") + ylab("Duration (days)") +
  geom_point() + theme_classic(base_size=15) +
  geom_smooth(method = "lm", level=0.95) 

c<-ggplot(max_df_demog,
       aes(y=max_dist/1000, x=age_chicks)) +
  xlab("Age") + ylab("Maximum displacement (km)") +
  geom_point() + theme_classic(base_size=15) +
  geom_smooth(method = "lm", level=0.95) 

d<-ggplot(max_df_demog,
       aes(y=total_dist/1000, x=age_chicks)) +
  xlab("Age") + ylab("Total distance (km)") +
  #scale_y_continuous(labels = scales::comma) +
  geom_point() + theme_classic(base_size=15) +
  geom_smooth(method = "lm", level=0.95) 

#library(ggpubr)
ggarrange(a, b, c, d, ncol=2, nrow=2)


length(unique(max_df_demog$band))

lm <- lm((max_dist/1000) ~ 1 + age_chicks, data=max_df_demog )
summary(lm)
anova(lm)
plot(lm)


lm <- lm(duration ~ 1 + age_chicks, data=max_df_demog )
summary(lm)
anova(lm)

plot(lm)

ggplot(max_df_demog,
       aes(x=attempt.status, y=age_chicks)) +
  geom_boxplot() + theme_classic() 
 # geom_smooth(method = "lm") 




max_df_demog$age_chicks <- as.integer(max_df_demog$age_chicks)


lm <- lm(A ~ age_chicks, data=max_df_demog)
summary(lm)




#### year and 






# PREVIOUS ATTEMPTS DATA IS FLAWED 
lm <- lm(duration~no.previous.attempts, data=df_demog )
summary(lm)
plot(lm)


df_demog_c <- df_demog %>% filter(!is.na(attempt.status)) %>% droplevels()

df_demog_c$attempt.status <- as.factor(df_demog_c$attempt.status)


lm <- lm(no.previous.attempts ~ attempt.status, data=df_demog_c)
summary(lm) # no effect 





# Try big logistic regression for success/fail 

model <- glm(viability ~ 1 + Region + Type + Sum.neighbours + 
               Distance.edge + Region:Sum.neighbours + Region:Distance.edge + 
               Sum.neighbours:Distance.edge,
             # autocorrelation lat/long one, 
             family = binomial(link="logit"),   data = df_april) 
