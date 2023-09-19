# simulation of movements 



x<- 1:1000 # bird number
y<- runif(1000, min=0, max=100) # distances 
xy <- as.data.frame(cbind(x, y))
View(xy)

a <- runif(10000, min=1, max=1000) 
b <- runif(10000, min=1, max=1000) 

ab <- ab %>% # add new column to ab
              # use a and b as the bird id index in xy 
              # to calculate distance between bird a and bird b (using xy values )
  as.data.frame(cbind(ceiling(xy$x), ceiling(xy$x))) # index for bird 1 and bird 2 (x in xy)
View(ab)

## how make histogram of distances in ab column c

# that's the random distribution 
# real distances from data (come from pairs, same bird different trip or H3: different year) to plot over top and compare 
# observed if same as random - means that no site fidelity - or if very close to front - going to same spot - small distances between trips 

hist(xy$y) # uniform 

# if site fidelity - first clue that they aren't randomly selecting enviroment 


summary(max_df$gmt)
