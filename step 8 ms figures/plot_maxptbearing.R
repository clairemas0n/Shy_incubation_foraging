########## plot max dist point and bearing 
library(ozmaps); library(circular)

## data: max_df2 


layout.matrix <- matrix(c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16), nrow = 4, ncol = 4)
              
layout(mat=layout.matrix, 
       heights = c(5,5,5,5),
       widths = c(5,5,5,5))


par(mfrow = c(4,4)); par(mar = c(1,1,1,1))

max_df2 <- 
coordinates(max_df2) <- c("max_long", "max_lat") 
proj4string(max_df2) <- CRS("+proj=longlat +datum=WGS84")

# 2013 

plot(max_df2[max_df2$year=="2013",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2013", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[1]]); arrows.circular(mean(cirY[[1]]))
arrows.circular(cirW[[1]], col="deepskyblue3", lwd=1)
nw_tally 

#2014
plot(max_df2[max_df2$year=="2014",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2014", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[2]]); arrows.circular(mean(cirY[[2]]))
arrows.circular(cirW[[2]], col="deepskyblue3", lwd=1)

# 2015
plot(max_df2[max_df2$year=="2015",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2015", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[3]]); arrows.circular(mean(cirY[[3]]))
arrows.circular(cirW[[3]], col="deepskyblue3", lwd=1)

plot(max_df2[max_df2$year=="2016",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2016", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[4]]); arrows.circular(mean(cirY[[4]]))
arrows.circular(cirW[[4]], col="deepskyblue3", lwd=1)

# 2017
plot(max_df2[max_df2$year=="2017",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2017", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[5]]); arrows.circular(mean(cirY[[5]]))
arrows.circular(cirW[[5]], col="deepskyblue3", lwd=1)

#2018
plot(max_df2[max_df2$year=="2018",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2018", adj=1, line=1.5, cex.main=2);oz(add=TRUE)
plot(cirY[[6]] ); arrows.circular(mean(cirY[[6]]))
arrows.circular(cirW[[6]], col="deepskyblue3", lwd=1)

#2019
plot(max_df2[max_df2$year=="2019",] , xlim= c(139, 148.7) , ylim= c(-42.6, -37.8)); title("2019", adj=1, line=1.5, cex.main=2);oz(add=TRUE)

plot(cirY[[7]]); arrows.circular(mean(cirY[[7]]))
arrows.circular(cirW[[7]], col="deepskyblue3", lwd=1)


mean(cir)



