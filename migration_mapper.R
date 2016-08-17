#run this if you don't have these packages installed: 
#install.packages('raster');install.packages('ggplot2');install.packages('RColorBrewer')
library(raster);library(ggplot2);library(RColorBrewer);library(data.table);library(magrittr);library(viridis)
#setwd("/R/migration_mapper/")

#read in rufous hummingbird eBird reports and a country outlines map
ruhu <- fread("~/Documents/ruhu/sampling/ebd_rufhum_relNov-2014/ebd_rufhum_relNov-2014.txt")
ruhu <- ruhu[,list(LONGITUDE,LATITUDE,`OBSERVATION DATE`)]
ruhu$`OBSERVATION DATE` <- as.Date(ruhu$`OBSERVATION DATE`,"%m/%d/%Y")
ruhu$month <- as.numeric(substr(ruhu$`OBSERVATION DATE`,6,7))
ruhu$year <- as.numeric(substr(ruhu$`OBSERVATION DATE`,1,4))
#colnames(ruhu) <- c("row","long","lat","month")
#ruhu <- subset(ruhu,long < -96) #remove all reports east of -96 degrees longitude (this is the historic range limit for S. rufus)

#set the extent of the analysis, order is c(x-min,x-max,y-min,y-max)
ext <- extent(c(-155,-65,15,67))  

#load a map of country outlines
map <-  crop(shapefile("data/map/cntry06.shp"),ext)

#create an empty grid (aka a "raster") at 1 degree resolution. 
#Longitude ranges from -180 to 180 and latitude from -90 to 90, so set min and max accordingly. Fill all cells with 0 (vals=0) to start. 
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=0)
r.ruhu <- crop(r,ext)

#read in a list of "effort" rasters
setwd("/R/migration_mapper/data/effort/")
files <- list.files() #letters in file names are in order of months, Jan-Dec. 
effort <- lapply(files,FUN=function(e) crop(raster(e),ext))
setwd("/R/migration_mapper/")

#loop through months, printing a map for each month to a pdf file. 
setwd("/R/migration_mapper/png/")
for(i in c(1:12)){
  a <- subset(ruhu,month == i)   #subset the reports from one month
  locs <- SpatialPoints(data.frame(a$LONGITUDE,a$LATITUDE))    #pull the longitude and latitude (x and y coordinates) from the full dataset and format as SpatialPoints
  nRUHU <- rasterize(locs,r.ruhu,fun="count")    #count the number of points per grid cell with the "rasterize" function
  nReports <- effort[[i]]   #load the effort grid for "i"th month
  frequency <- nRUHU/nReports  #divide RUHU reports by the number of all reports in each grid cell to get the frequency of reports
  frequency <- data.frame(rasterToPoints(frequency))
  #png(filename=paste("ruhu_freq_",letters[i],".png",sep=""),width=700,height=500) #open a blank file to save the output and name with the "paste" function. Use letters to make sure the files get ordered correctly for making a gif later. 

  ggplot()+coord_map()+xlim(-150,-65)+ylim(15,65)+theme_bw()+
    theme(panel.grid=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())+
    scale_fill_gradientn(colors=rev(viridis(10)),values=seq(0,1,.05),name="Report\nFrequency",limits=c(0,1))+
    geom_path(data=fortify(map),aes(x=long,y=lat,group=group),col="grey",lwd=.4)+
    stat_summary_2d(data=frequency,aes(x=x,y=y,z=layer,fun="mean"),bins=50,alpha=0.75)+
    ggtitle(i)
  ggsave(filename=paste("ruhu_freq_",letters[i],".png",sep=""))
  
  #plot(frequency,col=rev(viridis(20)),legend=T,axes=F,breaks=seq(0,1,.05),
       #main=paste(i),xaxs="i", yaxs="i")+plot(map,col=NA,border="grey",add=TRUE)  #print the map to file
  #dev.off()  #save the png
}
setwd("/R/migration_mapper/")

#let's break that down: 
#first we pull out just one month of reports. Let's do June as an example
  a <- subset(ruhu,month == 6)
#then we get x,y coordinates for all the rufous hummingbird reports
  locs <- SpatialPoints(data.frame(a$long,a$lat))
#now we "rasterize" the points. a raster is just a grid. We want to set the value of each grid square to the number of reports inside it. 
  nRUHU <- rasterize(locs,r.ruhu,fun="count")
#load up the "effort" grid for each month. 
  nReports <- effort[[6]]
#then we'll divide by the total number of ebird reports (for ANY species) in that grid cell. Why do we need to do this?
  frequency <- nRUHU/nReports
#the png function creates an empty image file. 
#we're using the paste function to make custom names for each file by cycling through the letters of the alphabet: paste("ruhu_freq_",letters[i],".png",sep="")
  #png(filename=paste("ruhu_freq_",letters[i],".png",sep=""),width=700,height=500)
#then (finally) plot it!
  plot(frequency,col=brewer.pal(n=7,name="YlOrRd"),legend=F,axes=F,breaks=c(1e-2,5e-2,1e-1,5e-1,1,5,10),
       main=paste(a$monthName[i]),xaxs="i", yaxs="i")+plot(map,col=NA,add=TRUE)  
#the dev.off() function tells R to close and save the png.
  #dev.off()
  #then load it up to an online service to make it a gif (there are lots: google it!)