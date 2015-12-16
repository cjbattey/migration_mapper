library(raster);library(ggplot2);library(RColorBrewer)
setwd("/R/migration_mapper/")

#read in rufous hummingbird eBird reports and a country outlines map
ruhu <- read.delim("data/ebd_rufhum_relAug-2015.txt",quote="")
ruhu$date <- as.Date(ruhu$OBSERVATION.DATE,"%Y-%m-%d")
ruhu$month <- as.numeric(substr(ruhu$date,6,7))
ruhu <- subset(ruhu,LONGITUDE < -96) #remove all reports east of -96 degrees longitude (this is the historic range limit for S. rufus)

#set the extent of the analysis, order is c(x-min,x-max,y-min,y-max)
ext <- extent(c(-155,-70,15,67))  

#load a map of country outlines
map <-  crop(shapefile("data/map/cntry06.shp"),ext)

#create an empty grid (aka a "raster") at 1 degree resolution. 
#Longitude ranges from -180 to 180 and latitude from -90 to 90, so set min and max accordingly. Fill all cells with 0 (vals=0) to start. 
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=0)
r.ruhu <- crop(r,ext)

#read in a list of "effort" rasters
setwd("./data/effort/")
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
  png(filename=paste("ruhu_freq_",letters[i],".png",sep=""),width=700,height=500) #open a blank file to save the output and name with the "paste" function. Use letters to make sure the files get ordered correctly for making a gif later. 
  plot(frequency,col=brewer.pal(n=7,name="YlOrRd"),legend=F,axes=F,breaks=c(1e-2,5e-2,1e-1,5e-1,1,5,10),
       main=paste(a$monthName[i]),xaxs="i", yaxs="i")+plot(map,col=NA,add=TRUE)  #print the map to file
  dev.off()  #save the png
}
setwd("/R/migration_mapper/")

