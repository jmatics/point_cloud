######################################################################
# Script for point cloud PTS file read and clip
# 08-08-2017
# JW
######################################################################

# Load Libraries
library(data.table)
library(sp)
library(rgdal)
library(raster)

######################################################################

# Read TLS point cloud

pts_path <- "D:/Data_Processing/2017/W02/TLS/w2_15082017"
setwd(pts_path)

pts.load <- fread("TLS_w2_15082017.pts",skip=1) # read pts file line using fread
names(pts.load) <- c("x", "y", "z","int") # Provide coulmn names

head(pts.load)
tail(pts.load)

######################################################################

# Read polygons

poly_path <- "D:/Data_Collection/2017/Sensor_Field_Data/W02/GIS_Data"
setwd(poly_path)

poly <- readOGR(dsn = ".", layer = "w2_poly") # read polygon shapefile
cood <- proj4string(poly) # extract CRS

######################################################################

# Change point cloud df to spdf (spatial point data frame)

pc.df <- as.data.frame(pts.load)
pc.spdf0 <- pc.df
coordinates(pc.spdf0) <- ~x+y # define x,y coordinates
proj4string(pc.spdf0) <- cood # define CRS

# remove redundant variables if memory problem come
rm(pts.load)
rm(pc.df)

######################################################################

# Clip the point cloud spdf to the polygon boundary

pc.spdf <- pc.spdf0[poly, ] # extracting

######################################################################

# Save clipped data as a shapefile

out_path <- "D:/Data_Processing/2017/W02/TLS/w2_15082017"
setwd(out_path)

writeOGR(pc.spdf, ".", "w2_15082017_pc", "ESRI Shapefile") # write as a shapefile

write.csv(pc.spdf, file = "w2_15082017_pc.csv", row.names = FALSE) # write data as a csv

######################################################################
