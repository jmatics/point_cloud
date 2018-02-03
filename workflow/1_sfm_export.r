###############################################################
# R Script - Calculate CSH from SFM DC and Clip to field plots
# 05-10-2017
# JW
###############################################################

rm(list=ls())
options(digits = 10, scipen=999)

library(readr)
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(rgl)
library(data.table)
library(ggplot2)

###############################################################

# Path for the TLS point cloud
sfm_path <- "D:/Data_Processing/2017/W02/Photo_Drone/w2_15052017_processing"

# Path for the DTM
#dtm_path <- "D:/Data_Processing/2017/W02/Photo_Drone/w2_15052017_processing"
dtm_path <- "D:/Data_Processing/2017/W02/TLS/w2_24052017"
  
# Path for CSH
csh_path <- "D:/Data_Processing/2017/W02/SFM_TIN/w2_15052017"

# Path for outputs
out_path <- "D:/Data_Processing/2017/W02/SFM_TIN/w2_15052017/out"

# Read DTM
setwd(dtm_path)
w2_dtm <- raster("w2_dtm_50cm.tif")
projection(w2_dtm) <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs" 
#w2_dtm <- raster("w2_15052017_sfm_dtm_min_poly.tif")
cood <- projection(w2_dtm)
plot(w2_dtm)

# Read TLS point cloud
setwd(sfm_path)
w2_sfm <- read.table("w2_15052017_sfm_pc.pts",skip=1, 
                     row.names=NULL, col.names=c("x","y","z","r","g","b","nx","ny","nz"))
head(w2_sfm)
tail(w2_sfm)

# Calculate CSH
sfm_spdf <- w2_sfm # the data frame converted to spatial point data frame (spdf)
coordinates(sfm_spdf) <- ~x+y
proj4string(sfm_spdf) <- cood

# Clip to study area
#sfm_spdf <- spdf[poly, ]
#w2_sfm2 <- as.data.frame(sfm_spdf)

# Extract height value for respective Point Cloud x,y coordinates
dtm_z <- extract(w2_dtm,coordinates(sfm_spdf))


# Calculate CSH (Actual Z - Terrain Z)
w2_sfm$csh <- sfm_spdf@data$z - dtm_z
w2_sfm <- w2_sfm[complete.cases(w2_sfm), ] # Remove NA values

csh_df <- w2_sfm
#csh_df$csh <- abs(csh_df$csh)
csh_df <- csh_df[which(csh_df$csh<=1),] # Remove csh more than 1m value
csh_df <- csh_df[which(csh_df$csh>=0),] # Remove csh less than 0m value

(dim(csh_df)[1]/dim(w2_sfm)[1])*100

# Plot CSH histogram
csh_hist <- qplot(csh_df$csh,
                  geom="histogram", binwidth = 0.01,  
                  main = "W1 SFM Point Cloud - using min z", 
                  xlab = "Crop Surface Height",  
                  fill=I("blue"), col=I("red"), 
                  alpha=I(.2), xlim=c(0,1))

csh_hist
summary(csh_df$csh)

csh_df <- csh_df[, c("x","y","csh","r","g","b")] # Remove actual z column

# Write CSH to PTS file
setwd(csh_path)
write.table(csh_df, file="w2_15052017_sfm_csh.pts", sep=" ", row.names=FALSE, col.names=FALSE)


##############################################################
# Extract csh cloud for each field plot
##############################################################

# Read filed plot polygon
setwd("D:/Data_Collection/2017/Sensor_Field_Data/W02/GIS_Data")
fp <- readOGR('.', "w2_fp_squares")

setwd(csh_path)

csh_spdf <- csh_df # Convert csh data frame to spdf
coordinates(csh_spdf) <- ~x+y
proj4string(csh_spdf) <- cood

########################## Rasterization of CSH ##########################

xrange <- csh_spdf@bbox[1,2] - csh_spdf@bbox[1,1] # x range calculation
yrange <- csh_spdf@bbox[2,2] - csh_spdf@bbox[2,1] # y range calculation

# Create empty raster with 20 cm pixel size
r_20cm <- raster(extent(csh_spdf), ncol=round(xrange/0.2), nrow=round(yrange/0.2))
projection(r_20cm) <- cood

# Calculate point density raster
point_count <- rasterize(csh_spdf@coords, r_20cm, csh_spdf@data$csh, fun="count")
plot(point_count)
# Calculate mean csh raster
mean_csh <- rasterize(csh_spdf@coords, r_20cm, csh_spdf@data$csh, fun=mean)
plot(mean_csh)

# Writting rasters
setwd(out_path)
writeRaster(point_count, filename = "w2_15052017_sfm_pointcount.tif", 
            format="GTiff", options="TFW=YES", overwrite=TRUE)
writeRaster(mean_csh, filename = "w2_15052017_sfm_mean.tif", 
            format="GTiff", options="TFW=YES", overwrite=TRUE)

###############################################################################
setwd(csh_path)
fpid <- as.character(fp@data$FP_id) # Names of the each fp
nop <- vector(mode="numeric", length=length(fp))

# Loop to clip CSH spdf for each fp
all_fp_csh <- data.frame()
for(j in 1:length(fp)){
  temp_fp <- fp[fp@data$FP_id == fpid[j], ] # Selecting fp
  temp_csh <- csh_spdf[temp_fp, ] # Clip CSH spdf from selected fp
  
  # Plotting clipped CSH
  plot(temp_csh, pch = "*", col = "green", main=fpid[j]) 
  plot(temp_fp, border="red", add=TRUE)
  
  nop[j] <- length(temp_csh)
  temp_df <- as.data.frame(temp_csh)
  temp_df$id <- rep(fpid[j], length(temp_csh))
  all_fp_csh <- rbind(all_fp_csh, temp_df)
  # Name for the PTS file
  temp_name <- paste("w2_15052017_", fpid[j], "_sfm_csh.csv", sep = "")
  # Writting df to PTS file
  write.csv(cbind(coordinates(temp_csh), temp_csh@data), 
            file = temp_name, row.names = FALSE)
  
  #rm(list = c("temp_fp", "temp_csh", "temp_df", "temp_name"))
}

nop_df <- cbind.data.frame(fpid, nop)
nop_df
write.csv(nop_df, file = "w2_15052017_sfm_csh_npoints.csv", row.names = FALSE, quote = FALSE)
all_fp_csh <- all_fp_csh[,c("id","x","y","csh","r","g","b")]
all_fp_csh$id <- factor(all_fp_csh$id)
write.csv(all_fp_csh, file = "w2_15052017_all_sfm_csh.csv", row.names = FALSE, quote = FALSE)

