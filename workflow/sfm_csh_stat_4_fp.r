###############################################################
# R Script - Calculate CSH descriptive stats for FP level
# 02-10-2017
# JW
###############################################################

rm(list=ls())
options(digits = 10)

library(readr)
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(rgl)
library(data.table)
library(ggplot2)
library(plyr)

###############################################################

# Path for CSH

csh_path <- "D:/Data_Processing/2017/W02/SFM_TIN/w2_15052017"
out_path <- "D:/Data_Processing/2017/W02/SFM_TIN/w2_15052017/out"

# Read fill point cloud
setwd(csh_path)
pc <- read.table("w2_15052017_sfm_csh.pts",skip=1, 
                           row.names=NULL, col.names=c("x","y","csh","r","g","b"))
head(pc)
tail(pc)

# Plot CSH histogram
setwd(out_path)
png(filename="w2_15052017_sfm_histo.png", width = 900, height = 566, res = 100)
qplot(pc$csh,
      geom="histogram", binwidth = 0.01,  
      main = "W2 SFM Point Cloud (first harvest)", 
      xlab = "SFM derived grass sward height (m)",  
      fill=I("blue"), col=I("red"), 
      alpha=I(.2), xlim=c(0,0.8))
dev.off()


# Read all fp point cloud
setwd(csh_path)
all_fp_csh <- read.csv("w2_15052017_all_sfm_csh.csv", header = TRUE)
head(all_fp_csh)
all_fp_csh$id <- factor(all_fp_csh$id) # Change id variable to factor
str(all_fp_csh)

# Plot CSH values for each field plot as box plots
setwd(out_path)
png(filename="w2_15052017_sfm_boxplot.png", width = 900, height = 566, res = 100)
ggplot(aes(y = csh, x = id), data = all_fp_csh) +
  geom_boxplot(fill='#A4A4A4', color="forestgreen") + ylim(c(0,0.8)) + theme_bw() +
  labs(x="Field plot ID", y="Grass sward height (m)", 
       title="W2 - Sward height derived from SFM (first harverst)") +
  theme(plot.title = element_text(size=12, face="bold", vjust=2, hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# Calculate CSH summary stats for each field plot 
# min, max, mean, median, q25, q75, q90, q95, q98
fp_summary <- ddply(all_fp_csh, "id", summarise, 
                    min = min(csh),
                    max = max(csh),
                    mean = mean(csh),
                    median = median(csh),
                    std = sd(csh),
                    p25 = quantile(csh, .25),
                    p75 = quantile(csh, .75),
                    p90 = quantile(csh, .90),
                    p95 = quantile(csh, .95),
                    p98 = quantile(csh, .98),
                    sum = sum(csh))

fp_summary$freq <- plyr::count(all_fp_csh, "id")$freq # Calculate frqunecy

colnames(fp_summary)[1] <- "fp_id" # Change name "id" to "fp_id"
#sid <- c(seq(101,114), 116, 118)
#fp_summary$s_id <- paste("302", sid, sep="_")
fp_summary$s_id <- paste("302", seq(201,218,1), sep="_") # Add sample id variable
fp_summary <- fp_summary[c(1,14,13,2:12)] # Reorder columns of the data frame

fp_summary

# Write summary stat in to a CSV file
setwd(out_path)
write.csv(fp_summary, file = "w2_15052017_fp_sfm_csh_stat.csv", row.names = FALSE)

