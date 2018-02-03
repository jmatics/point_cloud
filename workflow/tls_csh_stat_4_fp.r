###############################################################
# R Script - Calculate CSH descriptive stats for FP level
# 17-09-2017
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
library(plyr)
library(dplyr)
###############################################################

# Path for CSH

csh_path <- "D:/Data_Processing/2017/W01/TLS_CSH/w1_23062017"
out_path <- "D:/Data_Processing/2017/W01/TLS_CSH/w1_23062017/out"

# Read fill point cloud
setwd(csh_path)
pc <- read.table("w1_23062017_pc_csh.pts",skip=1, 
                 row.names=NULL, col.names=c("x","y","csh","i"))
head(pc)
tail(pc)

# Plot CSH histogram
setwd(out_path)
png(filename="w1_23062017_tls_histo.png", width = 900, height = 566, res = 100)
ggplot(data=pc, aes(pc$csh)) + ylim(c(0,2000000)) +
  geom_histogram(breaks=seq(0, 0.8, by = 0.01), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + theme_bw() +
  labs(title="w1 TLS Point Cloud (second harvest)") +
  labs(x="TLS derived grass sward height (m)", y="Count")
dev.off()


# Read all fp point cloud
setwd(csh_path)
all_fp_csh <- read.csv("w1_23062017_all_csh.csv", header = TRUE)
head(all_fp_csh)
all_fp_csh$id <- factor(all_fp_csh$id) # Change id variable to factor
str(all_fp_csh)

# Plot CSH values for each field plot as box plots

setwd(out_path)
png(filename="w1_23062017_csh_boxplot.png", width = 900, height = 566, res = 100)
ggplot(aes(y = csh, x = id), data = all_fp_csh) +
  geom_boxplot(fill='#A4A4A4', color="forestgreen") + ylim(c(0,0.6)) + theme_bw() +
  labs(x="Field plot ID", y="Grass sward height (m)", 
       title="w1 - Sward height derived from TLS data (second harverst)") +
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
                    p98 = quantile(csh, .98))

fp_summary$freq <- plyr::count(all_fp_csh, "id")$freq # Calculate frqunecy

colnames(fp_summary)[1] <- "fp_id" # Change name "id" to "fp_id"
fp_summary$s_id <- paste("302", seq(255,272,1), sep="_") # Add sample id variable
fp_summary <- fp_summary[c(1,13,12,2:11)] # Reorder columns of the data frame

fp_summary

# Write summary stat in to a CSV file
setwd(out_path)
write.csv(fp_summary, file = "w1_23062017_fp_csh_stat.csv", row.names = FALSE)

