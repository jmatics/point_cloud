###############################################################
# R Script - Destructive biomass data
# 02-10-2017
# JW
###############################################################

rm(list=ls())
options(digits = 5)

library(readr)
library(data.table)
library(ggplot2)
library(plyr)
library(reshape2)
library(cowplot)
library(corrr)
library(broom)
library(ggrepel)

###############################################################

csh_path <- "D:/Data_Processing/2017/F01/SFM_TIN/f1_06062017/out"
biomass_path <- "D:/Data_Processing/2017/F01/Biomass"

setwd(biomass_path)
bm_df <- read.csv("f1_06062017_biomass.csv", header = TRUE)
bm_df$dm_p <- (bm_df$dm/bm_df$wm)*100 # Calculate % of DM
bm_df$dmy <- (bm_df$dm/bm_df$wm)*bm_df$fmy # Calculate total dry matter yield

# BM table for analysis
biomass <- bm_df[,c(1,2,3,4,8)]
biomass$fmy <- bm_df$fmy*10
biomass$dmy <- bm_df$dmy*10

setwd(csh_path)
csh_df <- read.csv("f1_06062017_fp_sfm_csh_stat.csv", header = T)

# Mergeing two data frames
merge_df <- merge(biomass, csh_df)
#merge_df <- merge_df[-c(15,17),]

sink(file = "f1_06062017_sfm_csh_vs_bm.txt")

fmy_df <- merge_df[c(4,6:16)] # Keep columns of totla biomass and stat values
# Estimate CC - total biomass vs stat values
fmy_cor <- fmy_df %>% 
  correlate() %>% 
  focus(fmy)

fmy_cor

# Plot CC - total biomass vs stat values
bar_fmy_cor <-  fmy_cor %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(fmy)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = fmy)) + ylim(c(-1,1)) +
  geom_bar(stat = "identity", width = 0.4, fill = "coral") +
  geom_text(aes(label = round(fmy, digits = 2)), size = 4) +
  ylab("Correlation Coefficient with FMY") +
  xlab("SFM derived CSH variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dmy_df <- merge_df[c(5,6:16)] # Keep columns of dry matter % and stat values
# Estimate CC - dry matter precentage vs stat values
dmy_cor <- dmy_df %>% 
  correlate() %>% 
  focus(dmy)

dmy_cor

# Plot CC - dry matter precentage vs stat values
bar_dmy_cor <-  dmy_cor %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(dmy)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = dmy)) + ylim(c(-1,1)) +
  geom_bar(stat = "identity", width = 0.4, fill = "peru") +
  geom_text(aes(label = round(dmy, digits = 2)), size = 4) +
  ylab("Correlation Coefficient with DMY") +
  xlab("SFM derived CSH variable") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(bar_fmy_cor, bar_dmy_cor, ncol = 2, nrow = 1) # Plot together

###############################################################################

### Liner Regression#######################
# Function to create regression equation for the graphs

lm_eqn <- function(lmodel){
  eq <- substitute(italic(y) == a + b %.% italic(x)*","
                   ~~italic(R)^2~"="~r2, 
                   list(a = format(coef(lmodel)[1], digits = 2), 
                        b = format(coef(lmodel)[2], digits = 2), 
                        r2 = format(summary(lmodel)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_pvalue <- function(lmodel){
  if (glance(lmodel)$p.value < 0.001){st = "***"}
  else if (glance(lmodel)$p.value >= 0.001 && glance(lmodel)$p.value < 0.01){st = "**"}
  else if (glance(lmodel)$p.value >= 0.01 && glance(lmodel)$p.value < 0.05){st = "*"}
  else if (glance(lmodel)$p.value >= 0.05 && glance(lmodel)$p.value < 0.1){st = "."}
  else if (glance(lmodel)$p.value >= 0.1 && glance(lmodel)$p.value < 1){st = "ns"}
  pvalue <- substitute(italic(p)~"="~pval*","
                       ~~star, 
                       list(pval = format(round(glance(lmodel)$p.value, digits = 2), digits = 2),
                            star = st))
  as.character(as.expression(pvalue)); 
}

lm_rmse <- function(lmodel){
  eq <- substitute(italic(RMSE)~"="~rmse, 
                   list(rmse = format(sqrt(mean(lmodel$residuals^2)), digits = 2)))
  as.character(as.expression(eq));                 
}

# Function that returns Root Mean Squared Error
rmse <- function(error){
  sqrt(mean(error^2))
}

# LM - total biomass vs CSH mean
lm_fmy <- lm(fmy~p90, data = merge_df)
summary(lm_fmy)
(lm_fmy_rmse <- rmse(lm_fmy$residuals))
(lm_fmy_Rrmse <- ((lm_fmy_rmse/mean(merge_df$fmy))*100))
(lm_fmy_Rrmse <- ((lm_fmy_rmse/diff(range((merge_df$fmy))))*100))


scat1 <- ggplot(merge_df, aes(x=p90, y=fmy)) +
          geom_point(shape=19, col = "limegreen") +  #geom_text(aes(label=s_id)) +   # Use hollow circles
          geom_smooth(method=lm) + # Add linear regression line 
          labs(x="SFM derived grass sward height p90 (m)", 
            y=expression(paste("Fresh Matter Yeild (t  ", ha^{-1},")"))) +
          geom_text_repel(aes(label = fp_id), color = 'darkgreen') +
          geom_text(x = 0.2, y = 10.0, label = lm_eqn(lm_fmy), parse = TRUE) +
          geom_text(x = 0.2, y = 9.5, label = lm_pvalue(lm_fmy), parse = TRUE) +
          geom_text(x = 0.2, y = 9.0, label = lm_rmse(lm_fmy), parse = TRUE) +
          theme_bw()


# LM - dry matter precentage vs CSH 7th percentile
lm_dmy <- lm(dmy~p90, data = merge_df)
summary(lm_dmy)
(lm_dmy_rmse <- rmse(lm_dmy$residuals))
(lm_dmy_Rrmse <- ((lm_dmy_rmse/mean(merge_df$dmy))*100))
(lm_dmy_Rrmse <- ((lm_dmy_rmse/diff(range((merge_df$dmy))))*100))

sink()

scat2 <- ggplot(merge_df, aes(x=p90, y=dmy)) +
          geom_point(shape=19, col = "limegreen") +  #geom_text(aes(label=s_id)) +  # Use hollow circles
          geom_smooth(method=lm) + # Add linear regression line 
          labs(x="SFM derived grass sward height p90 (m)", 
            y=expression(paste("Dry Matter Yeild (t  ", ha^{-1},")"))) +
          geom_text_repel(aes(label = fp_id), color = 'darkgreen') +
          geom_text(x = 0.2, y = 2.0, label = lm_eqn(lm_dmy), parse = TRUE) +
          geom_text(x = 0.2, y = 1.9, label = lm_pvalue(lm_dmy), parse = TRUE) +
          geom_text(x = 0.2, y = 1.8, label = lm_rmse(lm_dmy), parse = TRUE) +
          theme_bw()

plot_grid(scat1, scat2, ncol = 2, nrow = 1, 
          labels=c('F01 - H1'), label_size=8,
          align="hv") # Plot together

png(filename="f1_06062017_sfm_csh_stat_vs_bm.png", width = 1500, height = 943, res = 100)
plot_grid(scat1, scat2, ncol = 2, nrow = 1, 
          labels=c('F01 - H1'), label_size=6,
          align="hv") # Plot together
dev.off()
