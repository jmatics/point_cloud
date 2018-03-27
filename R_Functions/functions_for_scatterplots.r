########################################################################
# Functions that helps to add regression equaiton, R squared value, p value,
# and, RMSE value to ggplot graphs in R
# 25-10-2017
# JW
########################################################################

library(ggplot2)
library(broom)

# Inputs for the all functions are linear regression models

# Function to add regression model equation

lm_eqn <- function(lmodel){
  eq <- substitute(italic(y) == a + b %.% italic(x)*","
                   ~~italic(R)^2~"="~r2, 
                   list(a = format(coef(lmodel)[1], digits = 2), 
                        b = format(coef(lmodel)[2], digits = 2), 
                        r2 = format(summary(lmodel)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# Function to add p value and siginificant codes

# Function for exact p value

lm_pvalue <- function(lmodel){
  # Identifying what is the significant code
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

# Function for "p < 0.001"

lm_p <- function(lmodel){
  eq <- substitute(italic(p)~" < 0.001", 
                   list(rmse = format(sqrt(mean(lmodel$residuals^2)), digits = 2)))
  as.character(as.expression(eq));                 
}


# Function to add RMSE

lm_rmse <- function(lmodel){
  eq <- substitute(italic(RMSE)~"="~rmse, 
                   list(rmse = format(sqrt(mean(lmodel$residuals^2)), digits = 2)))
  as.character(as.expression(eq));                 
}

# Adding function to ggplot
#/
#geom_text(x = 0.18, y = 0.8, label = lm_rmse(lm_fmy), parse = TRUE)
#x is x location of the text in the graph area
#y is y location of the text in the graph area
#label will be assigned according to the function's output value
#/
########## END ###################
