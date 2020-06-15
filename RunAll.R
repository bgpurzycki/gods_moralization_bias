################################################################################
library(rgdal)
library(classInt)
library(RColorBrewer)
library(rethinking) 
library(maps)
library(maptools)
library(scales)
library(sp)
library(plotrix)
library(ggmap)
library(ggrepel)
library(ggplot2)
library(reshape2)
library(ineq)
library(directlabels)
library(scales)
library(MASS)
library(lme4)
library(xtable)
library(doBy)
library(mapproj)

set.seed(8675309)

cbbPalette <- c(
  "#000000", 
  "#E69F00", 
  "#009E73", 
  "#0072B2", 
  "indianred", 
  "#CC79A7")
  
 setwd("C:/Users/cody_ross/Dropbox/Open Papers/Moralization Bias/NewWorkflow") 
 setwd("C:/Users/Mind Is Moving/Dropbox/Open Papers/Moralization Bias/NewWorkflow") 
 #setwd("NewWorkflow")  
####################################################################### Make Map
source("./Code/map.R")

######################################################### Make Descriptive Table
source("./Code/descriptive.R")

########################################################## Make Descriptive Plot
source("./Code/moralplots.R")

####################################################################### Run Stan
source("./Code/runstan.R")

################################################################ Process Results
source("./Code/modelresults.R")

################################################################ Fit checks
source("./Code/checks.R")









