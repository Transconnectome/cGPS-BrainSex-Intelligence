#######################################################################
##################### 2-2. ROC curve for TabNet #####################
#######################################################################

##################### load data ##################### 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/final results_gunAhn")
site <- read.csv("total_site.csv") #7803
#rand <- read.csv("total_rand.csv") #1957
#race <- read.csv("total_race.csv") #1957

### load package
library(pROC)

##################### data preprocessing ##################### 
site$y_mor_v <- as.factor(site$y_mor_v)
site$y_ct_v <- as.factor(site$y_ct_v)
site$y_morct_v <- as.factor(site$y_morct_v)

site$y_mor_t <- as.factor(site$y_mor_t)
site$y_ct_t <- as.factor(site$y_ct_t)
site$y_morct_t <- as.factor(site$y_morct_t)

### extract the source for roc
mor_roc_v <- roc(site$y_mor_v, site$pred_mor_v)
ct_roc_v <- roc(site$y_ct_v, site$pred_ct_v)
morct_roc_v <- roc(site$y_morct_v, site$pred_morct_v)

mor_roc_t <- roc(site$y_mor_t, site$pred_mor_t)
ct_roc_t <- roc(site$y_ct_t, site$pred_ct_t)
morct_roc_t <- roc(site$y_morct_t, site$pred_morct_t)

##################### visualize roc-auc curve ##################### 
##valid
plot.roc(mor_roc_v, #data
         col = "#A9A9A9", lwd = 3, #line of auc #dark gray
         axes=T, legacy.axes=T, #axis
         identity.col = "#A9A9A9", identity.lty=3, identity.lwd = 2) 
plot.roc(ct_roc_v, add = T, col = "#DCDCDC", lwd = 3) #light gray
plot.roc(morct_roc_v, add = T, col = "#000000", lwd = 3) #black

##test
plot.roc(mor_roc_t, #data
         col = "#A9A9A9", lwd = 3, #line of auc #dark gray
         axes=T, legacy.axes=T, #axis
         identity.col = "#A9A9A9", identity.lty=3, identity.lwd = 2) 
plot.roc(ct_roc_t, add = T, col = "#DCDCDC", lwd = 3) #light gray
plot.roc(morct_roc_t, add = T, col = "#000000", lwd = 3) #black




