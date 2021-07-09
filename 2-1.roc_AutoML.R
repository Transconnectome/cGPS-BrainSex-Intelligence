#######################################################################
##################### 2-1. ROC curve for AutoML #####################
#######################################################################

library(jsonlite)

##################### load data #####################
##train
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Connectome Lab/C_Sex_Final Results_H2O DAI/ROC json")
mor.rand <- fromJSON("train_mor(all)_random5.json")
mor.race <- fromJSON('train_mor(all)_race5.json')
mor.site <- fromJSON('train_mor(all)_site22.json')

ct.rand <- fromJSON("train_ct_random5.json")
ct.race <- fromJSON("train_ct_race5.json")
ct.site <- fromJSON("train_ct_site22.json")

morct.rand <- fromJSON("train_morct(all)_random5.json")
morct.race <- fromJSON("train_morct(all)_race5.json")
morct.site <- fromJSON("train_morct(all)_site22.json")

##test
mor.rand <- fromJSON("test_mor(all)_random5.json")
mor.race <- fromJSON('test_mor(all)_race5.json')
mor.site <- fromJSON('test_mor(all)_site22.json')

ct.rand <- fromJSON("test_ct_random5.json")
ct.race <- fromJSON("test_ct_race5.json")
ct.site <- fromJSON("test_ct_site22.json")

morct.rand <- fromJSON("test_morct(all)_random5.json")
morct.race <- fromJSON("test_morct(all)_race5.json")
morct.site <- fromJSON("test_morct(all)_site22.json")

##################### tpr, fpr #####################
### morphometry data
## roc of mor.rand
{tpr = vector(, length = length(mor.rand[[1]])); fpr = tpr; 
tpr.list = tpr; fpr.list = tpr
fpr.list = mor.rand[[5]]; tpr.list = mor.rand[[6]]
for(i in 1:length(tpr.list)){
  tpr[i] = tpr.list[[i]]
  fpr[i] = fpr.list[[i]]
}
mor.rand.roc <- data.frame(val = 'rand', type = 'mor', fpr, tpr)
}

## roc of mor.race
{tpr = vector(, length = length(mor.race[[1]])); 
  fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = mor.race[[5]]; tpr.list = mor.race[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  mor.race.roc <- data.frame(val = 'race', type = 'mor', fpr, tpr)
}

## roc of mor.site
{tpr = vector(, length = length(mor.site[[1]])); 
  fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = mor.site[[5]]; tpr.list = mor.site[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  mor.site.roc <- data.frame(val = 'site', type = 'mor', fpr, tpr)
}

### connectome data
## roc of ct.rand
{tpr = vector(, length = length(ct.rand[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
fpr.list = ct.rand[[5]]; tpr.list = ct.rand[[6]]
for(i in 1:length(tpr.list)){
  tpr[i] = tpr.list[[i]]
  fpr[i] = fpr.list[[i]]
}
ct.rand.roc <- data.frame(val = 'rand', type = 'ct', fpr, tpr)
}

## roc of ct.race
{tpr = vector(, length = length(ct.race[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = ct.race[[5]]; tpr.list = ct.race[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  ct.race.roc <- data.frame(val = 'race', type = 'ct', fpr, tpr)
}

## roc of ct.site
{tpr = vector(, length = length(ct.site[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = ct.site[[5]]; tpr.list = ct.site[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  ct.site.roc <- data.frame(val = 'site', type = 'ct', fpr, tpr)
}

### morphometry + connectome
## roc of morct.rand
{tpr = vector(, length = length(morct.rand[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
fpr.list = morct.rand[[5]]; tpr.list = morct.rand[[6]]
for(i in 1:length(tpr.list)){
  tpr[i] = tpr.list[[i]]
  fpr[i] = fpr.list[[i]]
}
morct.rand.roc <- data.frame(val = 'rand', type = 'morct', fpr, tpr)
}

## roc of morct.race
{tpr = vector(, length = length(morct.race[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = morct.race[[5]]; tpr.list = morct.race[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  morct.race.roc <- data.frame(val = 'race', type = 'morct', fpr, tpr)
}

## roc of morct.site
{tpr = vector(, length = length(morct.site[[1]])); fpr = tpr; tpr.list = tpr; fpr.list = tpr
  fpr.list = morct.site[[5]]; tpr.list = morct.site[[6]]
  for(i in 1:length(tpr.list)){
    tpr[i] = tpr.list[[i]]
    fpr[i] = fpr.list[[i]]
  }
  morct.site.roc <- data.frame(val = 'site', type = 'morct', fpr, tpr)
}

## summary roc; df.roc
df.roc <- rbind(mor.rand.roc, mor.race.roc, mor.site.roc, 
                ct.rand.roc, ct.race.roc, ct.site.roc,
                morct.rand.roc, morct.race.roc, morct.site.roc)

##################### visualize roc-auc curve #####################
### ROC curve
library(ggplot2)
library(dplyr)

### morct with leave-one-site-out-cross-validation
ggplot(data = df.roc %>% filter(val == "site"), aes(x = fpr, y = tpr, colour = type)) +
  geom_path(size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  #scale_color_manual(values = c("#333333", "#B8B8B8", "#000000")) +
  scale_color_grey(start = .9, end = 0.2) + ##grey color tone
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .8),
        legend.position = "None") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 23))
  #theme(legend.position = "top")


##cross validation 3가지를 포함하는 mor, con, morct 각각
ggplot(data = df.roc %>% filter(type == "mor"), aes(x = fpr, y = tpr, colour = val)) +
  geom_path(size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 5 ,colour = 'grey50', size = 1) +
  scale_color_manual(values = c("grey", "blue3", "red3")) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .8),
        legend.position = "None") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30)) 

ggplot(data = df.roc %>% filter(type == "ct"), aes(x = fpr, y = tpr, colour = val)) +
  geom_path(size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  scale_color_manual(values = c("grey", "blue3", "red3")) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .8),
        legend.position = "None") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30))

ggplot(data = df.roc %>% filter(type == "morct"), aes(x = fpr, y = tpr, colour = val)) +
  geom_path(size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  scale_color_manual(values = c("grey", "blue3", "red3")) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .8),
        legend.position = "None") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 30))



### visualization morphometry with different cross validation method
##mor
ggplot(data = mor.rand.roc, aes(x = fpr, y = tpr, colour = mor.rand, linetype = val)) +
  geom_path(size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .7),
        legend.position = "None")

###mor.race
ggplot(data = mor.race.roc, aes(x = fpr, y = tpr, colour = mor.race, linetype = val)) +
  geom_path(size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .7),
        legend.position = "None")

###mor.site
ggplot(data = mor.site.roc, aes(x = fpr, y = tpr, colour = mor.site, linetype = val)) +
  geom_path(size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = 3 ,colour = 'grey50', size = 1) +
  theme_bw() +
  labs(x = 'False Positive Rate', y = 'True Positive Rate') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .7),
        legend.position = "None")




