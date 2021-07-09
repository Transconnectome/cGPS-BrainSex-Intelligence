#######################################################################
##################### supple 2. cognitive intelligence #####################
#######################################################################

library(dplyr)
library(ggplot2)

##################### visualization default setting ##################### 
m.col = "#5B84B1FF"
f.col = "#FC766AFF"
sex.col = scale_fill_discrete(type = c(m.col, f.col))
sex.coll = scale_color_discrete(type = c(m.col, f.col))
sex.scale = scale_x_discrete(labels = c("Male", "Female"))
my.theme <- theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20), 
                               axis.text = element_text(size = 15, color = "black"))
gp <- ggplot(data = df) + sex.col

##################### load data #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
nih <- read.csv("demo.nih.r.total.csv", na = "")
names(nih) <- gsub("nihtbx_", "", names(nih))

nih$sex <- as.factor(nih$sex)

##################### cohens_d #####################
effectsize::cohens_d(picvocab ~ sex, data = nih)
effectsize::cohens_d(reading ~ sex, data = nih)
effectsize::cohens_d(cardsort ~ sex, data = nih)
effectsize::cohens_d(flanker ~ sex, data = nih)
effectsize::cohens_d(list ~ sex, data = nih)
effectsize::cohens_d(pattern ~ sex, data = nih)
effectsize::cohens_d(picture ~ sex, data = nih)
effectsize::cohens_d(cryst ~ sex, data = nih)
effectsize::cohens_d(fluidcomp ~ sex, data = nih)
effectsize::cohens_d(totalcomp ~ sex, data = nih)

##################### visualize data ##################### 
x = tidyr::gather(key = "key", value = "value", nih[,c(7, 19:length(nih))], -sex)
x$key <- as.factor(x$key)
x$key <- factor(x$key, levels = c("picvocab", "reading", "cardsort", "flanker", "list", "pattern",
                                  "picture", "cryst", "fluidcomp", "totalcomp")) #8, 9, 1, 3, 5, 6, 7, 2, 4, 10

ggplot(data = x) + geom_violin(aes(x = key, y = value, fill = sex), position = position_dodge(width = .7)) + 
  geom_boxplot(aes(x = key, y = value, fill = sex), width = .1, position = position_dodge(width = .7)) + 
  my.theme + sex.col + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 200) +
  scale_x_discrete(labels = nih.name) + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(y = "Score")


##################### load data ##################### 
nih.train <- read.csv(file.choose())

nih.train <- nih.train %>% 
  mutate(bsex = ifelse(nih.train$morctsex.1 >= .5, "1", "2"))
nih.train$bsex <- as.factor(nih.train$bsex) #each; n=475

##################### cohens_d #####################
effectsize::cohens_d(nihtbx_totalcomp ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_fluidcomp ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_cryst ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_picture ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_pattern ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_list ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_flanker ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_cardsort ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_reading ~ bsex, data = nih.train)
effectsize::cohens_d(nihtbx_picvocab ~ bsex, data = nih.train)

##################### visualize data ##################### 
x = tidyr::gather(key = "key", value = "value", nih.train[,c(19:length(nih.train))], - bsex)
x$key <- as.factor(x$key)
x$key <- factor(x$key, levels = c("nihtbx_picvocab", "nihtbx_reading", "nihtbx_cardsort", "nihtbx_flanker", "nihtbx_list", "nihtbx_pattern",
                                  "nihtbx_picture", "nihtbx_cryst", "nihtbx_fluidcomp", "nihtbx_totalcomp")) #8, 9, 1, 3, 5, 6, 7, 2, 4, 10

ggplot(data = x) + geom_violin(aes(x = key, y = value, fill = bsex), position = position_dodge(width = .7)) + 
  geom_boxplot(aes(x = key, y = value, fill = bsex), width = .1, position = position_dodge(width = .7)) + 
  my.theme + sex.col + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 200) +
  #scale_x_discrete(labels = nih.name) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(y = "Score")


