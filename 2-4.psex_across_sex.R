#######################################################################
##################### 2-4. Data Description #####################
#######################################################################

##################### load data #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
df <- read.csv("demo.total.csv", na = "") #9658

#age(months), height(inches), weight(lbs), volume(mm^3), bmi(inches/lbs^2), maternal edu(highest grade), income, 
#race ethnicity(white, black, hispanic,asian,other),married(married, widowed, divorced, separated, never married, living with partner)
library(dplyr)
df$sex <- as.factor(df$sex)
df$race.ethnicity <- as.factor(df$race.ethnicity)
df$high.educ <- as.integer(df$high.educ)
#df$income <- as.factor(df$income)
df$married <- as.factor(df$married)
df$abcd_site <- as.factor(df$abcd_site)

df.1 <- df %>% filter(sex == 1)
df.2 <- df %>% filter(sex == 2)
library(ggplot2)

### properties of ggplot
m.col = "#5B84B1FF"
f.col = "#FC766AFF"
sex.col = scale_fill_discrete(type = c(m.col, f.col))
sex.coll = scale_color_discrete(type = c(m.col, f.col))
sex.scale = scale_x_discrete(labels = c("Male", "Female"))
my.theme <- theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20), 
                               axis.text = element_text(size = 15, color = "black"))
gp <- ggplot(data = df) + sex.col

##################### visualization #####################
### male, female -> one plot
ggplot(data = df, aes(x = sex, y = morctsex.1,colour = morctsex.1)) +
     geom_jitter(width = .45, cex = 1.2, shape = 21) + 
     geom_violin(aes(fill = sex),alpha = .3) + 
     geom_boxplot(aes(fill = sex),alpha = .3, width = .1,outlier.shape  = NA) +
     scale_color_gradient2(midpoint = .5, low = "#FC766AFF", high = "#5B84B1FF", mid = "#cccccc", limits = c(0,1)) +
     scale_x_discrete(labels = c("Male", "Female")) +scale_fill_manual(values = c("#5B84B1FF", "#FC766AFF")) + 
     xlab("Sex") +
     ylab("Brain-Based Sex Score") +
     guides(fill = FALSE) +
     theme_bw() +
     theme(legend.key.height = unit(3, 'cm'),
           legend.title = element_blank(),
           legend.text = element_blank(),
           legend.position = "none",
           axis.text = element_text(size = 20),
           axis.title = element_text(size = 25))
myviolin(df)  

### violin plot
##in male
ggplot(data = df.1, aes(x = sex, y = morctsex.1,colour = morctsex.1)) +
  geom_jitter(width = .45, cex = 1.2, shape = 21) + 
  geom_violin(aes(fill = sex),alpha = .3) + 
  geom_boxplot(aes(fill = sex),alpha = .3, width = .1,outlier.shape  = NA) +
  scale_color_gradient2(midpoint = .5, low = "#FC766AFF", high = "#5B84B1FF", mid = "#cccccc", limits = c(0,1)) +
  scale_x_discrete(labels = "Male") +scale_fill_manual(values = "#5B84B1FF") + 
  xlab("Sex") +
  ylab("Brain-Based Sex Score") +
  guides(fill = FALSE) +
  theme_bw() +
  theme(legend.key.height = unit(3, 'cm'),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 25))

##in female
ggplot(data = df.2, aes(x = sex, y = morctsex.1,colour = morctsex.1)) +
  geom_jitter(width = .45, cex = 1.2, shape = 21) + 
  geom_violin(aes(fill = sex),alpha = .3) + 
  geom_boxplot(aes(fill = sex),alpha = .3, width = .1,outlier.shape  = NA) +
  scale_color_gradient2(midpoint = .5, low = "#FC766AFF", high = "#5B84B1FF", mid = "#cccccc", limits = c(0,1)) +
  scale_x_discrete(labels = "Male") +scale_fill_manual(values = "#FC766AFF") + 
  xlab("Sex") +
  ylab("Brain-Based Sex Score") +
  guides(fill = FALSE) +
  theme_bw() +
  theme(legend.key.height = unit(3, 'cm'),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= 25))

### histogram
##in male
ggplot(data = df.1) +
  geom_density(aes(x = morctsex.1, fill = I("#5B84B1FF"))) + 
  geom_histogram(aes(x = morctsex.1, fill = I("#5B84B1FF")), bins = 30) +
  theme_bw() +
  scale_fill_manual(values = "#5B84B1FF") +
  theme(legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25)) +
  xlab("Brain-Based Sex Score") + 
  ylab("Number of Participants") +
  ylim(0, 1500)

##in female
ggplot(data = df.2) +
  geom_density(aes(x = morctsex.1, fill = I("#FC766AFF"))) + 
  geom_histogram(aes(x = morctsex.1, fill = I("#FC766AFF")), bins = 30) +
  theme_bw() +
  scale_fill_manual(values = "#FC766AFF") +
  theme(legend.position = "none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 23)) +
  xlab("Brain-Based Sex Score") + 
  ylab("Number of Participants") +
  ylim(0, 1500)

