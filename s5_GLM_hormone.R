### draw default setting
library(ggplot2)
m.col = "#5B84B1FF" ##dark; #2d4258
f.col = "#FC766AFF" ##dark; #FF4333

sex.col = scale_fill_discrete(type = c(m.col, f.col))
sex.coll = scale_color_discrete(type = c(m.col, f.col))
sex.scale = scale_x_discrete(labels = c("Male", "Female"))
my.theme <- theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20), 
                               axis.text = element_text(size = 15, color = "black"))

#########################################################
############### Load data ###########################
#########################################################
library(dplyr)
library(lmerTest)

### data preprocessing
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
df <- read.csv("demo.gps.nih.cbcl.r.train.csv")

df$sex <- as.factor(df$sex)
df$race.ethnicity <- as.factor(df$race.ethnicity)
df$married <- as.factor(df$married)
df$abcd_site <- as.factor(df$abcd_site)

df.1 <- df %>% filter(sex == 1)
df.2 <- df %>% filter(sex == 2)

### hormone data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/raw")
ert <- read.csv("ert.raw.csv") #testosterone replacement therapy 
dhea <- read.csv("dhea.raw.csv") #dehydroepiandrosterone (디히드로에피안드로스테론)

hse <- read.csv("hse.raw.csv")
names(ert)

### df.ert
df.ert <- merge(df, ert, by = 'subjectkey') #2507

df.ert$sex <- as.factor(df.ert$sex)
df.ert$race.ethnicity <- as.factor(df.ert$race.ethnicity)
df.ert$married <- as.factor(df.ert$married)
df.ert$abcd_site <- as.factor(df.ert$abcd_site)
summary(df.ert)

### df.dhea
df.dhea <- merge(df, dhea, by = 'subjectkey') #2563

df.dhea$sex <- as.factor(df.dhea$sex)
df.dhea$race.ethnicity <- as.factor(df.dhea$race.ethnicity)
df.dhea$married <- as.factor(df.dhea$married)
df.dhea$abcd_site <- as.factor(df.dhea$abcd_site)
summary(df.dhea)

### df.hse
df.hse <- merge(df, hse, by = 'subjectkey') #1067

df.hse$sex <- as.factor(df.hse$sex)
df.hse$race.ethnicity <- as.factor(df.hse$race.ethnicity)
df.hse$married <- as.factor(df.hse$married)
df.hse$abcd_site <- as.factor(df.hse$abcd_site)
summary(df.hse)

## df.ert.dhea
df.ert.dhea <- merge(df.ert, dhea, by = 'subjectkey')
df.ert.dhea$sex <- as.factor(df.ert.dhea$sex)
df.ert.dhea$race.ethnicity <- as.factor(df.ert.dhea$race.ethnicity)
df.ert.dhea$married <- as.factor(df.ert.dhea$married)
df.ert.dhea$abcd_site <- as.factor(df.ert.dhea$abcd_site)

###
df.ert.1 <- df.ert %>% filter(sex == 1)
df.ert.2 <- df.ert %>% filter(sex ==2)

df.dhea.1 <- df.dhea %>% filter(sex == 1)
df.dhea.2 <- df.dhea %>% filter(sex ==2)

df.hse.1 <- df.hse %>% filter(sex == 1)
df.hse.2 <- df.hse %>% filter(sex == 2)

df.ert.dhea.1 <- df.ert.dhea %>% filter(sex == 1)
df.ert.dhea.2 <- df.ert.dhea %>% filter(sex == 2)

df.ert.dhea.1$sex <- as.factor(df.ert.dhea.1$sex)
df.ert.dhea.1$race.ethnicity <- as.factor(df.ert.dhea.1$race.ethnicity)
df.ert.dhea.1$married <- as.factor(df.ert.dhea.1$married)
df.ert.dhea.1$abcd_site <- as.factor(df.ert.dhea.1$abcd_site)

df.ert.dhea.2$sex <- as.factor(df.ert.dhea.2$sex)
df.ert.dhea.2$race.ethnicity <- as.factor(df.ert.dhea.2$race.ethnicity)
df.ert.dhea.2$married <- as.factor(df.ert.dhea.2$married)
df.ert.dhea.2$abcd_site <- as.factor(df.ert.dhea.2$abcd_site)

#########################################################
############### Load family ID ###########################
#########################################################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
familyID <- readxl::read_xlsx("familyID.xlsx", na = "") #11878
familyID.uniq <- familyID[!duplicated(familyID[,"rel_family_id"]), ] #9856


#########################################################
############### merge family ID and gps ###########################
#########################################################
df.ert.dhea.fm <- merge(familyID, df.ert.dhea, by = 'subjectkey')
df.ert.dhea.fm.1 <- merge(familyID, df.ert.dhea.1, by = 'subjectkey')
df.ert.dhea.fm.2 <- merge(familyID, df.ert.dhea.2, by = 'subjectkey')

####################################################################################################
######################################## glm (brain & hormone) ########################################
######################################################################################################
######################### GLM of NIH In Both Sex Group #########################
###ert
train.ert <- lmer(data = df.ert.dhea.fm, 
                 formula = ert ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                   married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(train.ert)

train.ert.eta <- effectsize::eta_squared(anova(train.ert, data = df.ert.dhea.fm), partial = T, ci = 0.95)[1,2] #family=binomial
train.ert.eta


###dhea
ggplot(data = df.ert.dhea.fm, aes(x = morctsex.1, y = dhea)) + geom_point(cex = 1.5, shape = 21, color = "black") + 
  geom_smooth(method = "lm", se = T, size = 2, color = "#FDDB27FF") + my.theme + ylim(0, 500) + 
  labs(x = "Brain-Based Sex Score", y = "DHEA")

train.dhea <- lmer(data = df.ert.dhea.fm, 
                  formula = dhea ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                    married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(train.dhea)

train.dhea.eta <- effectsize::eta_squared(anova(train.dhea, data = df.ert.dhea.fm), partial = T, ci = 0.95)[1,2] #family=binomial
train.dhea.eta

######################### GLM of NIH In Male #########################
library(ggExtra)
### in male group
###ert
train.ert.1 <- lmer(data = df.ert.dhea.fm.1, 
                   formula = ert ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                     married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(train.ert.1)

train.ert.eta.1 <- effectsize::eta_squared(anova(train.ert.1, data = df.ert.dhea.fm.1), partial = T, ci = 0.95)[1,2] #family=binomial
train.ert.eta.1


###dhea
train.dhea.1 <- lmer(data = df.ert.dhea.fm.1, 
                    formula = dhea ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                      married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(train.dhea.1)

train.dhea.eta.1 <- effectsize::eta_squared(anova(train.dhea.1, data = df.ert.dhea.fm.1), partial = T, ci = 0.95)[1,2] #family=binomial
train.dhea.eta.1



######################### GLM of NIH In Female #########################
### in female group
cor(df.ert.dhea.fm.2$ert, df.ert.dhea.fm.2$morctsex.1)

###ert

train.ert.2 <- lmer(data = df.ert.dhea.fm.2, 
                   formula = ert ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                     married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(train.ert.2)

train.ert.eta.2 <- effectsize::eta_squared(anova(train.ert.2, data = df.ert.dhea.fm.2), partial = T, ci = 0.95)[1,2] #family=binomial
train.ert.eta.2


###dhea
ggplot(data = df.ert.dhea.fm.2, aes(x = morctsex.1, y = dhea)) + geom_point(cex = 1.5, shape = 21, color = "black") + 
  geom_smooth(method = "lm", se = T, size = 2, color = "#FC766AFF") + my.theme + ylim(0, 500) + 
  labs(x = "Brain-Based Sex Score", y = "DHEA")

train.dhea.2 <- lmer(data = df.ert.dhea.fm.2, 
                    formula = dhea ~ morctsex.1 +  age + height + weight + BMI + high.educ + income + 
                      married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(train.dhea.2)

train.dhea.eta.2 <- effectsize::eta_squared(anova(train.dhea.2, data = df.ert.dhea.fm.2), partial = T, ci = 0.95)[1,2] #family=binomial
train.dhea.eta.2


#######################################################################
########################hormone & intelligence (both group) ##########################
#######################################################################
######total######
#ert
ert.total <- glm(data = df.ert.dhea.fm, 
                 formula = nihtbx_totalcomp ~ ert + sex + age + height + weight + BMI + high.educ + income + 
                   married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.total)

ert.total.eta <- effectsize::eta_squared(aov(glm(ert.total, data = df.ert.dhea.fm)), partial = T, ci = 0.95)[1,2] #family=binomial
ert.total.eta

#dhea
dhea.total <- glm(data = df.ert.dhea.fm, 
                  formula = nihtbx_totalcomp ~ dhea + sex + age + height + weight + BMI + high.educ + income + 
                    married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(dhea.total)

dhea.total.eta <- effectsize::eta_squared(aov(glm(dhea.total, data = df.ert.dhea.fm)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.total.eta


######fluid######
#ert
ert.fld <- glm(data = df.ert.dhea.fm, 
               formula = nihtbx_fluidcomp ~ ert + sex + age + height + weight + BMI + high.educ + income + 
                 married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.fld)

ert.fld.eta <- effectsize::eta_squared(aov(glm(ert.fld, data = df.ert.dhea.fm)), partial = T, ci = 0.95)[1,2] #family=binomial
ert.fld.eta

#dhea
dhea.fld <- glm(data = df.ert.dhea.fm, 
                formula = nihtbx_fluidcomp ~ dhea + sex + age + height + weight + BMI + high.educ + income + 
                  married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(dhea.fld)

dhea.fld.eta <- effectsize::eta_squared(aov(glm(dhea.fld, data = df.ert.dhea.fm)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.fld.eta



######cryst ######
#ert
ert.cry <- glm(data = df.ert.dhea.fm, 
               formula = nihtbx_cryst ~ ert + sex + age + height + weight + BMI + high.educ + income + 
                 married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.cry)

ert.cry.eta <- effectsize::eta_squared(aov(ert.cry, data = df.ert.dhea.fm), partial = T, ci = 0.95)[1,2] #family=binomial
ert.cry.eta

#dhea
dhea.cry <- glm(data = df.ert.dhea.fm, 
                formula = nihtbx_cryst ~ dhea + sex + age + height + weight + BMI + high.educ + income + 
                  married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(dhea.cry)

dhea.cry.eta <- effectsize::eta_squared(aov(glm(dhea.cry, data = df.ert.dhea.fm)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.cry.eta

#######################################################################
########################hormone & intelligence (each sex group) ##########################
#######################################################################
###### ERT ######
#ert-total
ert.total <- glm(data = df.ert.dhea.fm.1, 
                 formula = nihtbx_totalcomp ~ ert  + age + height + weight + BMI + high.educ + income + 
                   married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.total)

ert.total.eta <- effectsize::eta_squared(aov(glm(ert.total, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
ert.total.eta

#ert-fluid
ert.fld <- glm(data = df.ert.dhea.fm.1, 
               formula = nihtbx_fluidcomp ~ ert + age + height + weight + BMI + high.educ + income + 
                 married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.fld)

ert.fld.eta <- effectsize::eta_squared(aov(glm(ert.fld, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
ert.fld.eta

#ert-cryst
ert.cry <- glm(data = df.ert.dhea.fm.1, 
               formula = nihtbx_cryst ~ ert +  age + height + weight + BMI + high.educ + income + 
                 married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(ert.cry)

ert.cry.eta <- effectsize::eta_squared(aov(glm(ert.cry, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
ert.cry.eta

###### DHEA ######
#dhea-total
dhea.total <- glm(data = df.ert.dhea.fm.1, 
                  formula = nihtbx_totalcomp ~ dhea + age + height + weight + BMI + high.educ + income + 
                    married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(dhea.total)

dhea.total.eta <- effectsize::eta_squared(aov(glm(dhea.total, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.total.eta

#dhea-fluid
dhea.fld <- glm(data = df.ert.dhea.fm.1, 
                formula = nihtbx_fluidcomp ~ dhea + age + height + weight + BMI + high.educ + income + 
                  married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(dhea.fld)

dhea.fld.eta <- effectsize::eta_squared(aov(glm(dhea.fld, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.fld.eta


#dhea-crsyt
dhea.cry <- glm(data = df.ert.dhea.fm.1, 
                formula = nihtbx_cryst ~ dhea + age + height + weight + BMI + high.educ + income + 
                  married + race.ethnicity + abcd_site+ (1 | rel_family_id))
summary(dhea.cry)

dhea.cry.eta <- effectsize::eta_squared(aov(glm(dhea.cry, data = df.ert.dhea.fm.1)), partial = T, ci = 0.95)[1,2] #family=binomial
dhea.cry.eta
