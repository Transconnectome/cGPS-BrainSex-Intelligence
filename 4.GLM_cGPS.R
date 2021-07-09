#######################################################################
##################### 4. GLM_cognitive gps #####################
#######################################################################

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
########### total ########### 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
gps.total <- read.csv("demo.gps.r.total.csv", na = "")
gps.total$sex <- as.factor(gps.total$sex)

gps.total$sex <- as.factor(gps.total$sex)
gps.total$race.ethnicity <- as.factor(gps.total$race.ethnicity)
gps.total$high.educ <- as.integer(gps.total$high.educ)
#gps.total$income <- as.factor(gps.total$income)
gps.total$married <- as.factor(gps.total$married)
gps.total$abcd_site <- as.factor(gps.total$abcd_site)

library(dplyr)
gps.total.1 <- gps.total %>% filter(sex ==1)
gps.total.2 <- gps.total %>% filter(sex == 2)

########### train ########### 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
gps.train <- read.csv("demo.gps.r.train.csv", na = "")
gps.train$sex <- as.factor(gps.train$sex)

#gps.train <- mutate(gps.train, morctsex.1z = (morctsex.1 - mean(morctsex.1)) / sd(morctsex.1))
#gps.train <- mutate(gps.train, morctsex.1log = cuberoot(morctsex.1))

gps.train$sex <- as.factor(gps.train$sex)
gps.train$race.ethnicity <- as.factor(gps.train$race.ethnicity)
gps.train$high.educ <- as.integer(gps.train$high.educ)
#gps.train$income <- as.factor(gps.train$income)
gps.train$married <- as.factor(gps.train$married)
gps.train$abcd_site <- as.factor(gps.train$abcd_site)

library(dplyr)
gps.train.1 <- gps.train %>% filter(sex ==1)
gps.train.2 <- gps.train %>% filter(sex == 2)

##################### load family ID #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
familyID <- readxl::read_xlsx("familyID.xlsx", na = "") #11878
familyID.uniq <- familyID[!duplicated(familyID[,"rel_family_id"]), ] #9856

##################### merge GPS and family ID #####################
gps.train.fm <- merge(familyID, gps.train, by = 'subjectkey')
gps.train.fm.1 <- merge(familyID, gps.train.1, by = 'subjectkey')
gps.train.fm.2 <- merge(familyID, gps.train.2, by = 'subjectkey')

##################### GLM default setting#####################
library(ggExtra)
library(lmerTest)
library(MASS)

###### GLM of cGPS In Both Sex Group ######
###EA
gps.train.EA <- lmer(data = gps.train.fm, 
                    formula = EA ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                      married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.EA)

EA.eta <- effectsize::eta_squared(anova(gps.train.EA, data = gps.train), partial = T, ci = 0.95)[1,2] #family=binomial
EA.eta

###CP
gps.train.CP <- lmer(data = gps.train.fm, 
                     formula = CP ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.CP)
CP.eta <- effectsize::eta_squared(anova(gps.train.CP, data = gps.train.fm), partial = T, ci = 0.95)[1,2] #family=binomial
CP.eta

###IQ
gps.train.IQ <- lmer(data = gps.train.fm, 
                    formula = IQ ~ morctsex.1 +  sex + age + height + weight + BMI + high.educ + income + 
                      married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.IQ)

IQ.eta <- effectsize::eta_squared(anova(gps.train.IQ, data = gps.train), partial = T, ci = 0.95)[1,2] #family=binomial
IQ.eta


###### GLM of cGPS In Males ######
###EA
gps.train.EA.1 <- lmer(data = gps.train.fm.1, 
                     formula = EA ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.EA.1)

EA.eta.1 <- effectsize::eta_squared(anova(gps.train.EA.1, data = gps.train.fm.1), partial = T, ci = 0.95)[1,2] #family=binomial
EA.eta.1

###CP
gps.train.CP.1 <- lmer(data = gps.train.fm.1, 
                     formula = CP ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.CP.1)
CP.eta.1 <- effectsize::eta_squared(anova(gps.train.CP.1, data = gps.train.fm.1), partial = T, ci = 0.95)[1,2] #family=binomial
CP.eta.1

###IQ
gps.train.IQ.1 <- lmer(data = gps.train.fm.1, 
                     formula = IQ ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.IQ.1)

IQ.eta.1 <- effectsize::eta_squared(anova(gps.train.IQ.1, data = gps.train.1), partial = T, ci = 0.95)[1,2] #family=binomial
IQ.eta.1

###### GLM of cGPS In Females ######
###EA
gps.train.EA.2 <- lmer(data = gps.train.fm.2, 
                       formula = EA ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                         married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.EA.2)

EA.eta.2 <- effectsize::eta_squared(anova(gps.train.EA.2, data = gps.train.fm.2), partial = T, ci = 0.95)[1,2] #family=binomial
EA.eta.2

###CP
gps.train.CP.2 <- lmer(data = gps.train.fm.2, 
                       formula = CP ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                         married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.CP.2)
CP.eta.2 <- effectsize::eta_squared(anova(gps.train.CP.2, data = gps.train.fm.2), partial = T, ci = 0.95)[1,2] #family=binomial
CP.eta.2

###IQ
gps.train.IQ.2 <- lmer(data = gps.train.fm.2, 
                       formula = IQ ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                         married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(gps.train.IQ.2)

IQ.eta.2 <- effectsize::eta_squared(anova(gps.train.IQ.2, data = gps.train.2), partial = T, ci = 0.95)[1,2] #family=binomial
IQ.eta.2

