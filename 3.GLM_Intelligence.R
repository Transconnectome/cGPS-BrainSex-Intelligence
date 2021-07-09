##################### visualization default setting ##################### 

library(ggplot2)
m.col = "#5B84B1FF" ##dark; #2d4258
f.col = "#FC766AFF" ##dark; #FF4333

sex.col = scale_fill_discrete(type = c(m.col, f.col))
sex.coll = scale_color_discrete(type = c(m.col, f.col))
sex.scale = scale_x_discrete(labels = c("Male", "Female"))
my.theme <- theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20), 
                               axis.text = element_text(size = 15, color = "black"))

##################### load data #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
nih.test <- read.csv("demo.nih.r.test.csv", na = "")
nih.train <- read.csv("demo.nih.r.train.csv", na = "")

##################### load data #####################
nih.test$sex <- as.factor(nih.test$sex)
nih.test$race.ethnicity <- as.factor(nih.test$race.ethnicity)
nih.test$married <- as.factor(nih.test$married)
nih.test$abcd_site <- as.factor(nih.test$abcd_site)

nih.train$sex <- as.factor(nih.train$sex)
nih.train$race.ethnicity <- as.factor(nih.train$race.ethnicity)
nih.train$married <- as.factor(nih.train$married)
nih.train$abcd_site <- as.factor(nih.train$abcd_site)

library(dplyr)
nih.test.1 <- nih.test %>% filter(sex == 1)
nih.test.2 <- nih.test %>% filter(sex == 2)

nih.train.1 <- nih.train %>% filter(sex == 1)
nih.train.2 <- nih.train %>% filter(sex == 2)

##################### load family ID #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
familyID <- readxl::read_xlsx("familyID.xlsx", na = "") #11878
familyID.uniq <- familyID[!duplicated(familyID[,"rel_family_id"]), ] #9856

##################### merge GPS and family ID #####################
nih.train.fm <- merge(familyID, nih.train)
nih.train.fm.1 <- merge(familyID, nih.train.1)
nih.train.fm.2 <- merge(familyID, nih.train.2)

##################### GLM default setting #####################
library(ggExtra)
library(lmerTest)
library(effectsize)

###### GLM of Intelligence In Both Sex Group ######
###total comp
total.train <- lmer(data = nih.train.fm, 
                   formula = nihtbx_totalcomp ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                     married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(total.train)
total.eta <- eta_squared(anova(total.train, data = nih.train.fm), partial = T, ci = 0.95)[1,2]
total.eta

###fluid comp
fluid.train <- lmer(data = nih.train.fm, 
                   formula = nihtbx_fluidcomp ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                     married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(fluid.train)
fluid.eta <- eta_squared(anova(fluid.train, data = nih.train.fm), partial = T, ci = 0.95)[1,2]
fluid.eta

### cryst
cryst.train <- lmer(data = nih.train.fm, 
                   formula = nihtbx_cryst ~ morctsex.1 + sex + age + height + weight + BMI + high.educ + income + 
                     married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(cryst.train)
cryst.eta <- eta_squared(anova(cryst.train, data = nih.train.fm), partial = T, ci = 0.95)[1,2]
cryst.eta

###### GLM of Intelligence In Males ######
###total comp
total.train.1 <- lmer(data = nih.train.fm.1, 
                     formula = nihtbx_totalcomp ~ morctsex.1 +  age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(total.train.1)

total.eta.1 <- eta_squared(anova(total.train.1, data = nih.train.fm.1), partial = T, ci = 0.95)[1,2]
total.eta.1

###fluid comp
fluid.train.1 <- lmer(data = nih.train.fm.1, 
                     formula = nihtbx_fluidcomp ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(fluid.train.1)

fluid.eta.1 <- eta_squared(anova(fluid.train.1, data = nih.train.fm.1), partial = T, ci = 0.95)[1,2]
fluid.eta.1

### cryst
cryst.train.1 <- glm(data = nih.train.fm.1, 
                     formula = nihtbx_cryst ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site)
summary(cryst.train.1)
#[train] morctsex.1       2.54182    0.38635   6.579 5.37e-11 ***

cryst.eta.1 <- eta_squared(aov(cryst.train.1, data = nih.train.fm.1), partial = T, ci = 0.95)[1,2]
cryst.eta.1

###### GLM of Intelligence In Females ######
###total comp
total.train.2 <- lmer(data = nih.train.fm.2, 
                      formula = nihtbx_totalcomp ~ morctsex.1 +  age + height + weight + BMI + high.educ + income + 
                        married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(total.train.2)


total.eta.2 <- eta_squared(anova(total.train.2, data = nih.train.fm.2), partial = T, ci = 0.95)[1,2]
total.eta.2

###fluid comp
fluid.train.2 <- lmer(data = nih.train.fm.2, 
                      formula = nihtbx_fluidcomp ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                        married + race.ethnicity + abcd_site + (1 | rel_family_id))
summary(fluid.train.2)

fluid.eta.2 <- eta_squared(anova(fluid.train.2, data = nih.train.fm.1), partial = T, ci = 0.95)[1,2]
fluid.eta.2

### cryst
cryst.train.2 <- glm(data = nih.train.fm.2, 
                     formula = nihtbx_cryst ~ morctsex.1 + age + height + weight + BMI + high.educ + income + 
                       married + race.ethnicity + abcd_site)
summary(cryst.train.2)

cryst.eta.2 <- eta_squared(aov(cryst.train.2, data = nih.train.fm.2), partial = T, ci = 0.95)[1,2]
cryst.eta.2



