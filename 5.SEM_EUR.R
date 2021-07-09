#######################################################################
##################### 5. SEM with European only #####################
#######################################################################
##################### load packages #####################
library(lavaan)
library(dplyr)
library(lavaanPlot)
library(GPArotation)
library(psych)
library(tidyverse)
library(knitr)
library(MBESS)
library(fastDummies)

##################### load data #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
df <- read.csv("demo.gps.nih.r.train.csv")
names(df) <- gsub("nihtbx_", "", names(df))

##################### data preprocessing #####################
df.x <- df[,c(1,2:12, 17, 19:21, 22:31)] #demo, mortsex.1, cogGPSs, NIHtoolbox scales
summary(df.x)
df.x$sex <- as.factor(df.x$sex)
#df.x$high.educ <- as.factor(df.x$high.educ)
df.x$high.educ <-as.numeric(df.x$high.educ)
df.x$married <- as.factor(df.x$married)
df.x$race.ethnicity <- as.factor(df.x$race.ethnicity)
df.x$abcd_site <- as.factor(df.x$abcd_site)

df.a <- df.x[,-c(7,8,10,11,12, 14:16)] #factor & gps (already z-normalization did)

df.z <- matrix(nrow = nrow(df.a), ncol = ncol(df.a)-1)
for(j in 2:ncol(df.a)){
  for(i in 1:nrow(df.a)){
    df.z[i,j-1] <- (df.a[i,j] - mean(df.a[,j], na.rm=T))/sd(df.a[,j], na.rm=T)
  }
}
df.z <- data.frame(df.z)
names(df.z) <- names(df.a)[2:ncol(df.a)]

df.zz <- data.frame(df.x[,c(1,7,8,10,11,12, 14:16)], df.z) #factors including sex, high educ,..., CP, EA, IQ
names(df.zz) <- c("subjectkey","sex", "high.educ", "married", "race.ethnicity", "abcd_site", "CP", "EA", "IQ", names(df.z))
str(df.zz)
df.1 <- df.zz %>% filter(sex == "1") #%>% select(-sex)
df.2 <- df.zz %>% filter(sex == "2") #%>% select(-sex)

##################### dummy coding for covariates #####################
df.zz.pheno <- fastDummies::dummy_cols(.data = df.zz,
                                    select_columns = c("married", "race.ethnicity", "abcd_site")) #factor variables
df.zz.pheno.1 <- df.zz.pheno %>% filter(sex == 1)
df.zz.pheno.2 <- df.zz.pheno %>% filter(sex == 2)

##################### load European only #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/abcd final/Gene/PCA")
EUR <- read.csv("ABCD_QCed_2021_PCair_8620samples_EUR.sample", sep = "\t")
EUR$FID <- gsub("AB000", "", EUR$FID)
EUR$FID <- gsub("[0-9][0-9][0-9][0-9]_", "", EUR$FID)
names(EUR)[1] <- 'subjectkey'

###df.eur.dummy
df.eur.1 <- merge(EUR[1], df.zz.pheno.1, by = 'subjectkey')
df.eur.2 <- merge(EUR[1], df.zz.pheno.2, by = 'subjectkey')

##################### SEM #####################
model  <- "
### main model
###### declare latent variables
cGPS =~ CP + EA + IQ
crystalized =~ picvocab + reading
fluid =~ flanker + cardsort + pattern + picture + list
# cryst ~ picvocab + reading
# fluidcomp ~ flanker + cardsort + pattern + picture + list
intelligence =~ crystalized + fluid
psex =~ morctsex.1

###### covs
#+ high.educ + age + height + weight + BMI + income
#+ married_1 + married_2 + married_3 + married_4 + married_5 + married_6 
#+ race.ethnicity_1 + race.ethnicity_2 + race.ethnicity_3 + race.ethnicity_4 + race.ethnicity_5 
#+ abcd_site_1 + abcd_site_2 + abcd_site_3 + abcd_site_4 + abcd_site_5 + abcd_site_6 + abcd_site_7 + abcd_site_8 + abcd_site_9 + abcd_site_10 + abcd_site_11 + abcd_site_12 + abcd_site_13 + abcd_site_14 + abcd_site_15 + abcd_site_16 +abcd_site_17 + abcd_site_18 + abcd_site_19 + abcd_site_20 + abcd_site_21

###### indirect pathway
intelligence ~ a*psex + married_2 + married_3 + married_4 + married_5 + married_6 + abcd_site_1 + abcd_site_2 + abcd_site_3 + abcd_site_4 + abcd_site_5 + abcd_site_6 + abcd_site_7 + abcd_site_8 + abcd_site_9 + abcd_site_10 + abcd_site_11 + abcd_site_12 + abcd_site_13 + abcd_site_14 + abcd_site_15 + abcd_site_17 + abcd_site_18 + abcd_site_19 + abcd_site_20 + abcd_site_21 + high.educ + age + height + weight + BMI + income
psex ~ b*cGPS + married_2 + married_3 + married_4 + married_5 + married_6 + abcd_site_1 + abcd_site_2 + abcd_site_3 + abcd_site_4 + abcd_site_5 + abcd_site_6 + abcd_site_7 + abcd_site_8 + abcd_site_9 + abcd_site_10 + abcd_site_11 + abcd_site_12 + abcd_site_13 + abcd_site_14 + abcd_site_15  +abcd_site_17 + abcd_site_18 + abcd_site_19 + abcd_site_20 + abcd_site_21 + high.educ + age + height + weight + BMI + income

######  direct pathway
intelligence ~ c*cGPS 

ab := a*b
total := c + (a*b)

flanker ~~ cardsort
flanker ~~ pattern
cardsort ~~ pattern
picture ~~ list
pattern ~~ picture
cardsort ~~ picture

picvocab ~~ list
cardsort ~~ list
reading ~~ list
"
##################### SEM fitting #####################
### In Males
fit1 = sem(model, data = df.eur.1, bootstrap = 1000, iseed = 1) #total race with covs #, fixed.x = F
summary(fit1, standardized = TRUE)
fitmeasures(fit1, c("cfi", "rmsea", "srmr")) ##summary
parameterestimates(fit1, boot.ci.type = "bca.simple", standardized = T) %>% kable()

### In Females
fit2 = sem(model, data = df.eur.2, bootstrap = 1000, iseed = 1) #total race with covs
summary(fit2, standardized = TRUE)
fitmeasures(fit2, c("cfi", "rmsea", "srmr")) ##summary
parameterestimates(fit2, boot.ci.type = "bca.simple", standardized = T) %>% kable()


