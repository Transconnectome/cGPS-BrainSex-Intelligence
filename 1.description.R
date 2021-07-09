#######################################################################
##################### demographic characteristics #####################
#######################################################################

##################### load data #####################
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/Sex/analysis data")
df <- read.csv("demo.total.csv", na = "") #9658

#age(months), height(inches), weight(lbs), volume(mm^3), bmi(inches/lbs^2), maternal edu(highest grade), income, 
#race ethnicity(white, black, hispanic,asian,other),married(married, widowed, divorced, separated, never married, living with partner)

df$sex <- as.factor(df$sex)
df$race.ethnicity <- as.factor(df$race.ethnicity)
df$high.educ <- as.integer(df$high.educ)
#df$income <- as.factor(df$income)
df$married <- as.factor(df$married)
df$abcd_site <- as.factor(df$abcd_site)
df$vol = df$vol/1000 # mm^3 to cm^3

##################### visualization #####################
library(ggplot2)
library(dplyr)

### properties of ggplot
m.col = "#5B84B1FF"
f.col = "#FC766AFF"
sex.col = scale_fill_discrete(type = c(m.col, f.col))
sex.coll = scale_color_discrete(type = c(m.col, f.col))
sex.scale = scale_x_discrete(labels = c("Male", "Female"))
my.theme <- theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20), 
                  axis.text = element_text(size = 15, color = "black"))
gp <- ggplot(data = df) + sex.col

### visualization
# sex proportion
gp + geom_bar(aes(x = "", fill = sex), position = "stack") + coord_polar("y") + theme_void() + theme(legend.position = "none")
# sex propotion by site
gp + geom_bar(aes(x = reorder(abcd_site, desc(abcd_site)), fill = sex), position = position_fill(reverse = T)) +
  labs(y = "%", x = "Research Site") + my.theme + coord_flip()
# sex propotion by race
gp + geom_bar(aes(x = reorder(race.ethnicity, desc(race.ethnicity)), fill = sex), position = position_fill(reverse = T)) + coord_flip() +
  labs(y = "%", x = "Race") + my.theme + scale_x_discrete(labels = c("Other", "Asian", "Hispanic", "Black", "White"))
# brain volume
gp + geom_violin(aes(x = sex, y = vol, fill = sex), trim = TRUE, width = .7) + 
  geom_boxplot(aes(x = sex, y = vol, fill = sex), width = .1) + my.theme + 
  ylim(0, 3000) + labs(x = "Sex", y = "Brain Volume           ") + sex.scale

var.test(vol ~ sex, data = df) # p < .000: not equal
t.test(vol ~ sex, data = df, var.equal = F) # p < .000
effectsize::cohens_d(vol ~ sex, data = df)

# height
gp + geom_violin(aes(x = sex, y = height, fill = sex), width = .7) + geom_boxplot(aes(x = sex, y = height, fill = sex), width = .1) +
  my.theme + ylim(0, 100) + sex.scale + labs(y = "Height (inch)", x = "Sex")

var.test(height ~ sex, data = df)
t.test(height ~ sex, data = df, var.equal = F)
effectsize::cohens_d(height ~ sex, data = df)

# weight
gp + geom_violin(aes(x = sex, y = weight, fill = sex), width = .7) + geom_boxplot(aes(x = sex, y = weight, fill = sex), width = .1) +
  my.theme + ylim(0, 300) + sex.scale + labs(y = "Weight (lb)", x = "Sex")

var.test(weight ~ sex, data = df)
t.test(weight ~ sex, data = df, var.equal = F)
effectsize::cohens_d(weight ~ sex, data = df)

# age
gp + geom_violin(aes(x = sex, y = age, fill = sex), width = .7) + geom_boxplot(aes(x = sex, y = age, fill = sex), width = .1) +
  my.theme + ylim(0, 200) + sex.scale + labs(y = "Age (month)", x = "Sex")

var.test(age ~ sex, data = df)
t.test(age ~ sex, data = df, var.equal = T)
effectsize::cohens_d(age ~ sex, data = df)

# BMI
gp + geom_violin(aes(x = sex, y = BMI, fill = sex), width = .7) + geom_boxplot(aes(x = sex, y = BMI, fill = sex), width = .1) +
  my.theme + ylim(0, 100) + sex.scale + labs(y = "BMI         ", x = "Sex")

var.test(BMI ~ sex, data = df)
t.test(BMI ~ sex, data = df, var.equal = F)
effectsize::cohens_d(BMI ~ sex, data = df)

