### study design diagram
library(grid)
library(Gmisc)
grid.newpage()

# set some parameters to use repeatedly
treeleft <- .2
treemid <-  .4
treeright <- .6
fixedcol <- 0.9
width <- .18
toplev <- 0.8
bottomlev <- 0.3
gp <- gpar(fill = "lightgrey")
gpf <- gpar(fill = "skyblue")

# label areas
(mainlabel <- boxGrob("Measurement Structure", 
                      x=treemid, y=0.95, box_gp = gpar(fill="white"), width = 2*width))

(fixedlabel <- boxGrob("Fixed Effects", 
                       x=fixedcol, y=0.95, box_gp = gpar(fill="white"), width = width))

# create boxes
(sub1 <- boxGrob("County 1", 
                 x=treeleft, y=toplev, box_gp = gp, width = width))

(sub_ellipses <- boxGrob("...", 
                         x=treemid, y=toplev, box_gp = gp, width = width/2))

(subI <- boxGrob("County 26", 
                 x=treeright, y=toplev, box_gp = gp, width = width))

(fixedtop <- boxGrob("None", 
                     x=fixedcol, y=toplev, box_gp = gpf, width = width/1.25))
# observation level
width_obs <- width/2
(sub11 <- boxGrob("Subject 1", 
                  x=treeleft-width_obs-0.01, y=bottomlev, box_gp = gp, width = width_obs))

(sub1_ellipses <- boxGrob("...", 
                          x=treeleft, y=bottomlev, box_gp = gp, width = width_obs/2))

(sub1n_1 <- boxGrob("Subject n1", 
                    x=treeleft+width_obs+0.01, y=bottomlev, box_gp = gp, width = width_obs))

(subI1 <- boxGrob("Subject 1", 
                  x=treeright-width_obs-0.01, y=bottomlev, box_gp = gp, width = width_obs))

(subI_ellipses <- boxGrob("...", 
                          x=treeright, y=bottomlev, box_gp = gp, width = width_obs/2))

(subIn_I <- boxGrob("Subject n26", 
                    x=treeright+width_obs+0.01, y=bottomlev, box_gp = gp, width = width_obs))


(fixedbottom <- boxGrob("Age \n Employment Length \n Level \n Hrs/Week \n Age Group", 
                        x=fixedcol, y=bottomlev, box_gp = gpf, width = width/1.25))

# connect boxes
connectGrob(sub1, sub11, "v")
connectGrob(sub1, sub1n_1, "v")

connectGrob(subI, subI1, "v")
connectGrob(subI, subIn_I, "v")

### read in data and basic formatting
library(readr)

# read in data
workforce <- read_csv("TeacherWageAgeGroups.csv")

# refactor
workforce$County <- as.factor(workforce$County)
workforce$Infants <- as.factor(workforce$Infants)
workforce$Toddlers <- as.factor(workforce$Toddlers)
workforce$Preschoolers <- as.factor(workforce$Preschoolers)
workforce$OrgIdF <- as.factor(workforce$OrgId)
workforce$Level <- as.factor(workforce$Level)
workforce$Level <- relevel(workforce$Level, "Membership")
levels(workforce$Level) <- c("Membership", "1", "2", "3", "4", "5", 
                             "6", "7", "8", "9")

# split PR levels into high vs. low depending on if college credit required
workforce$LevelSimp <- "low"
for(i in 1:nrow(workforce)){
  if(workforce$Level[i] == 4 | workforce$Level[i] == 5 | workforce$Level[i] == 6 | 
     workforce$Level[i] == 7 | workforce$Level[i] == 8 | workforce$Level[i] == 9){
    workforce$LevelSimp[i] <- "high"
  }
}

workforce$LevelSimp <- as.factor(workforce$LevelSimp)
workforce$LevelSimp <- relevel(workforce$LevelSimp, "low")
levels(workforce$LevelSimp)

### deal with missing wage data

# create variable for missing wage
workforce$WageReported <- TRUE

for(i in 1:nrow(workforce)){
  if(workforce$HourlyWage[i] == 0){
    workforce$WageReported[i] <- FALSE
  }
}

workforce$WageReported <- as.factor(workforce$WageReported)
levels(workforce$WageReported)

# convert 0s to NAs for missing wage data
for(i in 1:nrow(workforce)){
  if(workforce$HourlyWage[i] == 0){
    workforce$HourlyWage[i] <- NA
  }
}

# subset to remove missing wages
workforceSub <- subset(workforce, !is.na(HourlyWage)) 

### view figures and tables surrounding missing wage data
library(knitr)
library(yarrr)

# table wage reported counts
reportWages <- table(workforce$WageReported)
kable(reportWages, format = "markdown", col.names = 
        c("Wage Reported", "Count"))

# percent reported wage
reportWages[2] / nrow(workforce)

# sample sizes
nrow(workforce)
unique(workforce$County)
nrow(workforceSub)
unique(workforceSub$County)

# registry level
plot(WageReported ~ Level, data = workforce, 
     xlab = "Career Path level", 
     ylab = "Wage reported",
     main = "Proportion of Subjects that Reported \n Hourly Wage by Career Path Level")

# table wage reported by level
levelTable <- xtabs(~ WageReported + Level, data = workforce)
ftable(levelTable)

# age
cdplot(WageReported ~ Age, data = workforce, ylab = "Wage reported",
       main = "Proportion of Subjects that Reported \n Hourly Wage by Age")

# length of employment
cdplot(WageReported ~ EmployLengthYear, data = workforce, 
       ylab = "Wage reported", 
       xlab = "Length of employment (years)", 
       main = "Proportion of Subjects that Reported \n Hourly Wage by Length of Employment")

### find any systematic predictors for missingness of wage data

# wage reported by variables of possible interest
glm1 <- glm(WageReported ~ Age + EmployLengthYear + LevelSimp, 
            data = workforce, family = "binomial")
summary(glm1)

# effects plot
library(effects)
plot(allEffects(glm1)[2], ylab = "Wage Reported",
     xlab = "Length of current employment (years)", 
     main = "Effects Plot for Wage Reported by Length of Current Employment")

### tables and plots of response and predictors
library(mosaic)

# table age group counts
ageGroupTable1 <- xtabs(~ Infants + Toddlers + Preschoolers, 
                        data = workforceSub)
ftable(ageGroupTable1)

# table each age group by wage
favstats(HourlyWage ~ Infants + Toddlers + Preschoolers, data = workforce)

# transform response
workforceSub$LogWage <- log(workforceSub$HourlyWage)

# plot untransformed wage data
par(mfrow = c(1, 2))
pirateplot(HourlyWage ~ WageReported, xlab = "", 
           ylab = "Hourly wage (U.S. dollars)",
           main = "Distribution of Hourly Wage",
           data = workforceSub, theme = 3)

# plot transformed wage data
pirateplot(LogWage ~ WageReported, xlab = "", 
           ylab = "Log-transformed hourly wage (U.S. dollars)",
           main = "Distribution of Log-transformed Hourly Wage",
           data = workforceSub, theme = 3)

# plot untransformed wages by age groups
par(mfrow = c(1, 1))
pirateplot(HourlyWage ~ Infants + Toddlers + Preschoolers, 
           data = workforceSub, theme = 3,
           ylab = "Hourly wage")

# plot transformed wage by age groups
pirateplot(LogWage ~ Infants + Toddlers + Preschoolers, 
           data = workforceSub, theme = 3,
           ylab = "Log-transformed hourly wage")

### fit mixed models 
library(lme4)
library(lmerTest)

# wage by 2-way interactions that include toddlers
lmer1 <- lmer(LogWage ~ Infants * Toddlers + Toddlers * Preschoolers +
                Age + EmployLengthYear + Level + HoursPerWeek + (1|County), 
              data = workforceSub)
summary(lmer1)

# reduce model (drop infants*toddlers)
lmer2 <- lmer(LogWage ~ Toddlers * Preschoolers + Infants + 
                Age + EmployLengthYear + Level + HoursPerWeek + (1|County), 
              data = workforceSub)
summary(lmer2)

### fit final model

# reduce model (drop infants)
lmer3 <- lmer(LogWage ~ Toddlers * Preschoolers + Age + EmployLengthYear + 
                Level + HoursPerWeek + (1|County), data = workforceSub)
summary(lmer3)

anovaResults <- anova(lmer3, type="II") 
kable(anovaResults, format = "markdown")

### assess model diagnostics
library(car)
# constant variance
plot(lmer3, pch=16, main = "Residuals vs. Fitted Values")
par(mfrow = c(1, 2))
# normality of residuals
q1 <- qqPlot(residuals(lmer3), envelope=F, pch=16, main = "Residuals vs. Normal")
# normality of random effect
q1 <- qqPlot(unlist(ranef(lmer3)[[1]]), envelope=F, pch=16, 
             main = "County Effect vs. Normal")

### effects plot
par(mfrow = c(1, 1))
library(effects)
plot(allEffects(lmer3))
plot(allEffects(lmer3)[5], multiline=T, ci.style="bars", grid=T, 
     ylab = "Hourly Wage (log-transformed)",
     main = "Toddlers by Preschoolers Interaction Effect Plot")

### get confidence intervals
confint(lmer3)

### get r-squared
library(MuMIn)
r.squaredGLMM(lmer3)

### get icc
# icc = county variance / (county variance + residual variance)
icc <- 0.01033 / (0.01033 + 0.03009)
icc

### fit same model with untransformed wage
lmer4 <- lmer(HourlyWage ~ Toddlers * Preschoolers + Age + EmployLengthYear + 
                Level + HoursPerWeek + (1|County), data = workforceSub)
anova(lmer4, type="II") 
summary(lmer4)

### model diagnostics
# constant variance
plot(lmer4, pch=16, main = "Residuals vs. Fitted Values")
par(mfrow = c(1, 2))
# normality of residuals
q1 <- qqPlot(residuals(lmer4), envelope=F, pch=16, 
             main = "Residuals vs. Normal")
# normality of random effect
q1 <- qqPlot(unlist(ranef(lmer4)[[1]]), envelope=F, pch=16,
             main = "County Effect vs. Normal")
