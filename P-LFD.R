# R.Version()
# This script was written using R 3.2.3


## load packages and data ####
library(lattice)
library(plyr)
library(reshape2)
library(stargazer)
library(tables)
library(mice)
library(knitr)

library(RCurl)
library(foreign)
url <- "https://raw.githubusercontent.com/proyect-uhasselt/P-LFD/master/QoL_data.csv"
data <- getURL(url)      
df <- read.table(textConnection(data), header = TRUE, sep = ",")
rm(url, data)

## Inspect dataframe and restructure ####
dim(df) #50 rows and 22 columns
length(unique(df$PatientID)) #75 unique pts
table(df$Visit) #2 visit momentents, 75 each

# All variables are measured at baseline and only two (HRQoL and RQ) also at follow-up
# As shown above, the data is in long structure
# When we calculate difference scores for HRQoL and RQ, every pt will have 1 row
# So do this first and store as 'base'

#HRQoL
cast <- dcast(df, PatientID ~ Visit, value.var = "HRQoL")
names(cast) <- c("PatientID", "HRQoL1", "HRQol2")
df$HRQoLdiff <- cast$HRQoL1 - cast$HRQol2 
rm(cast)

#RQ
cast <- dcast(df, PatientID ~ Visit, value.var = "RQ")
names(cast) <- c("PatientID", "RQ1", "RQ2")
df$RQdiff <- cast$RQ1 - cast$RQ2 
rm(cast)

#save the baseline measurement including the difference scores just created as base
base <- df[1:75, ]
names(base)

## Missing values ####
kable(t(md.pattern(base)), type = "latex") # shows the missing values pattern

## Outliers (TO DO, who originally did that?)

## Baseline characteristics ####

# Preparations
# First, we assign the proper variables types and labels and loose variable names with "_" 
# since this gives problem when compiling in latex
base$Trt <- factor(base$Trt, levels = c(1:3), labels = c("A", "B", "C"))
base$Country <- factor(base$Country, levels = c(1,2, 3), labels = c("A", "B", "C"))
base$Gender <- factor(base$Gender, levels = c(1,2), labels = c("Female", "Male"))
base$Civil_Status <- factor(base$Civil_Status, levels = c(1:4), 
                            labels = c("Single", "Widow", "Divorce", "Together"))
names(base)[names(base) == 'Civil_Status'] <- 'CivilStatus'
base$Living_Situation <- factor(base$Living_Situation, level = c(1,2), 
                                labels = c("Alone", "Not alone"))
names(base)[names(base) == 'Living_Situation'] <- 'LivingSituation'
base$Smoke <- factor(base$Smoke, levels = c(1:4), 
                     labels = c("Never", "Past", "Light to Moderate", "Heavy"))
base$Work <- factor(base$Work, levels = c(1:3),
                    labels = c("Part-time", "Full-time", "Retired"))
base$Phys_Act <- factor(base$Phys_Act, levels = c(1,2), labels = c("Light", "Heavy"))
names(base)[names(base) == 'Phys_Act'] <- 'PhysAct'
base$Comorbidity <- factor(base$Comorbidity, levels = c(1,2), labels = c("No", "Yes"))
base$COPD_severity <- factor(base$COPD_severity, levels = c(1:4),
                             labels = c("Mild", "Moderate", "Severe", "Very severe"))
names(base)[names(base) == 'COPD_severity'] <- 'COPDseverity'

# Categorical variables
# categorical <- c("Trt", "Country", "Gender", "Civil_Status", "Living_Situation",
#                  "Educ_level", "Smoke", "Work", "Phys_Act", "Comorbidity",
#                  "COPD_severity")
tab <- tabular((Country) + (Gender) + (CivilStatus) + (LivingSituation) + (Smoke) + (Work) +
                 (PhysAct) + (Comorbidity) + (COPDseverity) ~ 
                 (n=1) + Format(digits = 2) * (Trt + 1), data = base)
tab

# Continuous variables
continuous <- c("HRQoL", "RQ", "Age", "Height", "Weight", "BMI",
                  "STST", "BDI", "FEV", "HRQoLdiff", "RQdiff")
stargazer(base[ ,continuous], 
          type = "text", 
          summary.stat = c("n","mean","sd", "p25", "median", "p75", "min", "max"))

# HRQoL differences scores over treatment
ddply(base, .(Trt), summarise, 
      mean = mean(HRQoLdiff, na.rm = TRUE),
      median = median(HRQoLdiff, na.rm = TRUE),
      Q25 = quantile(HRQoLdiff, 0.25, na.rm = TRUE),
      Q75 = quantile(HRQoLdiff, 0.75, na.rm = TRUE),
      Min = range(HRQoLdiff, na.rm = TRUE)[1],
      Max = range(HRQoLdiff, na.rm = TRUE)[2],
      Nmiss = sum(is.na(HRQoLdiff)))

## Plots ####

## Outcomes measures

# Plot HRQoL over visits in the three treatments
# (note that this uses 'df')
df$Trt <- factor(df$Trt, levels = c(1:3), labels = c("Treatment A", "Treatment B", "Treatment C"))
xyplot(HRQoL ~ as.factor(Visit) | Trt, groups = PatientID, data = df, type = c("p", "l"),
       main = "HRQoL scores at visit 1 and 2",
       xlab = "Visits",
       ylab = "HRQoL score")

# Boxplot of the HRQoL difference scores per treatment
boxplot(HRQoLdiff ~ Trt, data = base,
        main = "HRQoL difference scores",
        xlab = "Treatment",
        ylab = "Difference score")

# Same for RQ
# (note that this uses 'df')
xyplot(RQ ~ as.factor(Visit) | Trt, groups = PatientID, data = df, type = c("p", "l"),
       main = "RQ scores at visti 1 and 2",
       xlab = "Visits",
       ylab = "RQ score")

# Boxplot of the RQ difference scores per treatment
boxplot(RQdiff ~ Trt, data = base,
        main = "RQ difference scores",
        xlab = "Treatment",
        ylab = "Difference score")

## predictors over treatment

#continuous

par(mfrow=c(4,2))
boxplot(Age ~ Trt, data = base,
        main = "Age at baseline over treatments",
        xlab = "Treatment",
        ylab = "Age")
boxplot(Height ~ Trt, data = base,
        main = "Height at baseline over treatments",
        xlab = "Treatment",
        ylab = "Height")
boxplot(Weight ~ Trt, data = base,
        main = "Weight at baseline over treatments",
        xlab = "Treatment",
        ylab = "Weight")
boxplot(BMI ~ Trt, data = base,
        main = "BMI at baseline over treatments",
        xlab = "Treatment",
        ylab = "BMI")
boxplot(STST ~ Trt, data = base,
        main = "STST at baseline over treatments",
        xlab = "Treatment",
        ylab = "STST")
boxplot(BDI ~ Trt, data = base,
        main = "BDI at baseline over treatments",
        xlab = "Treatment",
        ylab = "BDI")
boxplot(FEV ~ Trt, data = base,
        main = "FEV at baseline over treatments",
        xlab = "Treatment",
        ylab = "FEV")

# Scatterplots of continuous variables against HRQoL difference scores
## ----HRQoL---------------------------------------------------------------
attach(base)
par(mfrow=c(3,2))
plot(Age, HRQoLdiff, main="Scatterplot of HRQoLdiff vs. Age")
plot(BMI, HRQoLdiff, main="Scatterplot of HRQoLdiff vs. BMI")
plot(STST, HRQoLdiff, main="Scatterplot of HRQoLdiff vs. STST")
plot(BDI, HRQoLdiff, main="Scatterplot of HRQoLdiff vs. BDI")
plot(FEV, HRQoLdiff, main="Scatterplot of HRQoLdiff vs. FEV")
detach(base)




