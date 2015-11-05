library(lattice)
library(plyr)
library(reshape2)
library(stargazer)
library(tables)


df <- read.table("QoL_data.csv", header = TRUE, sep = ",")

dim(df)
length(unique(df$PatientID))
table(df$Visit)

df$Trt <- as.factor(df$Trt)
levels(df$Trt) <- c("A", "B", "C")
xyplot(HRQoL ~ as.factor(Visit) | Trt, groups = PatientID, data = df, type = c("p", "l"))
