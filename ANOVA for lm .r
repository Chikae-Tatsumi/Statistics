library(lme4)
library(lmerTest)

setwd("~/R/Analysis/1_Test")
TABLE <- read.csv("metadata.csv",header=T, row.names=1)
DESIGN <- read.csv("experimental_design.csv",header=T)
DESIGN <- na.omit(DESIGN)

Factor1 <- DESIGN$AAA #CHANGE ME
Factor2 <- DESIGN$BBB #CHANGE ME
NAME1 <- "AAA" #CHANGE ME
NAME2 <- "BBB" #CHANGE ME

Results <- NULL
for (i in 1:ncol(TABLE)){
anova <- anova(lm(TABLE[,i]~Factor1*Factor2))
Fval <- anova[,4]
Pval <- anova[,5]

Bind <- c(Fval[1], Pval[1], Fval[2], Pval[2], Fval[3], Pval[3])

names(Bind) <- c(paste(NAME1,".F",sep=""),paste(NAME1,".P",sep=""),paste(NAME2,".F",sep=""),paste(NAME2,".P",sep=""),paste(NAME1,"*",NAME2,".F",sep=""),paste(NAME1,"*",NAME2,".P",sep=""))
Results <- rbind(Results, Bind)}

rownames(Results) <- colnames(TABLE)

Results.asterisk <- Results
for (i in 1:nrow(Results)){
if (Results[i,2] < 0.001) {Results.asterisk[i,2] <- "***"
} else if (Results[i,2] < 0.01) {Results.asterisk[i,2] <- "**"
} else if (Results[i,2] < 0.05) {Results.asterisk[i,2] <- "*"
} else {Results.asterisk[i,2]<- "n.s"}}
for (i in 1:nrow(Results)){
if (Results[i,4] < 0.001) {Results.asterisk[i,4] <- "***"
} else if (Results[i,4] < 0.01) {Results.asterisk[i,4] <- "**"
} else if (Results[i,4] < 0.05) {Results.asterisk[i,4] <- "*"
} else {Results.asterisk[i,4]<- "n.s"}}
for (i in 1:nrow(Results)){
if (Results[i,6] < 0.001) {Results.asterisk[i,6] <- "***"
} else if (Results[i,6] < 0.01) {Results.asterisk[i,6] <- "**"
} else if (Results[i,6] < 0.05) {Results.asterisk[i,6] <- "*"
} else {Results.asterisk[i,6]<- "n.s"}}

write.csv(Results.asterisk, "anova.csv") 
