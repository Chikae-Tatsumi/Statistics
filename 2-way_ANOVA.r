library(car)

setwd("~/R/Analysis/1_Test")
METADATA <- read.csv("metadata.csv")
DESIGN <- read.csv("experimental_design.csv")

Factor1 <- DESIGN$Factor1 #CHANGE ME
Factor2 <- DESIGN$Factor2 #CHANGE ME
NAME1 <- "Factor1" #CHANGE ME
NAME2 <- "Factor2" #CHANGE ME
Group <- paste (Factor1, Factor2,sep="")

MODEL1 <- METADATA[,i]~Factor1*Factor2
MODEL2 <- sqrt(METADATA[,i])~Factor1*Factor2

LT <- leveneTest(METADATA[,i]~Group)

Results <- NULL
for (i in 1:ncol(METADATA)){
if (LT$`Pr(>F)`[-2]>0.05) {anova <- summary(aov(MODEL1))} else {anova <- summary(aov(MODEL2))}
Fval <- anova[[1]]$`F value`
Pval <- anova[[1]]$`Pr(>F)`
Bind <- c(Fval[1], Pval[1], Fval[2], Pval[2], Fval[3], Pval[3])
names(Bind) <- c(paste(NAME1,".F",sep=""),paste(NAME1,".P",sep=""),paste(NAME2,".F",sep=""),paste(NAME2,".P",sep=""),paste(NAME1,"*",NAME2,".F",sep=""),paste(NAME1,"*",NAME2,".P",sep=""))
Results <- rbind(Results, Bind)}

rownames(Results) <- colnames(METADATA)

write.csv(Results, "2-way_ANOVA.csv") 
