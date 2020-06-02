setwd("~/R/Analysis/1_Test")
METADATA <- read.csv("metadata.csv")
DESIGN <- read.csv("experimental_design.csv")

METADATA.S<-scale(METADATA)

Factor1 <- scale(DESIGN$Factor1) #CHANGE ME
Factor2 <- scale(DESIGN$Factor2) #CHANGE ME
NAME1 <- "Factor1" #CHANGE ME
NAME2 <- "Factor2" #CHANGE ME

MODEL <- METADATA.S[,i]~Factor1*Factor2

Results <- NULL
for (i in 1:ncol(METADATA)){
lm <- summary(lm(MODEL))
Estimate <- lm$coefficients[,1]
Pval <-lm$coefficients[,4]
Bind <- c(Estimate[2], Pval[2], Estimate[3], Pval[3], Estimate[4], Pval[4])
names(Bind) <- c(paste(NAME1,".E",sep=""),paste(NAME1,".P",sep=""),paste(NAME2,".E",sep=""),paste(NAME2,".P",sep=""),paste(NAME1,"*",NAME2,".E",sep=""),paste(NAME1,"*",NAME2,".P",sep=""))
Results <- rbind(Results, Bind)}

rownames(Results) <- colnames(METADATA)

write.csv(Results, "lm.csv") 