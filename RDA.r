library(vegan)

# Import files
setwd("~/R/Analysis/1_Test")
METADATA <- read.csv("metadata.csv")
setwd("~/R/Analysis/1_Test/ITS")
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
ASV <- ASV.table [,1:(ncol(ASV.table)-7)]
ASV.t <- t(ASV)

# If you want to remove NA data
bind <- cbind (ASV.t, METADATA)
bind.rm <- na.omit (bind)
ASV.t <- bind.rm [, 1:nrow(ASV.table)]
METADATA.rm <- na.omit (METADATA)

# RDA
rda <- rda(ASV.t ~ Moist*Temp, data=METADATA.rm) # CHANGE ME
anova <- anova(rda, by="margin")

# Save
write.csv (anova, "RDA.csv")