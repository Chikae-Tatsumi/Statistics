# Import files
setwd("~/R/Analysis/1_Test")
METADATA <- read.csv(file = "metadata.csv",header=T)

# Factanal analysis
factanal <- factanal(na.omit(METADATA), factors=2,cutoff=FALSE)
bind <- cbind(factanal$loadings[,1],factanal$loadings[,2])
colnames(bind) <- c("Factor 1","Factor 2")

# Save
write.csv(bind, "Factanal.csv") 