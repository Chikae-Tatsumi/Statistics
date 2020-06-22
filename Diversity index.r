library(vegan)

setwd("~/R/Analysis/1_Test/ITS")
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)

ASV <- ASV.table [,1:(ncol(ASV.table)-7)]
ASV.t <- t(ASV)
shannon <- diversity(ASV.t, index="shannon",base=2)
simpson <- diversity(ASV.t, index="simpson")
invsimpson <- diversity(ASV.t, index="invsimpson")
fisher <- fisher.alpha(ASV.t)
bind<-cbind(shannon, simpson, invsimpson,fisher)

write.table(bind, file="diversity.csv")