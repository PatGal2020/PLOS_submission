rm(list =ls())
gc()
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
overall.wd="C:/Users/pg2015/Desktop/rats/rat_code/measurements"
setwd(overall.wd) 

data=read.table("ranges.txt", header=T)

pdf("percentage-paranode_rat.pdf", width = 11, height = 9)
s <- ggplot(data, aes(x=ratgroup, y=percentage, 
          fill=factor(range,levels=c("morethan4","less4more3","less3more2","less2more1"))))+
  geom_bar( stat="identity",alpha=0.5)+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  theme(text = element_text(size=20))+
  guides(fill=guide_legend(title="Case"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(s)
dev.off()