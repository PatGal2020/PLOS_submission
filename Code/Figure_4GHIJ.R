rm(list =ls())
gc()

library(ggplot2)

setwd("C:/Users/pg2015/Desktop/rats/rat_code/measurements")

data=read.table("Summary_data2.txt")

average_paranodal_length=data$V2
number_microglia=data$V3
number_astrocytes=data$V5
overlap_50=c(0.099, 0.068, 0.072, 0.078, 0.065,0.047, 0.037, 0.041, 0.036, 0.05, 0.04 )
microglia_area=c(4.64,	3.72,	3.55,	4.63,	4.85,	4.24,	3.44,	3.79,	4.17,	4.06,	4.38)
more_than_3=c(31/200, 10/200, 27/200, 23/200, 6/200, 5/200, 19/200, 4/200, 0/200, 1/200, 0/200)

cor.test(number_microglia,more_than_3, method="spearman")
cor.test(number_astrocytes,more_than_3, method="spearman")
cor.test(overlap_50, more_than_3, method="spearman")

a=c(0.099, 0.068, 0.072, 0.078, 0.065)
c=c(0.047, 0.037, 0.041)
b=c(0.036, 0.05, 0.04)

wilcox.test(a,c)

cor.test(microglia_area, average_paranodal_length, method=spearman)
lm(number_microglia ~ number_astrocytes)

plot(number_microglia, number_astrocytes)

Rat=c("LT-IFN","LT-IFN", "LT-IFN", "LT-IFN","LT-IFN", "GFP","GFP","GFP","Control","Control", "Control")


plot_data=data.frame("average_paranodal_length"=average_paranodal_length,"microglia"=number_microglia, 
                     "astrocytes"=number_astrocytes, "overlapping"=overlap_50, "Rat"=Rat
                     )


write.csv(data, file="data.csv")


lm( number_microglia ~ number_astrocytes)
a=cor(number_astrocytes,number_microglia, method="pearson")

plot(number_microglia,number_astrocytes)
p<-ggplot(data=plot_data,aes(x=microglia, y=astrocytes))+
  geom_point(aes(color=Rat,alpha=0.5, size=1))+
  geom_smooth(method=lm, se=FALSE,color="black", size=1)+
  theme_bw()+xlab("Number of microglia (#)")+ylab("a)")+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)


l<-ggplot(data=plot_data,aes(x=astrocytes, y=overlapping))+
  geom_point(aes(color=Rat,alpha=0.5, size=1))+
  geom_smooth(method=lm,se=FALSE, color="black", size=1)+
  theme_bw()+xlab("Number of astrocytes (#)")+ylab("Overlapping")+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
  
print(l)


q<-ggplot(data=plot_data,aes(x=astrocytes, y=average_paranodal_length))+
  geom_point(aes(color=Rat,alpha=0.5, size=2))+
  geom_smooth(method=lm,se=FALSE, color="black", size=1)+
  theme_bw()+xlab("Number of astrocytes (#)")+ylab("Paranodal Length (µm)")+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(q)

s<-ggplot(data=plot_data,aes(x=astrocytes, y=overlapping))+
  geom_point(aes(color=Rat,alpha=0.5, size=1))+
  geom_smooth(method=lm,se=FALSE, color="black", size=1)+
  theme_bw()+xlab("Number of astrocytes (#)")+ylab("Overlapping")+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(s)


K<-ggplot(data=plot_data,aes(x=average_paranodal_length, y=overlap_50))+
  geom_point(aes(color=Rat,alpha=0.5, size=1))+
  geom_smooth(method=lm, se=FALSE,color="black", size=1)+
  theme_bw()+ylab("% of overlap regions (Intensity Threshold=50) ")+xlab("Mean Paranodal Length (µm)")+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(K)



tiff("paranodal.tiff",width=12, height=6,units = 'in', res=400)
p<-ggplot(data=plot_data,aes(x=microglia, y=astrocytes))+
  geom_point(aes(size=average_paranodal_length, color=Rat), alpha=0.5)+
  scale_size(range = c(5, 20))+
  theme_bw()+xlab("Number of microglia (#)")+ylab("Number of astrocytes(#)")+
  guides(size=guide_legend("Paranodal Length (µm)"))+
  guides(color=guide_legend("Rat Model"))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue") )+
  scale_fill_manual(values = alpha(c("gray54", "sandybrown", "cornflowerblue")))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)

dev.off() 

tiff("channels.tiff",width=12, height=6,units = 'in', res=400)
ggplot(data=plot_data,aes(x=microglia, y=astrocytes))+
  geom_point(aes(size=overlapping, color=Rat), alpha=0.5)+
  scale_size(range = c(5, 20))+
  theme_bw()+xlab("Number of microglia (#)")+ylab("Number of astrocytes(#)")+
  theme(axis.text=element_text(size=24),title=element_text(size=24))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  guides(size=guide_legend("% overlap regions "))+
  guides(color=guide_legend("Rat Model"))+
  guides(colour = guide_legend(override.aes = list(size=10)))+
  scale_color_manual(values=c("gray54", "sandybrown", "cornflowerblue"))+
  scale_fill_manual(values = alpha(c("gray54", "sandybrown", "cornflowerblue")))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
dev.off()


library(ggplot2)

plot_ly(plot_data, x = ~number_microglia, y = ~number_astrocytes, z = ~average_paranodal_length) %>%
  add_markers(color = ~average_paranodal_length)
