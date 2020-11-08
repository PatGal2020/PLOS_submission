rm(list =ls())
gc()
library(ggplot2)
overall.wd="C:/Users/pg2015/Desktop/rats/rat_code/measurements"
setwd(overall.wd) 



data=read.table("caspr.txt")
rat1=data$V1
rat2=data$V2
rat3=data$V3
rat4=data$V4
rat5=data$V5
rat6=data$V6
rat7=data$V7
rat8=data$V8
rat9=data$V9
rat10=data$V10
rat11=data$V11

IFN=c(rat1,rat2,rat3,rat4,rat5)
GFP=c(rat6,rat7,rat8)
GFP = c(GFP, rep(NA,1000))
GFP = GFP [1:1005]
GFP=c(GFP,rm.na=T)
Control=c(rat9,rat10,rat11)
Control = c(Control, rep(NA,1000))
Control = Control [1:1005]

wilcox.test(IFN,Control)
wilcox.test(GFP,Control)
wilcox.test(IFN,GFP)

total_lengths_rat=cbind(IFN,GFP,Control)

total_lengths_rat = as.data.frame(total_lengths_rat)

#ggplot2, we need to convert the data to a data frame.  
tot=c(total_lengths_rat[,1],total_lengths_rat[,2],total_lengths_rat[,3])
type=c(rep("LT-IFN",nrow(total_lengths_rat)),rep("GFP",nrow(total_lengths_rat)),
       rep("Control",nrow(total_lengths_rat)))
dat=data.frame("Class"=type,"Length"=tot)
dat = dat[!is.na(dat$Length),]

pdf("paranodal_case_plots.pdf", width = 9, height = 6)
p<-ggplot(dat,aes(x=Class,y=Length))+
  geom_jitter(aes(fill=Class),width = 0.2, color="gray",alpha=0.5)+
  geom_boxplot(aes(fill=Class),alpha=0.5,notch=TRUE)+coord_flip()+
  scale_fill_manual(values=c("darkgray", "chocolate1","cornflowerblue" ))+
  ylab("Length(µm)")+
  xlab("Group")+
  guides(fill=guide_legend(title="Group"))+
  theme_bw()+
  theme(text = element_text(size=20))+
  theme(axis.text =element_text(color="black",size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)
dev.off()

#ggplot2 all cases
data1=data.frame(rat1,rat2,rat3,rat4,rat5,rat6,rat7,rat8,rat9,rat10,rat11)
tot2=c(data1[,1],data1[,2],data1[,3],data1[,4],data1[,5],data1[,6],data1[,7],data1[,8],data1[,9],
       data1[,10],data1[,11])

type2=c(rep("IFN-1",nrow(data1)),rep("IFN-2",nrow(data1)),rep("IFN-3",nrow(data1)),
        rep("IFN-4",nrow(data1)),rep("IFN-5",nrow(data1)),rep("GFP-1",nrow(data1)),
        rep("GFP-2",nrow(data1)),rep("GFP-3",nrow(data1)),rep("Control-1",nrow(data1)),
        rep("Control-2",nrow(data1)),rep("Control-3",nrow(data1)))


data2=data.frame("Class"=type2,"Length"=tot2)
data2 = data2[!is.na(data2$Length),]
data2$Class<- factor(data2$Class,levels=unique(data2$Class))

mylabels=c(
           "LT-IFN","LT-IFN","LT-IFN","LT-IFN","LT-IFN",
           "GFP","GFP","GFP","Control","Control","Control")

pdf("paranodal_plots.pdf",width=9, height=7)
S <- ggplot(data2, aes(x=Class, y=Length)) + 
  geom_jitter(aes(fill=Class),width = 0.2, color="gray",alpha=0.5)+
  geom_boxplot(aes(fill=Class),notch=TRUE,alpha=0.5)+coord_flip()+
  scale_fill_manual(
                  values=c( 
                           "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue"
                             ,"cornflowerblue","chocolate1","chocolate1","chocolate1",
"darkgray","darkgray","darkgray"
                             ))+
  ylab("Paranodal Length(µm)")+theme_bw()+
  xlab("Group")+
  theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=mylabels)
 
print(S)
dev.off()
