rm(list =ls())
gc()
library(ggplot2)
library(gridExtra)
setwd("C:/Users/pg2015/Desktop/Measurements/caspr-measurements/new_graph")

controls=read.csv("controls.csv", header=T)
patients=read.csv("patients.csv", header=T)


morethanfive=c(controls$one)
morethanfive <- morethanfive[!is.na(morethanfive)]
less5more4=c(controls$two)
less5more4 <- less5more4[!is.na(less5more4)]
less4more2=c(controls$three)
less4more2 <- less4more2[!is.na(less4more2)]
lesstwo=c(controls$four)
lesstwo<- lesstwo[!is.na(lesstwo)]



lengths=c(morethanfive,less5more4,less4more2,lesstwo)


#ggplot2, we need to convert the data to a data frame.  

type=c(rep("morethanfive",length(morethanfive)),rep("less5more4",length(less5more4)),rep("less4more2",length(less4more2)),
       rep("lesstwo",length(lesstwo)))
dat=data.frame("Class"=type,"Length"=lengths)




p <- ggplot(dat, aes(x=Class, y=Length)) + 
  geom_boxplot(color = "black", fill = "red", alpha=0.8, notch=TRUE)+
  ylab("Caspr Length(µm)")+theme_bw()+
  xlab("Controls")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(lineheight=3, color="black", size=20))+
  scale_x_discrete(labels=c("lesstwo" = "Length<2", "less4more2" = "4>Length>2",
                                                 "less5more4" = "4>Length>5", "morethanfive"="Length>5"), 
                   limits=c("lesstwo","less4more2","less5more4","morethanfive"))+
  ggtitle("Paranodal Length Distributions")+theme(axis.text=element_text(size=10),
                                                  axis.title=element_text(size=18))+
  scale_y_continuous(limits=c(0,10))

print(p)




morethanfive_p=c(patients$one)
morethanfive_p <- morethanfive_p[!is.na(morethanfive_p)]
less5more4_p=c(patients$two)
less5more4_p <- less5more4_p[!is.na(less5more4_p)]
less4more2_p=c(patients$three)
less4more2_p <- less4more2_p[!is.na(less4more2_p)]
lesstwo_p=c(patients$four)
lesstwo_p<- lesstwo_p[!is.na(lesstwo_p)]



lengths_p=c(morethanfive_p,less5more4_p,less4more2_p,lesstwo_p)


#ggplot2, we need to convert the data to a data frame.  

type_p=c(rep("morethanfive_p",length(morethanfive_p)),rep("less5more4_p",length(less5more4_p)),
       rep("less4more2_p",length(less4more2_p)),
       rep("lesstwo_p",length(lesstwo_p)))
dat_p=data.frame("Class"=type_p,"Length"=lengths_p)


q <- ggplot(dat_p, aes(x=Class, y=Length)) + 
  geom_boxplot(color = "black", fill = "blue", alpha=0.8, notch=TRUE)+
  ylab("Caspr Length(µm)")+theme_bw()+
  xlab("Patients")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  ggtitle("Paranodal Length Distributions")+theme(axis.text=element_text(size=10),
                                                  axis.title=element_text(size=18))+
  theme(plot.title = element_text(lineheight=3, color="black", size=20))+
  scale_x_discrete(labels=c("lesstwo_p" = "Length<2", "less4more2_p" = "4>Length>2",
                            "less5more4_p" = "4>Length>5", "morethanfive_p"="Length>5"), 
                   limits=c("lesstwo_p","less4more2_p","less5more4_p","morethanfive_p"))+
  scale_y_continuous(limits=c(0,10))

print(q)

grid.arrange(p, q,nrow=1)


####ranges#####
##order (morethanfive, less5more, less4more2,lesstwo)

control_percentages=c(0.97, 3.13,58.19,37.7)
patient_percentages=c(4.09, 7.05, 68.15, 20.71)







total_percentages=c(control_percentages, patient_percentages)
range_type=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm")
class_type=c("Control","Control","Control","Control","Patient","Patient","Patient","Patient")

data_range=data.frame("range"=range_type,"percentage"=total_percentages, "class"=class_type)

pdf("percentage-paranode.pdf", width = 11, height = 9)
s <- ggplot(data_range, aes(x=class, y=percentage, 
                      fill=factor(range,levels=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm"))))+
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
