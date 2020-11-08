rm(list =ls())
gc()
overall.wd="C:/Users/pg2015/Desktop/cytokines_cultures/nmda blocker-measurements"
setwd(overall.wd) 
library(ggplot2)

max_length=200

###############Controls########

slice12=read.csv("Control3.csv", header=TRUE, sep=",")
slice12=slice12$Length

slice13=read.csv("Control4.csv", header=TRUE, sep=",")
slice13=slice13$Length

slice14=read.csv("Control5.csv", header=TRUE, sep=",")
slice14=slice14$Length

slice15=read.csv("Control6.csv", header=TRUE, sep=",")
slice15=slice15$Length

###############Blocker 100ng/ml########

slice1=read.csv("Slice1-nmda-100.csv", header=TRUE, sep=",")
slice1=slice1$Length

slice2=read.csv("Slice2-nmda-100.csv", header=TRUE, sep=",")
slice2=slice2$Length

slice3=read.csv("Slice3-nmda-100.csv", header=TRUE, sep=",")
slice3=slice3$Length

slice4=read.csv("Slice4-nmda-100.csv", header=TRUE, sep=",")
slice4=slice4$Length

slice5=read.csv("Slice5-nmda-100.csv", header=TRUE, sep=",")
slice5=slice5$Length

########################two doses##############################

slice7=read.csv("slice1_2doses.csv", header=TRUE, sep=",")
slice7=slice7$Length

slice8=read.csv("slice2_2doses.csv", header=TRUE, sep=",")
slice8=slice8$Length

slice9=read.csv("slice3_2doses.csv", header=TRUE, sep=",")
slice9=slice9$Length

slice10=read.csv("slice4_2doses.csv", header=TRUE, sep=",")
slice10=slice10$Length
slice10=slice10[1:200]



paranodes_blocker_100=c(slice1, slice2, slice3, slice4, slice5)
paranodes_blocker_control=c(slice12,slice13, slice14,slice15)
paranodes_blocker_twodoses=c(slice7, slice8, slice9, slice10)



lesstwo_blocker_100=0
less4more2_blocker_100=0
less5more4_blocker_100=0
morethanfive_blocker_100=0
for ( i in paranodes_blocker_100) {
  if (i<2){
    lesstwo_blocker_100=lesstwo_blocker_100+1
  }
  if (i>2 && i<4 ){
    less4more2_blocker_100=less4more2_blocker_100+1
  }
  if (i>4 && i<5 ){
    less5more4_blocker_100=less5more4_blocker_100+1
  }
  if (i>5){
    morethanfive_blocker_100=morethanfive_blocker_100+1
  }
}



lesstwo_blocker_control=0
less4more2_blocker_control=0
less5more4_blocker_control=0
morethanfive_blocker_control=0
for ( i in paranodes_blocker_control) {
  if (i<2){
    lesstwo_blocker_control=lesstwo_blocker_control+1
  }
  if (i>2 && i<4 ){
    less4more2_blocker_control=less4more2_blocker_control+1
  }
  if (i>4 && i<5 ){
    less5more4_blocker_control=less5more4_blocker_control+1
  }
  if (i>5){
    morethanfive_blocker_control=morethanfive_blocker_control+1
  }
}


lesstwo_blocker_twodoses=0
less4more2_blocker_twodoses=0
less5more4_blocker_twodoses=0
morethanfive_blocker_twodoses=0
for ( i in paranodes_blocker_twodoses) {
  if (i<2){
    lesstwo_blocker_twodoses=lesstwo_blocker_twodoses+1
  }
  if (i>2 && i<4 ){
    less4more2_blocker_twodoses=less4more2_blocker_twodoses+1
  }
  if (i>4 && i<5 ){
    less5more4_blocker_twodoses=less5more4_blocker_twodoses+1
  }
  if (i>5){
    morethanfive_blocker_twodoses=morethanfive_blocker_twodoses+1
  }
}








total_percentages_blocker=c(morethanfive_blocker_100/1000,less5more4_blocker_100/1000, 
                              less4more2_blocker_100/1000, lesstwo_blocker_100/1000, 
                              morethanfive_blocker_control/800,less5more4_blocker_control/800,
                              less4more2_blocker_control/800, lesstwo_blocker_control/800, 
                             morethanfive_blocker_twodoses/800,less5more4_blocker_twodoses/800,
                            less4more2_blocker_twodoses/800, lesstwo_blocker_twodoses/800
                            
                            )

range_type=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm", 
             "L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm")


class_type=c("d","d","d","d",
             "Control","Control","Control","Control", 
             "two", "two", "two","two")


data_range_blocker=data.frame("range"=range_type,"percentage"=total_percentages_blocker, "class"=class_type)



paranodes_length_total=c(paranodes_blocker_100, paranodes_blocker_control, paranodes_blocker_twodoses)
type= c( rep("d", 1000), rep("control", 800), rep("two", 800))


blocker_frame=data.frame("Length"=paranodes_length_total, "Treatment"=type)

pdf("paranode_blocker.pdf", width = 11, height = 9)
b<-ggplot(blocker_frame,aes(x=Treatment,y=Length))+
  coord_flip()+
  geom_jitter(aes(fill=Treatment),width = 0.2, color="gray",alpha=0.3)+
  geom_boxplot(aes(fill=Treatment), alpha=0.5,notch=TRUE)+
  scale_fill_manual(values=c("#f39b7fb2",  "dimgray", "gray"))+
  ylab("Paranodal Length(µm)")+
  xlab("Treatment")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(b)
dev.off()


pdf("percentage_paranode_blocker.pdf", width = 11, height = 9)
o <- ggplot(data_range_blocker, aes(x=class, y=percentage, 
                                      fill=factor(range,levels=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm"))))+
  geom_bar( stat="identity",alpha=0.5)+
  theme_bw()+
  scale_fill_manual(values=c("mediumpurple1",  "#3c5488b2","gray", "black" ))+
  scale_y_continuous(expand=c(0,0))+
  theme(text = element_text(size=20))+
  guides(fill=guide_legend(title="Case"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(o)
dev.off()











kruskal.test(Length ~ Treatment, data=blocker_frame)
pairwise.wilcox.test(blocker_frame$Length, blocker_frame$Treatment,p.adjust.method="bonferroni"
)
pairwise.wilcox.test(blocker_frame$Length, blocker_frame$Treatment,p.adjust.method="bonferroni"
)



wilcox.test(paranodes_blocker_100, paranodes_blocker_control)
wilcox.test(paranodes_blocker_twodoses, paranodes_blocker_100)

