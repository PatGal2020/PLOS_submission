rm(list =ls())
gc()
overall.wd="D:/cytokines_cultures/Measurements_3doses"
setwd(overall.wd) 
install.packages("ggplot2")
library(ggplot2)

max_length=200


#############3doses########################

slice1=read.csv("slice1.csv", header=TRUE, sep=",")
slice1=slice1$Length

slice2=read.csv("slice2.csv", header=TRUE, sep=",")
slice2=slice2$Length

slice3=read.csv("slice3.csv", header=TRUE, sep=",")
slice3=slice3$Length

slice4=read.csv("slice4.csv", header=TRUE, sep=",")
slice4=slice4$Length

slice5=read.csv("slice5.csv", header=TRUE, sep=",")
slice5=slice5$Length

slice6=read.csv("slice6.csv", header=TRUE, sep=",")
slice6=slice6$Length

#############2doses#######################


slice7=read.csv("slice1_2doses.csv", header=TRUE, sep=",")
slice7=slice7$Length

slice8=read.csv("slice2_2doses.csv", header=TRUE, sep=",")
slice8=slice8$Length

slice9=read.csv("slice3_2doses.csv", header=TRUE, sep=",")
slice9=slice9$Length

slice10=read.csv("slice4_2doses.csv", header=TRUE, sep=",")
slice10=slice10$Length
slice10=slice10[1:200]


###############Controls########

slice12=read.csv("Control3.csv", header=TRUE, sep=",")
slice12=slice12$Length

slice13=read.csv("Control4.csv", header=TRUE, sep=",")
slice13=slice13$Length

slice14=read.csv("Control5.csv", header=TRUE, sep=",")
slice14=slice14$Length

slice15=read.csv("Control6.csv", header=TRUE, sep=",")
slice15=slice15$Length



###############Microglia#######
slice16=read.csv("Slice1_microglia.csv", header=TRUE, sep=",")
slice16=slice16$Length[1:200]

slice17=read.csv("Slice2_microglia.csv", header=TRUE, sep=",")
slice17=slice17$Length[1:200]

slice18=read.csv("Slice3_microglia.csv", header=TRUE, sep=",")
slice18=slice18$Length[1:200]

slice19=read.csv("Slice4_microglia.csv", header=TRUE, sep=",")
slice19=slice19$Length[1:200]


slice20=read.csv("Slice5_microglia.csv", header=TRUE, sep=",")
slice20=slice20$Length[1:200]

################Glutamate 75uM############### 

slice21=read.csv("Slice1_75uM.csv", header=TRUE, sep=",")
slice21=slice21$Length[1:200]

slice22=read.csv("Slice2_75uM.csv", header=TRUE, sep=",")
slice22=slice22$Length[1:200]

slice23=read.csv("Slice3_75uM.csv", header=TRUE, sep=",")
slice23=slice23$Length[1:200]

slice24=read.csv("Slice4_75uM.csv", header=TRUE, sep=",")
slice24=slice24$Length[1:200]


################Glutamate 100uM############### 


slice25=read.csv("Slice1_100.csv", header=TRUE, sep=",")
slice25=slice25$Length

slice26=read.csv("Slice2_100.csv", header=TRUE, sep=",")
slice26=slice26$Length





paranodes_length_3doses=c(slice1, slice2, slice3, slice4, slice5, slice6)

lesstwo_three=0
less4more2_three=0
less5more4_three=0
morethanfive_three=0
for ( i in paranodes_length_3doses) {
  if (i<2){
    lesstwo_three=lesstwo_three+1
  }
  if (i>2 && i<4 ){
    less4more2_three=less4more2_three+1
  }
  if (i>4 && i<5 ){
    less5more4_three=less5more4_three+1
  }
  if (i>5){
    morethanfive_three=morethanfive_three+1
  }

}

paranodes_length_2doses=c(slice7, slice8, slice9, slice10)

lesstwo_two=0
less4more2_two=0
less5more4_two=0
morethanfive_two=0
for ( i in paranodes_length_2doses) {
  if (i<2){
    lesstwo_two=lesstwo_two+1
  }
  if (i>2 && i<4 ){
    less4more2_two=less4more2_two+1
  }
  if (i>4 && i<5 ){
    less5more4_two=less5more4_two+1
  }
  if (i>5){
    morethanfive_two=morethanfive_two+1
  }
  
}


paranodes_length_controls=c(slice12, slice13, slice14, slice15)

lesstwo_controls=0
less4more2_controls=0
less5more4_controls=0
morethanfive_controls=0
for ( i in paranodes_length_controls) {
  if (i<2){
    lesstwo_controls=lesstwo_controls+1
  }
  if (i>2 && i<4 ){
    less4more2_controls=less4more2_controls+1
  }
  if (i>4 && i<5 ){
    less5more4_controls=less5more4_controls+1
  }
  if (i>5){
    morethanfive_controls=morethanfive_controls+1
  }
}


paranodes_length_microglia=c(slice16, slice17, slice18, slice19, slice20)

lesstwo_microglia=0
less4more2_microglia=0
less5more4_microglia=0
morethanfive_microglia=0
for ( i in paranodes_length_microglia) {
  if (i<2){
    lesstwo_microglia=lesstwo_microglia+1
  }
  if (i>2 && i<4 ){
    less4more2_microglia=less4more2_microglia+1
  }
  if (i>4 && i<5 ){
    less5more4_microglia=less5more4_microglia+1
  }
  if (i>5){
    morethanfive_microglia=morethanfive_microglia+1
  }
}



paranodes_length_75=c(slice21, slice22, slice23, slice24)

lesstwo_75=0
less4more2_75=0
less5more4_75=0
morethanfive_75=0
for ( i in paranodes_length_75) {
  if (i<2){
    lesstwo_75=lesstwo_75+1
  }
  if (i>2 && i<4 ){
    less4more2_75=less4more2_75+1
  }
  if (i>4 && i<5 ){
    less5more4_75=less5more4_75+1
  }
  if (i>5){
    morethanfive_75=morethanfive_75+1
  }
}



paranodes_length_100=c(slice25, slice26)


lesstwo_100=0
less4more2_100=0
less5more4_100=0
morethanfive_100=0
for ( i in paranodes_length_100) {
  if (i<2){
    lesstwo_100=lesstwo_100+1
  }
  if (i>2 && i<4 ){
    less4more2_100=less4more2_100+1
  }
  if (i>4 && i<5 ){
    less5more4_100=less5more4_100+1
  }
  if (i>5){
    morethanfive_100=morethanfive_100+1
  }
}




paranodes_length_cytokine=c(paranodes_length_3doses,paranodes_length_2doses, paranodes_length_controls )
type_cytokine= c(rep("three",1200), rep("two", 800), rep("control", 800))


mean(paranodes_length_2doses)
mean(paranodes_length_controls)

total_percentages_cytokines=c(morethanfive_three/1200,less5more4_three/1200, 
                    less4more2_three/1200, lesstwo_three/1200, 
                    morethanfive_two/800,less5more4_two/800, less4more2_two/800, lesstwo_two/800, 
                    morethanfive_controls/800,less5more4_controls/800,
                    less4more2_controls/800, lesstwo_controls/800)

range_type=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm",
             "L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm")
class_type=c("e","e","e","e",
             "d","d","d","d",
             "Control","Control","Control","Control")

data_range_cytokines=data.frame("range"=range_type,"percentage"=total_percentages_cytokines, "class"=class_type)


paranodes_length_microglial=c(paranodes_length_microglia, paranodes_length_75, paranodes_length_controls )


type_microglia= c(rep("microglia",1000), rep("glutamate", 800), rep("control", 800))


all_groups=c(paranodes_length_3doses,paranodes_length_2doses, paranodes_length_controls,
             paranodes_length_microglia, paranodes_length_75,  paranodes_length_100)

type_all=c(rep("e",1200), rep("d", 800), rep("control", 800), rep("g",1000), 
           rep("f", 800),  rep("h", 146))
all_frame=data.frame("Length"=all_groups, "Treatment"=type_all)



total_percentages_microglia=c(morethanfive_microglia/1000,less5more4_microglia/1000, 
                              less4more2_microglia/1000, lesstwo_microglia/1000, 
                              morethanfive_75/800,less5more4_75/800, 
                              less4more2_75/800, lesstwo_75/800, 
                              morethanfive_controls/800,less5more4_controls/800,
                              less4more2_controls/800, lesstwo_controls/800)



total_percentages_all=c(morethanfive_three/1200,less5more4_three/1200, 
                              less4more2_three/1200, lesstwo_three/1200, 
                              morethanfive_two/800,less5more4_two/800, less4more2_two/800, lesstwo_two/800, 
                              morethanfive_controls/800,less5more4_controls/800,
                              less4more2_controls/800, lesstwo_controls/800, 
                        morethanfive_microglia/1000,less5more4_microglia/1000, 
                        less4more2_microglia/1000, lesstwo_microglia/1000, 
                        morethanfive_75/800,less5more4_75/800, 
                        less4more2_75/800, lesstwo_75/800, 
                        morethanfive_100/146,less5more4_100/146, 
                        less4more2_100/146, lesstwo_100/146)

range_type_all=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm",
                 "L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm",
                 "L>5µm",
                 "5µm<L<4µm", "4µm<L<2µm","L<2µm",   "L>5µm",
                 "5µm<L<4µm", "4µm<L<2µm","L<2µm" )

class_all=c("e","e","e","e",
            "d","d","d","d",
            "Control","Control","Control","Control", "g","g","g","g",
            "f","f","f","f",
            "h","h","h","h")


data_range_all=data.frame("range"=range_type_all,
                                "percentage"=total_percentages_all, 
                                "class"=class_all)



range_type=c("L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm","L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm",
             "L>5µm", "5µm<L<4µm", "4µm<L<2µm","L<2µm")
class_type_microglia=c("Microglia","Microglia","Microglia","Microglia",
             "Glutamate","Glutamate","Glutamate","Glutamate",
             "Control","Control","Control","Control")

data_range_microglia=data.frame("range"=range_type,
                                "percentage"=total_percentages_microglia, 
                                "class"=class_type_microglia)


paranode_dataframe=data.frame("Length"=paranodes_length_cytokine, "Treatment"=type_cytokine )
paranode_dataframe_microglia=data.frame("Length"=paranodes_length_microglial, "Treatment"=type_microglia)





pdf("paranodal_cytokine_plot.pdf", width = 9, height = 6)
p<-ggplot(paranode_dataframe,aes(x=Treatment,y=Length))+coord_flip()+
  geom_jitter(aes(fill=Treatment),width = 0.2, color="gray",alpha=0.3)+
  geom_boxplot(aes(fill=Treatment), alpha=0.6,notch=TRUE)+
  scale_fill_manual(values=c("#f39b7fb2",  "#3c5488b2", "gray"))+
  ylab("Paranodal Length(µm)")+
  xlab("Treatment")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)
dev.off()


pdf("percentage_paranode_cytokines.pdf", width = 11, height = 9)
s <- ggplot(data_range_cytokines, aes(x=class, y=percentage, 
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
print(s)
dev.off()



pdf("paranodal_microglia_plot.pdf", width = 9, height = 6)
m<-ggplot(paranode_dataframe_microglia,aes(x=Treatment,y=Length))+
  coord_flip()+
  geom_jitter(aes(fill=Treatment),width = 0.2, color="gray",alpha=0.3)+
  geom_boxplot(aes(fill=Treatment), alpha=0.5,notch=TRUE)+
  scale_fill_manual(values=c("#f39b7fb2",  "skyblue3","palevioletred" ))+
  ylab("Paranodal Length(µm)")+
  xlab("Treatment")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(m)
dev.off()




pdf("percentage_paranode_microglia.pdf", width = 11, height = 9)
h <- ggplot(data_range_microglia, aes(x=class, y=percentage, 
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
print(h)
dev.off()


pdf("ALL_plot2.pdf", width = 9, height = 6)
G<-ggplot(all_frame,aes(x=Treatment,y=Length))+ coord_flip()+
  geom_jitter(aes(fill=Treatment),width = 0.2, color="gray",alpha=0.3)+
  geom_boxplot(aes(fill=Treatment), outlier.shape=NA, alpha=0.5,notch=TRUE)+ 
  scale_fill_manual(values=c("#f39b7fb2", "gray", "#3c5488b2","skyblue3","palevioletred",
                             "black"))+
  scale_y_continuous(trans="log2")+ 
  ylab("Paranodal Length(µm)")+
  xlab("Treatment")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(G)
dev.off()


pdf("ranges_all.pdf", width = 11, height = 9)
j <- ggplot(data_range_all, aes(x=class, y=percentage, 
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
print(j)
dev.off()



kruskal.test(Length ~ Treatment, data=all_frame)

pairwise.wilcox.test(all_frame$Length, all_frame$Treatment,p.adjust.method="bonferroni"
                    )

wilcox.test(paranodes_length_75, paranodes_length_100)


mean(c(paranodes_length_100))

wilcox.test(paranodes_length_2doses,paranodes_length_3doses)
wilcox.test(paranodes_length_3doses,paranodes_length_controls)
wilcox.test(paranodes_length_2doses,paranodes_length_controls)

wilcox.test(paranodes_length_microglia, paranodes_length_controls)
wilcox.test(paranodes_length_microglia, paranodes_length_75)
wilcox.test(paranodes_length_microglia, paranodes_length_3doses)

mean(paranodes_length_3doses)
mean(paranodes_length_microglia)


