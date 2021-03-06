rm(list =ls())
gc()
library(ggplot2)
max_length=200
setwd("C:/Users/pg2015/Desktop/Measurements/smi32-caspr me")

ms404c<-read.csv("ms404_cbp.csv")
positives12<-ms404c$Length[ms404c$SMI32==1]
negatives12<-ms404c$Length[ms404c$SMI32==0]
positives12= c(positives12, rep(NA,1000))
positives12= positives12 [1:max_length]
ms404p<-read.csv("ms404_pcg.csv")
positives13<-ms404p$Length[ms404p$SMI32==1]
negatives13<-ms404p$Length[ms404p$SMI32==0]
positives13= c(positives13, rep(NA,1000))
positives13= positives13 [1:max_length]

ms406c<-read.csv("ms406_cbp.csv")
positives28<-ms406c$Length[ms406c$SMI32==1]
negatives28<-ms406c$Length[ms406c$SMI32==0]
positives28= c(positives28, rep(NA,1000))
positives28= positives28 [1:max_length]
ms406p<-read.csv("ms406_pcg.csv")
positives27<-ms406p$Length[ms406p$SMI32==1]
negatives27<-ms406p$Length[ms406p$SMI32==0]
positives27= c(positives27, rep(NA,1000))
positives27= positives27 [1:max_length]




ms461p<-read.csv("ms461_pcg.csv")
positives1<-ms461p$Length[ms461p$SMI32==1]
negatives1<-ms461p$Length[ms461p$SMI32==0]
positives1= c(positives1, rep(NA,1000))
positives1= positives1 [1:max_length]
ms461c<-read.csv("ms461_cbp.csv", sep=";")
positives2<-ms461c$Length[ms461c$SMI32==1]
negatives2<-ms461c$Length[ms461c$SMI32==0]
positives2= c(positives2, rep(NA,1000))
positives2= positives2 [1:max_length]



ms466c<-read.csv("ms466_cbp.csv")
positives17<-ms466c$Length[ms466c$SMI32==1]
negatives17<-ms466c$Length[ms466c$SMI32==0]
positives17= c(positives17, rep(NA,1000))
positives17= positives17 [1:max_length]
ms466p<-read.csv("ms466_pcg.csv")
positives16<-ms466p$Length[ms466p$SMI32==1]
negatives16<-ms466p$Length[ms466p$SMI32==0]
positives16= c(positives16, rep(NA,1000))
positives16= positives16 [1:max_length]

ms478c<-read.csv("ms478_cbp.csv")
positives8<-ms478c$Length[ms478c$SMI32==1]
negatives8<-ms478c$Length[ms478c$SMI32==0]
positives8= c(positives8, rep(NA,1000))
positives8= positives8 [1:max_length]
ms478p<-read.csv("ms478_pcg.csv",sep=";")
positives9<-ms478p$Length[ms478p$SMI32==1]
negatives9<-ms478p$Length[ms478p$SMI32==0]
positives9= c(positives9, rep(NA,1000))
positives9= positives9[1:max_length]

ms510p<-read.csv("ms510_pcg.csv")
positives5<-ms510p$Length[ms510p$SMI32==1]
negatives5<-ms510p$Length[ms510p$SMI32==0]
positives5= c(positives5, rep(NA,1000))
positives5= positives5 [1:max_length]
ms510c<-read.csv("ms510_cbp.csv")
positives6<-ms510c$Length[ms510c$SMI32==1]
negatives6<-ms510c$Length[ms510c$SMI32==0]
positives6= c(positives6, rep(NA,1000))
positives6= positives6 [1:max_length]

ms535c<-read.csv("ms535_cbp.csv")
positives10<-ms535c$Length[ms535c$SMI32==1]
negatives10<-ms535c$Length[ms535c$SMI32==0]
positives10= c(positives10, rep(NA,1000))
positives10= positives10[1:max_length]
ms535p<-read.csv("ms535_pcg.csv",sep=";")
positives11<-ms535p$Length[ms535p$SMI32==1]
negatives11<-ms535p$Length[ms535p$SMI32==0]
positives11= c(positives11, rep(NA,1000))
positives11= positives11[1:max_length]

ms567c<-read.csv("ms567_cbp.csv")
positives23<-ms567c$Length[ms567c$SMI32==1]
negatives23<-ms567c$Length[ms567c$SMI32==0]
positives23= c(positives23, rep(NA,1000))
positives23= positives23 [1:max_length]
ms567p<-read.csv("ms567_pcg.csv")
positives22<-ms567p$Length[ms567p$SMI32==1]
negatives22<-ms567p$Length[ms567p$SMI32==0]
positives22= c(positives22, rep(NA,1000))
positives22= positives22 [1:max_length]

ms444p<-read.csv("ms444_pcg.csv")
positives7<-ms444p$Length[ms444p$SMI32==1]
negatives7<-ms444p$Length[ms444p$SMI32==0]
positives7= c(positives7, rep(NA,1000))
positives7= positives7 [1:max_length]

ms530c<-read.csv("ms530_cbp.csv")
positives31<-ms530c$Length[ms535c$SMI32==1]
negatives31<-ms530c$Length[ms535c$SMI32==0]
positives31= c(positives31, rep(NA,1000))
positives31= positives31 [1:max_length]
ms530p<-read.csv("ms530_pcg.csv")
positives32<-ms530p$Length[ms530p$SMI32==1]
negatives32<-ms530p$Length[ms530p$SMI32==0]
positives32= c(positives32, rep(NA,1000))
positives32= positives32 [1:max_length]

ms542p<-read.csv("ms542_pcg.csv")
positives24<-ms542p$Length[ms542p$SMI32==1]
negatives24<-ms542p$Length[ms542p$SMI32==0]
positives24= c(positives24, rep(NA,1000))
positives24= positives24 [1:max_length]

ms584c<-read.csv("ms584_cbp.csv")
positives15<-ms584c$Length[ms584c$SMI32==1]
negatives15<-ms584c$Lengt[ms584c$SMI32==0]
positives15= c(positives15, rep(NA,1000))
positives15= positives15 [1:max_length]
ms584p<-read.csv("ms584_pcg.csv",sep=";")
positives14<-ms584p$Length[ms584p$SMI32==1]
negatives14<-ms584p$Length[ms584p$SMI32==0]
positives14= c(positives14, rep(NA,1000))
positives14= positives14 [1:max_length]

ms585c<-read.csv("ms585_cbp.csv")
positives33<-ms585c$Length[ms585c$SMI32==1]
negatives33<-ms585c$Length[ms585c$SMI32==0]
positives33= c(positives33, rep(NA,1000))
positives33= positives33 [1:max_length]
ms585p<-read.csv("ms585_pcg.csv")
positives34<-ms585p$Length[ms585p$SMI32==1]
negatives34<-ms585p$Length[ms585p$SMI32==0]
positives34= c(positives34, rep(NA,1000))
positives34= positives34 [1:max_length]

ms587p<-read.csv("ms587_pcg.csv")
positives21<-ms587p$Length[ms587p$SMI32==1]
negatives21<-ms587p$Length[ms587p$SMI32==0]
positives21= c(positives21, rep(NA,1000))
positives21= positives21 [1:max_length]

ms411c<-read.csv("ms411_cbp.csv")
positives20<-ms411c$Length[ms411c$SMI32==1]
negatives20<-ms411c$Length[ms411c$SMI32==0]
positives20= c(positives20, rep(NA,1000))
positives20= positives20 [1:max_length]

ms422c<-read.csv("ms422_cbp.csv")
positives19<-ms422c$Length[ms422c$SMI32==1]
negatives19<-ms422c$Length[ms422c$SMI32==0]
positives19= c(positives19, rep(NA,1000))
positives19= positives19 [1:max_length]

ms500c<-read.csv("ms500_cbp.csv")
positives4<-ms500c$Length[ms500c$SMI32==1]
negatives4<-ms500c$Length[ms500c$SMI32==0]
positives4= c(positives4, rep(NA,1000))
positives4= positives4 [1:max_length]
ms500p<-read.csv("ms500_pcg.csv",sep=";")
positives3<-ms500p$Length[ms500p$SMI32==1]
negatives3<-ms500p$Length[ms500p$SMI32==0]
positives3= c(positives3, rep(NA,1000))
positives3= positives3 [1:max_length]

ms517c<-read.csv("ms517_cbp.csv")
positives25<-ms517c$Length[ms517c$SMI32==1]
negatives25<-ms517c$Length[ms517c$SMI32==0]
positives25= c(positives25, rep(NA,1000))
positives25= positives25 [1:max_length]
ms517p<-read.csv("ms517_pcg.csv")
positives26<-ms517p$Length[ms517p$SMI32==1]
negatives26<-ms517p$Length[ms517p$SMI32==0]
positives26= c(positives26, rep(NA,1000))
positives26= positives26 [1:max_length]

ms523p<-read.csv("ms523_pcg.csv")
positives30<-ms523p$Length[ms523p$SMI32==1]
negatives30<-ms523p$Length[ms523p$SMI32==0]
positives30= c(positives30, rep(NA,1000))
positives30= positives30 [1:max_length]
ms523c<-read.csv("ms523_cbp.csv",sep=";")
positives29<-ms523c$Length[ms523c$SMI32==1]
negatives29<-ms523c$Length[ms523c$SMI32==0]
positives29= c(positives29, rep(NA,1000))
positives29= positives29 [1:max_length]

ms549c<-read.csv("ms549_cbp.csv")
positives18<-ms549c$Length[ms549c$SMI32==1]
negatives18<-ms549c$Length[ms549c$SMI32==0]
positives18= c(positives18, rep(NA,1000))
positives18= positives18 [1:max_length]


#cortar todos los controles a #200 measurements

c48p<-read.csv("c48_1.csv")
c48p<-c48p$Length
c48p= c(c48p, rep(NA,1000))
c48p= c48p [1:max_length]


c54p<-read.csv("c54_1.csv")
c54p<-c54p$Length
c54p= c(c54p, rep(NA,1000))
c54p= c54p [1:max_length]


c54c<-read.csv("c54_2.csv")
c54c<-c54c$Length
c54c= c(c54c, rep(NA,1000))
c54c= c54c [1:max_length]

c72p<-read.csv("c72_1.csv")
c72p<-c72p$Length
c72p= c(c72p, rep(NA,1000))
c72p= c72p [1:max_length]

c72c<-read.csv("c72_2.csv")
c72c<-c72c$Length
c72c= c(c72c, rep(NA,1000))
c72c= c72c [1:max_length]

c74p<-read.csv("c74_1.csv")
c74p<-c74p$Length
c74p= c(c74p, rep(NA,1000))
c74p= c74p [1:max_length]

c74c<-read.csv("c74_2.csv")
c74c<-c74c$Length
c74c= c(c74c, rep(NA,1000))
c74c= c74c [1:max_length]

c75p<-read.csv("c75_1.csv")
c75p<-c75p$Length
c75p= c(c75p, rep(NA,1000))
c75p= c75p [1:max_length]


c75c<-read.csv("c75_2.csv")
c75c<-c75c$Length
c75c= c(c75c, rep(NA,1000))
c75c= c75c [1:max_length]

c76p<-read.csv("c76_1.csv")
c76p<-c76p$Length
c76p= c(c76p, rep(NA,1000))
c76p= c76p [1:max_length]
c76c<-read.csv("c76_2.csv")
c76c<-c76c$Length
c76c= c(c76c, rep(NA,1000))
c76c= c76c [1:max_length]


pdc29p<-read.csv("pdc29_1.csv")
pdc29p<-pdc29p$Length
pdc29p= c(pdc29p, rep(NA,1000))
pdc29p= pdc29p [1:max_length]
pdc29c<-read.csv("pdc29_2.csv")
pdc29c<-pdc29c$Length
pdc29c= c(pdc29c, rep(NA,1000))
pdc29c= pdc29c [1:max_length]
pdc39c<-read.csv("pdc39_2.csv")
pdc39c<-pdc39c$Length
pdc39c= c(pdc39c, rep(NA,1000))
pdc39c= pdc39c [1:max_length]
pdc40p<-read.csv("pdc40_1.csv")
pdc40p<-pdc40p$Length
pdc40p= c(pdc40p, rep(NA,1000))
pdc40p= pdc40p [1:max_length]
pdc40c<-read.csv("pdc40_2.csv")
pdc40c<-pdc40c$Length
pdc40c= c(pdc40c, rep(NA,1000))
pdc40c= pdc40c [1:max_length]

maxlength=3904

smi_pos=c(positives1,positives2, positives3,positives4,positives5,positives6,positives7,positives8,positives9,positives10,
          positives11,positives12,positives13,positives14,positives15,positives16,positives17,positives18,positives19,
          positives20,positives21,positives22,positives23,positives24,positives25,positives26,positives27,positives28,
          positives29, positives30,positives31,positives32,positives33,positives34)
smi_neg=c(negatives1,negatives2,negatives3,negatives4,negatives5,negatives6,negatives7,negatives8,negatives9,negatives10,negatives11,
          negatives12,negatives13,negatives14,negatives15,negatives16,negatives17,negatives18,negatives19,
          negatives20,negatives21,negatives22,negatives23,negatives24,negatives25,negatives26,negatives27,
          negatives29,negatives28,negatives30,negatives31,negatives32,negatives33,negatives34)

controls=c(c48p,c54c,c54p,c72c,c72p,c74c,c74p,c75c,c75p,c76c,c76p,pdc29c,pdc29p,pdc39c,
           pdc40c,pdc40p)

#mean(smi_neg)
#sd(smi_neg)/sqrt(760)
#mean(smi_pos)
#sd(smi_pos)/sqrt(933)
#median(smi_neg)
#median(smi_pos)


smi_neg<-c(smi_neg, rep(NA, 1000))
smi_neg<-smi_neg[1:maxlength]
smi_pos<-c(smi_pos, rep(NA, 1000))
smi_pos<-smi_pos[1:maxlength]
controls<-c(controls,rep(NA,1000))
controls<-controls[1:maxlength]

data=data.frame(controls,smi_pos,smi_neg)
tot2=c(data[,1],data[,2],data[,3])
type2=c(rep("Control",nrow(data)),rep("SMI32+",nrow(data)),rep("SMI32-",nrow(data)))
       


data2=data.frame("Class"=type2,"Length"=tot2)
pdf("SMI_plots.pdf", width = 9, height = 6)
p <- ggplot(data2, aes(x=Class, y=Length)) + 
  geom_jitter(aes(fill=Class),width = 0.2, color="gray",alpha=0.5)+
  coord_flip()+
  geom_boxplot(aes(fill = Class), alpha =0.5)+
  scale_fill_manual(values=c("darkgray","cornflowerblue","cornflowerblue"))+xlab("Case")+
  ylab("Length(�m)")+theme_bw()+theme(legend.position="none")+
  scale_y_continuous(expand=c(0,0), limits=c(0,10))+
  theme(text = element_text(size=12))+
  guides(fill=guide_legend(title="Group"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=12))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)
dev.off()

#statistical tests 
smi_pos_test=c(smi_pos,na.rm=TRUE)
smi_neg_test=c(smi_neg,na.rm=TRUE)
wilcox.test(smi_pos_test,smi_neg_test)
wilcox.test(smi_neg_test,controls)
wilcox.test(smi_pos_test,controls)
#means

mean_smipos=c(mean(positives1),mean(positives2),mean(positives3),mean(positives4),mean(positives5),mean(positives6),mean(positives7),
           mean(positives8),mean(positives9),mean(positives10),mean(positives11),mean(positives12),mean(positives13),
           mean(positives14),mean(positives15),mean(positives16),mean(positives17),
           mean(positives18),mean(positives19),mean(positives20),mean(positives21),
           mean(positives22),mean(positives23),mean(positives24),mean(positives25),
           mean(positives26),mean(positives27),
           mean(positives28), mean(positives29),mean(positives30),mean(positives31),mean(positives32),
           mean(positives33),mean(positives34))
sd_smipos=c(sd(positives1),sd(positives2),sd(positives3),sd(positives4),sd(positives5),sd(positives6),
              sd(positives7),sd(positives8),sd(positives9),sd(positives10),sd(positives11),sd(positives12),sd(positives13),
              sd(positives14),sd(positives15),sd(positives16),sd(positives17),
              sd(positives18),sd(positives19),sd(positives20),sd(positives21),
              sd(positives22),sd(positives23),sd(positives24),sd(positives25),
              sd(positives26),sd(positives27),sd(positives28), sd(positives29),sd(positives30),
             sd(positives31),sd(positives32),
             sd(positives33),sd(positives34))
se_smipos=sd_smipos/sqrt(50)


median_smipos=c(median(positives1),median(positives2),median(positives3),median(positives4),median(positives5),median(positives6),median(positives7),
                median(positives8),median(positives9),median(positives10),median(positives11),median(positives12),median(positives13),median(positives14),
                median(positives15),median(positives16),median(positives17),median(positives18),median(positives19),median(positives20),median(positives21),
                median(positives22),mean(positives23),median(positives24),median(positives25),median(positives26),median(positives27),
                median(positives28), median(positives29),median(positives30),median(positives31),median(positives32),median(positives33),median(positives34))
                                                                                      

hla=c(2.99,2.11,3.52,4.04,3.71,2.73,1.59,2.83,3.75,3.08,3.411,3.67,4.69,5.35,
      2.46,1.20,2.69,3.38,2.36,3.55,2.81,2.6,3.5,3.63,3.53,3.13,2.07,2.87,2.82,3.33,3.96,2.73,2.45,2.14)

hla_sd= c(0.36,1.04,1.38,1.14,1.02,1.64,0.3,0.55,0.66,1.98,1.120,1.55,1.38,2.4,0.5,0.86,1.3,1.32,0.59,
          1.47,0.93,0.49,0.8,1.28,2.69,1.36,1.57,0.65,1.22, 1.53,1.46,1.029,1.25,0.66)
hla_se= hla_sd/sqrt(10)
age=c(43,43,50,50,38,38,49,63,63,65,65,55,55,42,42,65,65,50,58,61,58,45,45,
      76,48,48,62,62,63,63,42,42,53,53)

duration=c(21,21,29,29,22,22,20,39,39,40,40,34,34 ,12,12,36,36,29,NaN,29,20,23,23,35,25,25,
           42,42,32,32,21,21,27,27)


plot(age, mean_smipos)
plot(duration, mean_smipos)
plot(hla, mean_smipos)
plot(age,median_smipos, ylim=c(0,6))
cor.test(duration,mean_smipos, method="spearman")
cor.test(age,mean_smipos,method="spearman")
cor.test(hla,mean_smipos,method="spearman")
fit=lm(mean_smipos~duration)
summary(fit)$r.squared

hla_datastructure=data.frame("hla"=hla, "smi_pos"=mean_smipos, "smipos_se"=se_smipos,
                             "hla_sd"=hla_sd, "hla_se"=hla_se, "duration"=duration)
fit=lm(smi_pos~hla)
summary(fit)$r.squared
coef(lm(hla ~ smi_pos, data = hla_datastructure))
coef(lm(smi_pos ~ duration, data = hla_datastructure))

cols=c("SE Microglia"="blue","SE Paranodal Length"="red")
jpeg("hla-smipos.jpg",width = 9, height = 6, units = 'in', res=300)
q <- ggplot(hla_datastructure, aes(x=hla, y=smi_pos)) + 
  geom_point()+geom_errorbar(aes(ymin=smi_pos-smipos_se, ymax=smi_pos+smipos_se,colour="SE Paranodal Length"))+
  geom_errorbarh(aes(xmin=hla-hla_se, xmax=hla+hla_se,colour="SE Microglia"))+
   theme_bw()+scale_x_continuous(limits=c(0.5,4.5))+scale_y_continuous(limits=c(2,5))+
   xlab("Mean area % occupied by microglia")+
    ylab("Mean paranodal length SMI32+")+
  ggtitle("Microglia inflammation versus Paranodal length")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(q)
dev.off()


cols1=c("SE Parnaodal Length")
jpeg("duration-smipos.jpg",width = 9, height = 6, units = 'in', res=300)
n <- ggplot(hla_datastructure, aes(x=duration, y=smi_pos)) + 
  geom_point()+
  geom_errorbar(aes(ymin=smi_pos-smipos_se, ymax=smi_pos+smipos_se,color="SE Parnaodal Length"))+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Disease Duration (yrs)")+
  ylab("Mean paranodal length SMI32+")+
  ggtitle(" Disease Duration versus Paranodal length")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(n)
dev.off()

jpeg("age-smipos.jpg",width = 9, height = 6, units = 'in', res=300)
n <- ggplot(hla_datastructure, aes(x=age, y=smi_pos)) + 
  geom_point()+
  geom_errorbar(aes(ymin=smi_pos-smipos_se, ymax=smi_pos+smipos_se,color="SE Parnaodal Length"))+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Age (yrs)")+
  ylab("Mean paranodal length SMI32+")+
  ggtitle(" Disease Duration versus Paranodal length")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(n)
dev.off()




plot(hla, mean_smipos, ylim=c(2,4))
cor.test(hla,mean_smipos,method="spearman")
a=lm(mean_smipos ~ hla)
cor.test(mean_smipos,duration,method="spearman")
p=lm(mean_smipos ~ duration)
cor.test(mean_smipos,age,method="spearman")
o=lm(mean_smipos ~ age)
fit=lm(mean_smipos~hla)
summary(fit)$r.squared



data=data.frame(c48p,c54c,c54p,c72c,c72p,c74c,c74p,c75c,c75p,c76c,c76p,pdc29c,pdc29p,pdc39c,
                pdc40c,pdc40p,positives1,positives2, positives3,positives4,positives5,positives6,positives7,positives8,positives9,positives10,
                positives11,positives12,positives13,positives14,positives15,positives16,positives17,positives18,positives19,
                positives20,positives21,positives22,positives23,positives24,positives25,positives26,positives27,positives28,
                positives29, positives30,positives31,positives32,positives33,positives34)

tot2=c(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6],data[,7],data[,8],data[,9],
       data[,10],data[,11],data[,12],data[,13],data[,14],data[,15],data[,16],data[,17],data[,18],
       data[,19],data[,20],data[,21],data[,22],data[,23],data[,24],data[,25],
       data[,26],data[,27],data[,28],data[,29],data[,30],data[,31],data[,32],data[,33],data[,34],
       data[,35],data[,36],data[,37],data[,38],data[,39],data[,40],data[,41],data[,42],data[,43],
       data[,44],data[,45],data[,46],data[,47],data[,48],data[,49],data[,50])



type2=c(rep("C48_1",nrow(data)),rep("C54_1",nrow(data)),rep("C54_2",nrow(data)),
        rep("C72_1",nrow(data)),rep("C72_2",nrow(data)),rep("C74_1",nrow(data)),
        rep("C74_2",nrow(data)),rep("C75_1",nrow(data)),rep("C75_2",nrow(data)),rep("C76_1",nrow(data)),
        rep("C76_2",nrow(data)),rep("pdc29_1",nrow(data)),rep("pdc29_2",nrow(data)),
        rep("pdc39_2",nrow(data)), rep("pdc40_1",nrow(data)),rep("pdc40_2",nrow(data)),
        rep("ms404_1",nrow(data)),
        rep("ms404_2",nrow(data)),rep("ms406_1",nrow(data)),rep("ms406_2",nrow(data)),
        rep("ms411_2",nrow(data)),
        rep("ms422_2",nrow(data)),rep("ms444_1",nrow(data)),rep("ms461_1",nrow(data)),
        rep("ms461_2",nrow(data)),rep("ms466_1",nrow(data)),rep("ms466_2",nrow(data)),
        rep("ms478_1",nrow(data)),
        rep("ms478_2",nrow(data)),rep("ms500_1",nrow(data)),rep("ms500_2",nrow(data)),
        rep("ms510_1",nrow(data)),
        rep("ms510_2",nrow(data)),rep("ms517_1",nrow(data)),
        rep("ms517_2",nrow(data)),rep(" ms523_1",nrow(data)),rep("ms523_2",nrow(data)),
        rep("ms530_1",nrow(data)),rep("ms530_2",nrow(data)),rep("ms549_2",nrow(data)),
        rep("ms567_1",nrow(data)),rep("ms567_2",nrow(data)),rep("ms584_1",nrow(data)),
        rep("ms584_2",nrow(data)),
        rep("ms535_1",nrow(data)),rep("ms535_2",nrow(data)),rep("ms542_1",nrow(data)),
        rep("ms585_1",nrow(data)),rep("ms585_2",nrow(data)),
        rep("ms587_1",nrow(data)))




data2=data.frame("Class"=type2,"Length"=tot2)
data2 = data2[!is.na(data2$Length),]
data2$Class<- factor(data2$Class,as.character(data2$Class))
# hacer el mean orand median de cada columna del data frame para hacer la correlacion con SMI32 y con HLA. 


jpeg("paranodal_plots_smipos.jpg",width=12, height=9,units = 'in', res=300)
p <- ggplot(data2, aes(x=Class, y=Length)) + 
  geom_boxplot(aes(fill =Class,alpha=0.5))+
  coord_flip()+
  scale_fill_manual(values=c("red","red","red","red","red","red","red","red","red","red",
                             "red","red","red","red","red","red",
                             "blue","blue","blue","blue","blue","blue","blue","blue","blue","blue",
                             "blue","blue","blue","blue","blue","blue","blue","blue","blue","blue",
                             "blue","blue","blue","blue","blue","blue","blue","blue","blue","blue",
                             "blue","blue","blue","blue"))+
  
  ylab("Length(�m)")+theme_bw()+
  xlab("Case")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  ggtitle("Paranodal Length Distributions")+theme(axis.text=element_text(size=10),
                                                  axis.title=element_text(size=18))+
  theme(plot.title = element_text(lineheight=3, color="black", size=20))

print(p)
dev.off()

           


