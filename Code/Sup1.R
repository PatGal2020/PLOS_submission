rm(list =ls())
gc()
overall.wd="C:/Users/pg2015/Desktop/Measurements/caspr-measurements"
setwd(overall.wd) 
max_length=200
library(ggplot2)
library(stats)
library(graphics)
library(plotly)
library(utf8)
library(rjson)
Sys.setenv("plotly_domain" = "http://mydomain.com")
Sys.setenv("plotly_username" = "pg1993")
Sys.setenv("plotly_api_key" = "Ju6BrozodGL66ALVLcFj")




#CONTROLS (16)

C48_1<-read.csv("c48_1.csv",header=TRUE)
C48_1 = c(C48_1[,9], rep(NA,1000))
C48_1= C48_1 [1:max_length]
overlap_50_48_1=0.06625404

C54_1<-read.csv("c54_1.csv",header=TRUE)
C54_1 = c(C54_1[,9], rep(NA,1000))
C54_1 = C54_1 [1:max_length]
overlap_50_54_1=0.06842158
C54_2<-read.csv("c54_2.csv",header=TRUE)
C54_2 = c(C54_2[,9], rep(NA,1000))
C54_2 = C54_2 [1:max_length]
overlap_50_54_2=0.06639053

C72_1<-read.csv("c72_1.csv",header=TRUE)
C72_1 = c(C72_1[,9], rep(NA,1000))
C72_1 = C72_1 [1:max_length]
mean(C72_1,na.rm=TRUE)
overlap_50_72_1=0.07525349

C72_2<-read.csv("c72_2.csv",header=TRUE)
C72_2 = c(C72_2[,9], rep(NA,1000))
C72_2 = C72_2 [1:max_length]
mean(C72_2,rm.na=TRUE)
overlap_50_72_2=0.07031387


C74_1<-read.csv("c74_1.csv",header=TRUE)
C74_1 = c(C74_1[,9], rep(NA,1000))
C74_1 = C74_1 [1:max_length]
mean(C74_1,rm.na=TRUE)
overlap_50_74_1=0.0781291
  
C74_2<-read.csv("c74_2.csv",header=TRUE)
C74_2 = c(C74_2[,9], rep(NA,1000))
C74_2 = C74_2 [1:max_length]
mean(C74_2,rm.na=TRUE)
overlap_50_74_2=0.07636232

C75_1<-read.csv("c75_1.csv",header=TRUE)
C75_1 = c(C75_1[,9], rep(NA,1000))
C75_1 = C75_1 [1:max_length]
mean(C75_1,rm.na=TRUE)
overlap_50_75_1=0.07832454
  
C75_2<-read.csv("c75_2.csv",header=TRUE)
C75_2 = c(C75_2[,9], rep(NA,1000))
C75_2 = C75_2 [1:max_length]
mean(C75_2,rm.na=TRUE)
overlap_50_75_2= 0.07774864


C76_1<-read.csv("c76_1.csv",header=TRUE)
C76_1 = c(C76_1[,9], rep(NA,1000))
C76_1 = C76_1 [1:max_length]
mean(C76_1,rm.na=TRUE)
overlap_50_76_1=  0.08228461

C76_2<-read.csv("c76_2.csv",header=TRUE)
C76_2 = c(C76_2[,9], rep(NA,1000))
C76_2 = C76_2 [1:max_length]
mean(C76_2,rm.na=TRUE)
overlap_50_76_2= 0.07838793


pdc29_1<-read.csv("pdc29_1.csv",header=TRUE)
pdc29_1 = c(pdc29_1[,9], rep(NA,1000))
pdc29_1 = pdc29_1 [1:max_length]
mean(pdc29_1,rm.na=TRUE)
overlap_50_29_1=0.03084112
pdc29_2<-read.csv("pdc29_2.csv",header=TRUE)
pdc29_2 = c(pdc29_2[,9], rep(NA,1000))
pdc29_2 = pdc29_2 [1:max_length]
overlap_50_29_2=0.02880658

pdc40_1<-read.csv("pdc40_1.csv",header=TRUE)
pdc40_1 = c(pdc40_1[,9], rep(NA,1000))
pdc40_1 = pdc40_1 [1:max_length]
overlap_50_40_1=0.06101399
pdc40_2<-read.csv("pdc40_2.csv",header=TRUE)
pdc40_2 = c(pdc40_2[,9], rep(NA,1000))
pdc40_2 = pdc40_2 [1:max_length]
overlap_50_40_2=0.06890612


pdc39_2<-read.csv("pdc39_2.csv",header=TRUE)
pdc39_2 = c(pdc39_2[,9], rep(NA,1000))
pdc39_2 = pdc39_2 [1:max_length]
overlap_50_39_2=0.06174264


overlap_50_controls=c(overlap_50_48_1, overlap_50_54_1, overlap_50_54_2, overlap_50_72_1, 
                      overlap_50_72_2,overlap_50_74_1, overlap_50_74_2, overlap_50_75_1, overlap_50_75_2,
                      overlap_50_76_2, overlap_50_76_1, overlap_50_29_1, overlap_50_29_2, overlap_50_40_1, overlap_50_40_2, 
                      overlap_50_39_2)

#patients (34) 

ms404_1<-read.csv("ms404_1.csv",header=TRUE)
ms404_1 = c(ms404_1[,9], rep(NA,1000))
ms404_1 = ms404_1 [1:max_length]
paranodes_mean13<-mean(ms404_1)
paranodes_median13<-median(ms404_1,na.rm=TRUE)
paranodes_sd13<-sd(ms404_1,na.rm = TRUE)
proportion_more4_13<-(33/252)*100
overlap_100_13=0.26
overlap_50_13=0.12
proportion_more5_13<-(19/252)*100
proportion_more2_13<-(177/252)*100


ms404_2<-read.csv("ms404_2.csv",header=TRUE)
ms404_2 = c(ms404_2[,9], rep(NA,1000))
ms404_2 = ms404_2 [1:max_length]
paranodes_mean12<-mean(ms404_2)
paranodes_sd12<-sd(ms404_2)
paranodes_median12<-median(ms404_2,na.rm=TRUE)
proportion_more4_12<-(37/251)*100
overlap_100_12=0.24
overlap_50_12=0.098
proportion_more5_12<-(6/251)*100
proportion_more2_12<-(175/251)*100


ms406_1<-read.csv("ms406_1.csv",header=TRUE)
ms406_1 = c(ms406_1[,9], rep(NA,1000))
ms406_1 = ms406_1 [1:max_length]
paranodes_mean27<-mean(ms406_1)
paranodes_sd27<-sd(ms406_1)
paranodes_median27<-median(ms406_1)
proportion_more4_27<-(10/249)*100
overlap_100_27=0.2
overlap_50_27=0.078
proportion_more5_27<-(1/249)*100
proportion_more2_27<-(154/249)*100


ms406_2<-read.csv("ms406_2.csv",header=TRUE)
ms406_2 = c(ms406_2[,9], rep(NA,1000))
ms406_2 = ms406_2 [1:max_length]
paranodes_mean28<-mean(ms406_2,na.rm=TRUE)
paranodes_sd28<-sd(ms406_2,na.rm=TRUE)
paranodes_median28<-median(ms406_2,na.rm=TRUE)
proportion_more4_28<-(11/195)*100
overlap_100_28=0.49
overlap_50_28=0.18
proportion_more5_28<-(5/195)*100
proportion_more2_28<-(149/195)*100


ms411_2<-read.csv("ms411_2.csv",header=TRUE)
ms411_2 = c(ms411_2[,9], rep(NA,1000))
ms411_2 = ms411_2 [1:max_length]
paranodes_mean20<-mean(ms411_2)
paranodes_sd20<-sd(ms411_2)
paranodes_median20<-median(ms411_2)
proportion_more4_20<-(12/250)*100
overlap_100_20=0.39
overlap_50_20=0.124
proportion_more5_20<-(2/250)*100
proportion_more2_20<-(151/250)*100




ms422_2<-read.csv("ms422_2.csv",header=TRUE)
ms422_2 = c(ms422_2[,9], rep(NA,1000))
ms422_2 = ms422_2 [1:max_length]
paranodes_mean19<-mean(ms422_2)
paranodes_sd19<-sd(ms422_2)
paranodes_median19<-median(ms422_2)
proportion_more4_19<-(13/250)*100
overlap_100_19=0.28
overlap_50_19=0.126
proportion_more5_19<-(1/250)*100
proportion_more2_19<-(143/250)*100




ms444_1<-read.csv("ms444_1.csv",header=TRUE)
ms444_1 = c(ms444_1[,9], rep(NA,1000))
ms444_1 = ms444_1 [1:max_length]
paranodes_mean7<-mean(ms444_1)
paranodes_sd7<-sd(ms444_1)
paranodes_median7<-median(ms444_1)
proportion_more4_7<-(12/300)*100
overlap_100_7=0.274
overlap_50_7=0.09
proportion_more5_7<-(3/300)*100
proportion_more2_7<-(342/300)*100






ms461_1<-read.csv("ms461_1.csv",header=TRUE)
ms461_1 = c(ms461_1[,9], rep(NA,1000))
ms461_1 = ms461_1 [1:max_length]
paranodes_mean1<-mean(ms461_1)
paranodes_sd1<-sd(ms461_1)
paranodes_median1<-median(ms461_1)
proportion_more4_1<-(26/301)*100
overlap_100_1=0.28
overlap_50_1=0.12
proportion_more5_1<-(9/301)*100
proportion_more2_1<-(180/301)*100



ms461_2<-read.csv("ms461_2.csv",header=TRUE)
ms461_2 = c(ms461_2[,9], rep(NA,1000))
ms461_2 = ms461_2 [1:max_length]
paranodes_mean2<-mean(ms461_2)
paranodes_sd2<-sd(ms461_2)
paranodes_median2<-median(ms461_2)
proportion_more4_2<-(8/300)*100
overlap_100_2=0.31
overlap_50_2=0.12
proportion_more5_2<-(1/300)*100
proportion_more2_2<-(165/300)*100



ms466_1<-read.csv("ms466_1.csv",header=TRUE)
ms466_1 = c(ms466_1[,9], rep(NA,1000))
ms466_1 = ms466_1 [1:max_length]
paranodes_mean16<-mean(ms466_1)
paranodes_sd16<-sd(ms466_1)
paranodes_median16<-median(ms466_1)
proportion_more4_16<-(4/250)*100
overlap_100_16=0.2
overlap_50_16=0.077
proportion_more5_16<-(0/250)*100
proportion_more2_16<-(114/250)*100






ms466_2<-read.csv("ms466_2.csv",header=TRUE)
ms466_2 = c(ms466_2[,9], rep(NA,1000))
ms466_2 = ms466_2 [1:max_length]
paranodes_mean17<-mean(ms466_2)
paranodes_sd17<-sd(ms466_2)
paranodes_median17<-median(ms466_2)
proportion_more4_17<-(12/250)*100
overlap_100_17=0.44
overlap_50_17=0.19
proportion_more5_17<-(3/250)*100
proportion_more2_17<-(143/250)*100




ms478_1<-read.csv("ms478_1.csv",header=TRUE)
ms478_1 = c(ms478_1[,9], rep(NA,1000))
ms478_1 = ms478_1 [1:max_length]
paranodes_mean9<-mean(ms478_1)
paranodes_sd9<-sd(ms478_1)
paranodes_median9<-median(ms478_1)
proportion_more4_9<-(30/300)*100
overlap_100_9=0.25
overlap_50_9=0.1
proportion_more5_9<-(12/300)*100
proportion_more2_9<-(148/300)*100



ms478_2<-read.csv("ms478_2.csv",header=TRUE)
ms478_2 = c(ms478_2[,9], rep(NA,1000))
ms478_2 = ms478_2 [1:max_length]
paranodes_mean8<-mean(ms478_2)
paranodes_sd8<-sd(ms478_2)
paranodes_median8<-median(ms478_2)
proportion_more4_8<-(2/301)*100
overlap_100_8=0.15
overlap_50_8=0.1
proportion_more5_8<-(0/301)*100
proportion_more2_8<-(142/301)*100




ms500_1<-read.csv("ms500_1.csv",header=TRUE)
ms500_1 = c(ms500_1[,9], rep(NA,1000))
ms500_1 = ms500_1 [1:max_length]
paranodes_mean3<-mean(ms500_1)
paranodes_sd3<-sd(ms500_1)
paranodes_median3<-median(ms500_1)
proportion_more4_3<-(31/254)*100
overlap_100_3=0.38
overlap_50_3=0.16
proportion_more5_3<-(13/254)*100
proportion_more2_3<-(175/254)*100




ms500_2<-read.csv("ms500_2.csv",header=TRUE)
ms500_2 = c(ms500_2[,9], rep(NA,1000))
ms500_2 = ms500_2 [1:max_length]
paranodes_mean4<-mean(ms500_2)
paranodes_sd4<-sd(ms500_2)
paranodes_median4<-median(ms500_2)
proportion_more4_4<-(56/250)*100
overlap_100_4=0.27
overlap_50_4=0.14
proportion_more5_4<-(28/250)*100
proportion_more2_4<-(167/250)*100




ms510_1<-read.csv("ms510_1.csv",header=TRUE)
ms510_1 = c(ms510_1[,9], rep(NA,1000))
ms510_1 = ms510_1 [1:max_length]
paranodes_mean5<-mean(ms510_1)
paranodes_sd5<-sd(ms510_1)
paranodes_median5<-median(ms510_1)
proportion_more4_5<-(24/250)*100
overlap_100_5=0.27
overlap_50_5=0.13
proportion_more5_5<-(6/250)*100
proportion_more2_5<-(188/250)*100




ms510_2<-read.csv("ms510_2.csv",header=TRUE,sep=";")
ms510_2 = c(ms510_2[,9], rep(NA,1000))
ms510_2 = ms510_2 [1:max_length]
paranodes_mean6<-mean(ms510_2)
paranodes_sd6<-sd(ms510_2)
paranodes_median6<-median(ms510_2)
proportion_more4_6<-(8/301)*100
overlap_100_6=0.24
overlap_50_6=0.11
proportion_more5_6<-(1/301)*100
proportion_more2_6<-(119/301)*100



ms517_1<-read.csv("ms517_1.csv",header=TRUE)
ms517_1 = c(ms517_1[,9], rep(NA,1000))
ms517_1 = ms517_1 [1:max_length]
paranodes_mean26<-mean(ms517_1)
paranodes_sd26<-sd(ms517_1)
paranodes_median26<-median(ms517_1) 
proportion_more4_26<-(11/250*100)
overlap_100_26=0.27
overlap_50_26=0.09
proportion_more5_26<-(5/250*100)
proportion_more2_26<-(150/250*100)






ms517_2<-read.csv("ms517_2.csv",header=TRUE)
ms517_2 = c(ms517_2[,9], rep(NA,1000))
ms517_2 = ms517_2 [1:max_length]
paranodes_mean25<-mean(ms517_2)
paranodes_sd25<-sd(ms517_2)
paranodes_median25<-median(ms517_2) 
proportion_more4_25<-(8/250)*100
overlap_100_25=0.23
overlap_50_25=0.1
proportion_more5_25<-(2/250)*100
proportion_more2_25<-(148/250)*100




ms523_1<-read.csv("ms523_1.csv",header=TRUE)
ms523_1 = c(ms523_1[,9], rep(NA,1000))
ms523_1 = ms523_1 [1:max_length]
paranodes_mean30<-mean(ms523_1)
paranodes_sd30<-sd(ms523_1)
paranodes_median30<-median(ms523_1) 
proportion_more4_30<-(35/286)*100
overlap_100_30=0.16
overlap_50_30=0.09
proportion_more5_30<-(7/286)*100
proportion_more2_30<-(167/286)*100




  
ms523_2<-read.csv("ms523_2.csv",header=TRUE)
ms523_2 = c(ms523_2[,9], rep(NA,1000))
ms523_2 = ms523_2 [1:max_length]
paranodes_mean29<-mean(ms523_2)
paranodes_sd29<-sd(ms523_2)
paranodes_median29<-median(ms523_2)
proportion_more4_29<-(25/303)*100
overlap_100_29=0.3
overlap_50_29=0.13
proportion_more5_29<-(13/303)*100
proportion_more2_29<-(157/303)*100



ms530_1<-read.csv("ms530_1.csv",header=TRUE)
ms530_1 = c(ms530_1[,9], rep(NA,1000))
ms530_1 = ms530_1 [1:max_length]
paranodes_mean32<-mean(ms530_1)
paranodes_sd32<-sd(ms530_1)
paranodes_median32<-median(ms530_1)
proportion_more4_32<-(6/233)*100
overlap_100_32=0.51
overlap_50_32=0.17
proportion_more5_32<-(1/233)*100
proportion_more2_32<-(139/233)*100






ms530_2<-read.csv("ms530_2.csv",header=TRUE)
ms530_2 = c(ms530_2[,9], rep(NA,1000))
ms530_2 = ms530_2 [1:max_length]
paranodes_mean31<-mean(ms530_2)
paranodes_sd31<-sd(ms530_2)
paranodes_median31<-median(ms530_2)
proportion_more4_31<-(69/250)*100
overlap_100_31=0.59
overlap_50_31=0.26
proportion_more5_31<-(30/250)*100
proportion_more2_31<-(187/250)*100





ms535_1<-read.csv("ms535_1.csv",header=TRUE)
ms535_1 = c(ms535_1[,9], rep(NA,1000))
ms535_1 = ms535_1 [1:max_length]
paranodes_mean11<-mean(ms535_1)
paranodes_sd11<-sd(ms535_1)
paranodes_median11<-median(ms535_1)
proportion_more4_11<-(36/250)*100
overlap_100_11=0.49
overlap_50_11=0.21
proportion_more5_11<-(22/250)*100
proportion_more2_11<-(164/250)*100





ms535_2<-read.csv("ms535_2.csv",header=TRUE)
ms535_2 = c(ms535_2[,9], rep(NA,1000))
ms535_2 = ms535_2 [1:max_length]
paranodes_mean10<-mean(ms535_2)
paranodes_sd10<-sd(ms535_2)
paranodes_median10<-median(ms535_2)
proportion_more4_10<-(65/250)*100
overlap_100_10=0.63
overlap_50_10=.24
proportion_more5_10<-(35/250)*100
proportion_more2_10<-(178/250)*100






ms542_1<-read.csv("ms542_1.csv",header=TRUE)
ms542_1 = c(ms542_1[,9], rep(NA,1000))
ms542_1 = ms542_1 [1:max_length]
paranodes_mean24<-mean(ms542_1)
paranodes_sd24<-sd(ms542_1)
paranodes_median24<-median(ms542_1)
proportion_more4_24<-(11/250)*100
overlap_100_24=0.31
overlap_50_24=0.1
proportion_more5_24<-(1/250)*100
proportion_more2_24<-(164/250)*100





ms549_2<-read.csv("ms549_2.csv",header=TRUE)
ms549_2 = c(ms549_2[,9], rep(NA,1000))
ms549_2 = ms549_2 [1:max_length]
paranodes_mean18<-mean(ms549_2)
paranodes_sd18<-sd(ms549_2)
paranodes_median18<-median(ms549_2)
proportion_more4_18<-(12/250)*100
overlap_100_18=0.19
overlap_50_18=0.08
proportion_more5_18<-(5/250)*100
proportion_more2_18<-(145/250)*100





ms567_1<-read.csv("ms567_1.csv",header=TRUE)
ms567_1 = c(ms567_1[,9], rep(NA,1000))
ms567_1 = ms567_1 [1:max_length]
paranodes_mean22<-mean(ms567_1)
paranodes_sd22<-sd(ms567_1)
paranodes_median22<-median(ms567_1)
proportion_more4_22<-(10/250)*100
overlap_100_22=0.22
overlap_50_22=0.09
proportion_more5_22<-(2/250)*100
proportion_more2_22<-(161/250)*100







ms567_2<-read.csv("ms567_2.csv",header=TRUE)
ms567_2 = c(ms567_2[,9], rep(NA,1000))
ms567_2 = ms567_2 [1:max_length]
paranodes_mean23<-mean(ms567_2)
paranodes_sd23<-sd(ms567_2)
paranodes_median23<-median(ms567_2)
proportion_more4_23<-(7/250)*100
overlap_100_23=0.2
overlap_50_23=0.08
proportion_more5_23<-(2/250)*100
proportion_more2_23<-(152/250)*100




ms584_1<-read.csv("ms584_1.csv",header=TRUE)
ms584_1 = c(ms584_1[,9], rep(NA,1000))
ms584_1 = ms584_1 [1:max_length]
paranodes_mean14<-mean(ms584_1)
paranodes_sd14<-sd(ms584_1)
paranodes_median14<-median(ms584_1)
proportion_more4_14<-(35/250)*100
overlap_100_14=0.36
overlap_50_14=0.17
proportion_more5_14<-(14/250)*100
proportion_more2_14<-(174/250)*100






ms584_2<-read.csv("ms584_2.csv",header=TRUE)
ms584_2 = c(ms584_2[,9], rep(NA,1000))
ms584_2 = ms584_2 [1:max_length]
paranodes_mean15<-mean(ms584_2,na.rm=TRUE)
paranodes_sd15<-sd(ms584_2,na.rm=TRUE)
paranodes_median15<-median(ms584_2,na.rm=TRUE)
proportion_more4_15<-(40/161)*100
overlap_100_15=0.74
overlap_50_15=0.36
proportion_more5_15<-(16/161)*100
proportion_more2_15<-(140/161)*100





ms585_1<-read.csv("ms585_1.csv",header=TRUE)
ms585_1 = c(ms585_1[,9], rep(NA,1000))
ms585_1 = ms585_1 [1:max_length]
paranodes_mean34<-mean(ms585_1)
paranodes_sd34<-sd(ms585_1)
paranodes_median34<-median(ms585_1)
proportion_more4_34<-(35/232)*100
overlap_100_34=0.35
overlap_50_34=0.15
proportion_more5_34<-(8/232)*100
proportion_more2_34<-(189/232)*100






ms585_2<-read.csv("ms585_2.csv",header=TRUE)
ms585_2 = c(ms585_2[,9], rep(NA,1000))
ms585_2 = ms585_2 [1:max_length]
paranodes_mean33<-mean(ms585_2)
paranodes_sd33<-sd(ms585_2)
paranodes_median33<-median(ms585_2)
proportion_more4_33<-(13/250)*100
overlap_100_33=0.36
overlap_50_33=0.13
proportion_more5_33<-(2/250)*100
proportion_more2_33<-(158/250)*100






ms587_1<-read.csv("ms587_1.csv",header=TRUE)
ms587_1 = c(ms587_1[,9], rep(NA,1000))
ms587_1 = ms587_1 [1:max_length]
paranodes_mean21<-mean(ms587_1)
paranodes_sd21<-sd(ms587_1)
paranodes_median21<-median(ms587_1)
proportion_more4_21<-(4/301)*100
overlap_100_21=0.4
overlap_50_21=0.12
proportion_more5_21<-(1/301)*100
proportion_more2_21<-(117/301)*100




#correlating paranodal length with clinical data
paranodal_mean=c(paranodes_mean1,paranodes_mean2,paranodes_mean3,paranodes_mean4,paranodes_mean5,paranodes_mean6,
                 paranodes_mean7,paranodes_mean8,paranodes_mean9,paranodes_mean10,paranodes_mean11,paranodes_mean12,
                 paranodes_mean13,paranodes_mean14,paranodes_mean15,paranodes_mean16,paranodes_mean17,paranodes_mean18,
                 paranodes_mean19,paranodes_mean20,paranodes_mean21,paranodes_mean22,paranodes_mean23,paranodes_mean24,
                 paranodes_mean25,paranodes_mean26,paranodes_mean27,paranodes_mean28,paranodes_mean29,paranodes_mean30,
                 paranodes_mean31,paranodes_mean32,paranodes_mean33,paranodes_mean34)
paranodal_median=c(paranodes_median1,paranodes_median2,paranodes_median3,paranodes_median4,
                   paranodes_median5,paranodes_median6,
                   paranodes_median7,paranodes_median8,paranodes_median9,paranodes_median10,
                   paranodes_median11,paranodes_median12,
                   paranodes_median13,paranodes_median14,paranodes_median15,
                   paranodes_median16,paranodes_median17,paranodes_median18,
                   paranodes_median19,paranodes_median20,paranodes_median21,
                   paranodes_median22,paranodes_median23,paranodes_median24,
                   paranodes_median25,paranodes_median26,paranodes_median27,
                   paranodes_mean28,paranodes_mean29,paranodes_mean30,
                   paranodes_median31,paranodes_median32,paranodes_median33,paranodes_median34)

paranodal_sd=c(paranodes_sd1,paranodes_sd2,paranodes_sd3,paranodes_sd4,paranodes_sd5,paranodes_sd6,paranodes_sd7,
               paranodes_sd8,paranodes_sd9,paranodes_sd10,paranodes_sd11,paranodes_sd12,paranodes_sd13,
               paranodes_sd14,paranodes_sd15,paranodes_sd16,paranodes_sd17,paranodes_sd18,paranodes_sd19,
               paranodes_sd20,paranodes_sd21,paranodes_sd22,paranodes_sd23,paranodes_sd24,paranodes_sd25,
               paranodes_sd26,paranodes_sd27,paranodes_sd28,paranodes_sd29,paranodes_sd30,paranodes_sd31,
               paranodes_sd32,paranodes_sd33,paranodes_sd34)

paranodes_more4=c(proportion_more4_1,proportion_more4_2,proportion_more4_3,proportion_more4_4,proportion_more4_5,
                  proportion_more4_6,proportion_more4_7,proportion_more4_8,proportion_more4_9,
                  proportion_more4_10,proportion_more4_11,proportion_more4_12,proportion_more4_13,proportion_more4_14,
                  proportion_more4_15,proportion_more4_16,proportion_more4_17,proportion_more4_18,proportion_more4_19,
                  proportion_more4_20,proportion_more4_21,proportion_more4_22,proportion_more4_23,
                  proportion_more4_24,proportion_more4_25,proportion_more4_26,proportion_more4_27,proportion_more4_28,
                  proportion_more4_28,proportion_more4_30,proportion_more4_31,proportion_more4_32,proportion_more4_33,
                  proportion_more4_34
                  )

paranodes_more5=c(proportion_more5_1,proportion_more5_2,proportion_more5_3,proportion_more5_4,proportion_more5_5,
                  proportion_more5_6,proportion_more5_7,proportion_more5_8,proportion_more5_9,
                  proportion_more5_10,proportion_more5_11,proportion_more5_12,proportion_more5_13,proportion_more5_14,
                  proportion_more5_15,proportion_more5_16,proportion_more5_17,proportion_more5_18,proportion_more5_19,
                  proportion_more5_20,proportion_more5_21,proportion_more5_22,proportion_more5_23,
                  proportion_more5_24,proportion_more5_25,proportion_more5_26,proportion_more5_27,proportion_more5_28,
                  proportion_more5_28,proportion_more5_30,proportion_more5_31,proportion_more5_32,proportion_more5_33,
                  proportion_more5_34
)



paranodes_more2=c(proportion_more2_1,proportion_more2_2,proportion_more2_3,proportion_more2_4,proportion_more2_5,
                  proportion_more2_6,proportion_more2_7,proportion_more2_8,proportion_more2_9,
                  proportion_more2_10,proportion_more2_11,proportion_more2_12,proportion_more2_13,proportion_more2_14,
                  proportion_more2_15,proportion_more2_16,proportion_more2_17,proportion_more2_18,proportion_more2_19,
                  proportion_more2_20,proportion_more2_21,proportion_more2_22,proportion_more2_23,
                  proportion_more2_24,proportion_more2_25,proportion_more2_26,proportion_more2_27,proportion_more2_28,
                  proportion_more2_28,proportion_more2_30,proportion_more2_31,proportion_more2_32,proportion_more2_33,
                  proportion_more2_34
)



overlaps_100=c(overlap_100_1,overlap_100_2,overlap_100_3,overlap_100_4,overlap_100_5,overlap_100_6,overlap_100_7,
               overlap_100_8,overlap_100_9,overlap_100_10,overlap_100_11,overlap_100_12,overlap_100_13,overlap_100_14,
               overlap_100_15,overlap_100_16,overlap_100_17,overlap_100_18,overlap_100_19,overlap_100_20,overlap_100_21,
               overlap_100_22,overlap_100_23,overlap_100_24,overlap_100_25,overlap_100_26,overlap_100_27,overlap_100_28,
               overlap_100_29,overlap_100_30,overlap_100_31,overlap_100_32,overlap_100_33,overlap_100_34)
overlaps_50=c(overlap_50_1,overlap_50_2,overlap_50_3,overlap_50_4,overlap_50_5,overlap_50_6,overlap_50_7,
               overlap_50_8,overlap_50_9,overlap_50_10,overlap_50_11,overlap_50_12,overlap_50_13,overlap_50_14,
               overlap_50_15,overlap_50_16,overlap_50_17,overlap_50_18,overlap_50_19,overlap_50_20,overlap_50_21,
               overlap_50_22,overlap_50_23,overlap_50_24,overlap_50_25,overlap_50_26,overlap_50_27,overlap_50_28,
               overlap_50_29,overlap_50_30,overlap_50_31,overlap_50_32,overlap_50_33,overlap_50_34)


wilcox.test(overlaps_50, overlap_50_controls)
wilcox.test()

cor.test(hla,overlaps_50, method="spearman")
cor.test(hla,overlaps_100, method="spearman")
cor.test(duration, paranodes_more4, method="spearman")
cor.test(duration, paranodes_more5, method="spearman")
cor.test(age, paranodes_more4, method="spearman")
cor.test(age, paranodes_more5, method="spearman")
cor.test(hla, paranodes_more4, method="spearman")
cor.test(hla, paranodes_more5, method="spearman")
cor.test(overlaps_100, paranodes_more4, method="spearman")
cor.test(overlaps_100, paranodes_more5, method="spearman")
cor.test(overlaps_100,paranodal_mean , method="spearman")
cor.test(overlaps_50,paranodal_mean , method="spearman")
cor.test(overlaps_50, paranodes_more4, method="spearman")
cor.test(overlaps_50, paranodes_more5, method="spearman")
cor.test(paranodal_mean, overlaps_50, method="spearman")
cor.test(hla, paranodal_mean, method="spearman")
cor.test(duration, paranodes_more2, method="spearman")






paranodal_se=paranodal_sd/sqrt(200)

hla=c(2.99,2.11,3.52,4.04,3.71,2.73,1.59,2.83,3.75,3.08,3.411,3.67,4.69,5.35,
      2.46,1.20,2.69,3.38,2.36,3.55,2.81,2.6,3.5,3.63,3.53,3.13,2.07,2.87,2.82,3.33,3.96,2.73,2.45,2.14)

hla_sd= c(0.36,1.04,1.38,1.14,1.02,1.64,0.3,0.55,0.66,1.98,1.120,1.55,1.38,2.4,0.5,0.86,1.3,1.32,0.59,
          1.47,0.93,0.49,0.8,1.28,2.69,1.36,1.57,0.65,1.22, 1.53,1.46,1.029,1.25,0.66)
hla_se= hla_sd/sqrt(10)
age=c(43,43,50,50,38,38,49,63,63,65,65,55,55,42,42,65,65,50,58,61,58,45,45,
      76,48,48,62,62,63,63,42,42,53,53)

duration=c(21,21,29,29,22,22,20,39,39,40,40,34,34 ,12,12,36,36,29,NaN,29,20,23,23,35,25,25,
           42,42,32,32,21,21,27,27)


plot(duration, paranodes_more4)

hla_datastructure=data.frame("hla"=hla,"paranodal_mean"=paranodal_mean,"paranodal_se"=paranodal_se,
                             "hla_sd"=hla_sd, "hla_se"=hla_se, 
                             "duration"=duration, "paranodal median"=paranodal_median,"age"=age, "proportion5"=paranodes_more5, 
                             "proportion4"=paranodes_more4, "overlaps_100"=overlaps_100,"overlaps_50"=overlaps_50)



write.csv(hla_datastructure,file="hla.csv")







cols=c("SE Microglia"="darkgray","SE Paranodal Length"="cornflowerblue")
pdf("microglia-paranode-mean.pdf", width = 9, height = 6)
a <- ggplot(hla_datastructure, aes(x=hla, y=paranodal_mean)) + 
  geom_errorbarh(aes(xmin=hla-hla_se, xmax=hla+hla_se,colour="SE Microglia"))+
  geom_errorbar(aes(ymin=paranodal_mean-paranodal_se, ymax=paranodal_mean+paranodal_se,colour="SE Paranodal Length"))+
  theme_bw()+geom_smooth(method=lm,se=FALSE,colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Mean paranodal length (µm)")+
  ylab("Mean area  by microglia (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(a)
dev.off()



pdf("microglia-proportion5.pdf", width = 9, height = 6)
o <- ggplot(hla_datastructure, aes(x=hla, y=proportion5)) + 
  theme_bw()+geom_smooth(method="lm",se=FALSE,colour="black", size=1)+
  geom_errorbarh(aes(xmin=hla-hla_se, xmax=hla+hla_se,colour="SE Microglia"))+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab(" Mean area  by microglia (%)")+
  ylab("Proportion of paranodes > 5µm (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(o)
dev.off()



pdf("microglia-proportion4.pdf", width = 9, height = 6)
b <- ggplot(hla_datastructure, aes(x=hla, y=proportion4)) + 
  theme_bw()+geom_smooth(method="lm",se=FALSE,colour="black", size=1)+
  geom_errorbarh(aes(xmin=hla-hla_se, xmax=hla+hla_se,colour="SE Microglia"))+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab(" Mean area  by microglia (%)")+
  ylab("Proportion of paranodes > 4µm (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(b)
dev.off()



pdf("overlaps50-paranodemean.pdf", width = 9, height = 6)
t <- ggplot(hla_datastructure, aes(x=paranodal_mean, y=overlaps_50)) + 
  theme_bw()+geom_smooth(method=lm,se=FALSE,colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Paranodal mean")+
  ylab("Proportion of overlapping regions (Threshold=50)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(t)
dev.off()



pdf("overlaps50-proportion.pdf", width = 9, height = 6)
z <- ggplot(hla_datastructure, aes(x=proportion4, y=overlaps_50)) + 
  theme_bw()+geom_smooth(method=lm, se=FALSE, colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Proportion of paranodes > 4µm")+
  ylab("Mean area  by microglia (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(z)
dev.off()

pdf("overlaps100-paranodemean.pdf", width = 9, height = 6)
c <- ggplot(hla_datastructure, aes(x=paranodal_mean, y=overlaps_100)) + 
  theme_bw()+geom_smooth(method=lm,se=FALSE,colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Paranodal mean")+
  ylab("Proportion of overlapping regions (Threshold=50)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(c)
dev.off()



pdf("overlaps100-proportion.pdf", width = 9, height = 6)
d <- ggplot(hla_datastructure, aes(x=proportion4, y=overlaps_100)) + 
  theme_bw()+geom_smooth(method=lm, se=FALSE, colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Proportion of paranodes > 4µm")+
  ylab("Mean area  by microglia (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(d)
dev.off()












tiff("paranodal_case_plots.tiff", width = 9, height = 6, units = 'in', res=400)
p<-ggplot(dat,aes(x=Class,y=Length))+geom_point()+coord_flip()+
  geom_boxplot(aes(fill=Class), alpha=0.5,notch=TRUE)+
  scale_fill_manual(values=c("darkgray","cornflowerblue"))+
  ylab("Paranodal Length(µm)")+
  xlab("Case")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)
dev.off()





cols=c("SE Microglia"="darkgray","SE Paranodal Length"="cornflowerblue")
pdf("microglia-paranode-median.pdf", width = 9, height = 6)
v <- ggplot(hla_datastructure, aes(x=paranodal_median, y=hla)) + 
  geom_errorbarh(aes(xmin=paranodal_median-paranodal_se, xmax=paranodal_median+paranodal_se,colour="SE Paranodal Length"))+
  geom_errorbar(aes(ymin=hla-hla_se, ymax=hla+hla_se,colour="SE Microglia"))+
  theme_bw()+geom_smooth(method=lm,se=FALSE,colour="black", size=1)+
  geom_point(size=2)+
  scale_colour_manual(values=cols)+
  xlab("Median paranodal length (µm)")+
  ylab("Mean area  by microglia (%)")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=20))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=24))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(v)
dev.off()




dev.off()

jpeg("paranodemean-duration.jpg", width = 9, height = 6, units = 'in', res=300)
cols=c("Paranode SEM"="red")
o <- ggplot(hla_datastructure, aes(x=overlaps_50, y=hla)) + 
  geom_point()+
  geom_errorbar(aes(ymin=paranodal_mean-paranodal_se, ymax=paranodal_mean+paranodal_se,colour="Paranode SEM"))+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Disease Duration (yrs)")+
  ylab("Mean paranodal length")+
  geom_abline(slope=1, intercept=4)+
  ggtitle(" Disease Duration versus Paranodal length")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(o)
dev.off()


jpeg("paranodemedian-duration.jpg", width = 9, height = 6, units = 'in', res=300)
r <- ggplot(hla_datastructure, aes(x=duration, y=paranodal_median)) + 
  geom_point()+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Disease Duration (yrs)")+
  ylab("Median paranodal length")+
  ggtitle(" Disease Duration versus Paranodal length")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(r)
dev.off()

jpeg("paranodemedian-duration.jpg", width = 9, height = 6, units = 'in', res=300)
k <- ggplot(hla_datastructure, aes(x=overlaps_50, y=paranodal_mean)) + 
  geom_point()+
  theme_bw()+scale_y_continuous()+
  xlab("Proportion of overlapping (%)")+
  ylab("Mean paranodal length")+
  ggtitle(" Proportion of overlapping versus paranodal length(um)")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))+geom_smooth(method=lm,se=FALSE,colour="black")
print(k)
dev.off()
b <- ggplot(hla_datastructure, aes(x=overlaps_100, y=paranodal_mean)) + 
  geom_point()+
  theme_bw()+scale_y_continuous()+
  xlab("Proportion of overlapping (%)")+
  ylab("Mean paranodal length")+
  ggtitle(" Proportion of overlapping versus paranodal length(um)")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))+geom_smooth(method=lm,se=FALSE,colour="black")
print(b)

h <- ggplot(hla_datastructure, aes(x=overlaps_50, y=proportion)) + 
  geom_point()+
  theme_bw()+scale_y_continuous()+
  xlab("Proportion of overlapping (%)")+
  ylab("Proportion of paranodal more than 4um (%)")+
  ggtitle(" Proportion of overlapping versus proportion of  paranodes higher than 4 um (%)")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))+geom_smooth(method=lm,se=FALSE,colour="black")
print(h)

a <- ggplot(hla_datastructure, aes(x=overlaps_100, y=proportion)) + 
  geom_point()+
  theme_bw()+scale_y_continuous()+
  xlab("Proportion of overlapping (%)")+
  ylab("Proportion of paranodal more than 4um (%)")+
  ggtitle(" Proportion of overlapping versus proportion of  paranodes higher than 4 um (%)")+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))+geom_smooth(method=lm,se=FALSE,colour="black")
print(a)




cols1=c("SE Paranodal Length"="blue")
jpeg("Median_paranode_age.jpg",width = 9, height = 6, units = 'in', res=300)
l<- ggplot(hla_datastructure, aes(x=age, y=paranodal_median)) + 
  geom_point()+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Age (yrs)")+
  ylab("Meadian paranodal length")+
  ggtitle("Paranodal length versus Age")+
  scale_colour_manual(values=cols1)+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(l)
dev.off()

cols1=c("SE Paranodal Length"="blue")
jpeg("Mean_paranode_age.jpg",width = 9, height = 6, units = 'in', res=300)
s<- ggplot(hla_datastructure, aes(x=age, y=paranodal_mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=paranodal_mean-paranodal_se, ymax=paranodal_mean+paranodal_se,colour="SE Paranodal Length"))+
  theme_bw()+scale_y_continuous(limits=c(2,5))+
  xlab("Age (yrs)")+
  ylab("Mean paranodal length")+
  ggtitle("Paranodal length versus Age")+
  scale_colour_manual(values=cols1)+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"))+
  theme(plot.title = element_text(hjust = 0.5))
print(s)
dev.off()

#finding correlations 

cor.test(duration,paranodal_mean, method="spearman")
cor.test(duration,hla,method="spearman")
cor.test(duration,paranodal_median, method="spearman")
cor.test(age,paranodal_mean,method="spearman")
cor.test(age,paranodal_median,method="spearman")
cor.test(hla,paranodal_mean,method="spearman")
cor.test(hla,paranodes_more4,method="spearman")
cor.test(duration,paranodes_more4,method="spearman")
cor.test(age,paranodes_more4,method="spearman")
cor.test(overlaps_100,paranodes_more4,method="spearman")
cor.test(overlaps_100,hla,method="spearman")
cor.test(overlaps_50,hla,method="spearman")
cor.test(overlaps_100,paranodal_mean,method="spearman")
cor.test(overlaps_50,paranodal_mean,method="spearman")
cor.test(overlaps_50,paranodes_more4,method="spearman")
cor.test(overlaps_100,paranodes_more4,method="spearman")


p<-lm(paranodal_mean ~ duration)
a<-lm(paranodal_median ~ duration)
o<-lm(paranodal_mean ~ age)
v<-lm(paranodal_median ~ hla)
m <- lmlm(paranodal_mean ~ hla)
n<-lm(paranodal_median ~ hla)
summary(m)
cor.test(hla, paranodal_median,method="spearman")





#lets do some plots

lengths = cbind(C48_1,C54_1, C54_2, C72_1,C72_2,C74_1,C74_2,C75_1,C75_2,C76_1,C76_2,pdc29_1,pdc29_2,
                pdc39_2,pdc40_1,pdc40_2)
Controls = c(C48_1,C54_1, C54_2, C72_1,C72_2,
             C74_1,C74_2,C75_1,C75_2,C76_1,
             C76_2,pdc29_1,pdc29_2,
             pdc39_2,pdc40_1,pdc40_2)
Controls=c(Controls,rep(NA,1000))
Controls=Controls[1:6800]
MSPatients = c(ms404_1,ms404_2,ms406_1,ms406_2,ms411_2,ms422_2,ms444_1,ms461_1,ms461_2,ms466_1,
               ms466_2,ms478_1,ms478_2,
               ms500_1,ms500_2,ms510_1,ms510_2,ms517_1,ms517_2,
               ms523_1,ms523_2, ms530_1,ms530_2,ms549_2,ms567_1,ms567_2,ms584_1,ms584_2,
               ms535_1,ms535_2,ms542_1,
               ms585_1,ms585_2, ms587_1)
export_frame=data.frame("Controls"=Controls,"MSPatients"=MSPatients )

write.csv(export_frame, file="patient-control")

MSPatients=sort(MSPatients)
a=data.frame("MS"=MSPatients)


#mean(MSPatients,na.rm=T)
#sd(MSPatients,na.rm=T)/sqrt(6800)
#median(MSPatients,na.rm=T)
#median(Controls,na.rm=T)
total_lengths=cbind(Controls,MSPatients)
total_lengths = as.data.frame(total_lengths)

#ggplot2, we need to convert the data to a data frame.  
tot=c(total_lengths[,1],total_lengths[,2])
type=c(rep("Control",nrow(total_lengths)),rep("Patient",nrow(total_lengths)))
dat=data.frame("Class"=type,"Length"=tot)
dat = dat[!is.na(dat$Length),]



pdf("paranodal_case_plots.pdf", width = 9, height = 6)
p<-ggplot(dat,aes(x=Class,y=Length))+
  geom_jitter(aes(fill=Class),width = 0.2, color="gray",alpha=0.5)+
  geom_boxplot(aes(fill=Class),alpha=0.5,notch=TRUE)+coord_flip()+
  scale_fill_manual(values=c("darkgray","cornflowerblue"))+
  ylab("Paranodal Length(µm)")+
  xlab("Case")+
  theme_bw()+theme(legend.position="none")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black",size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))
print(p)
dev.off()

Sys.setenv("plotly_username"="Pg1993")
Sys.setenv("plotly_api_key"="4n9WAnHI1lTMcMo1QPQ9")

api_create(p, filename = "r-docs-midwest-boxplots")


write.csv(dat, file="mydatabox.csv")
htmlwidgets::saveWidget(as_widget(g), "graph.html")


api_create(p, filename = "controlpatient", sharing="public")



#Lets see if they are statistically significant 
wilcox.test(Controls,MSPatients)
Controls_mean=mean(Controls,na.rm=TRUE)
Control_sd=sd(Controls,na.rm=TRUE)
Controls_interquartile_range=IQR(Controls,na.rm=TRUE)
Controls_q=quantile(Controls,na.rm=TRUE)
patient_mean=mean(MSPatients,na.rm=TRUE)
patient_sd=sd(MSPatients,na.rm=TRUE)
patient_interquartile_range=IQR(MSPatients,na.rm=TRUE)
patient_q=quantile(MSPatients,na.rm=TRUE)


#ggplot2 all cases
data=data.frame(C48_1,C54_1, C54_2, C72_1,C72_2,C74_1,C74_2,C75_1,C75_2,C76_1,C76_2,pdc29_1,pdc29_2,
                pdc39_2,pdc40_1,pdc40_2,ms404_1,ms404_2,ms406_1,ms406_2,ms411_2,ms422_2,ms444_1,ms461_1,
                ms461_2,ms466_1,ms466_2,ms478_1,ms478_2,
                ms500_1,ms500_2,ms510_1,ms510_2,ms517_1,ms517_2,
                ms523_1,ms523_2, ms530_1,ms530_2,ms549_2,ms567_1,ms567_2,ms584_1,ms584_2,
                ms535_1,ms535_2,ms542_1,
                ms585_1,ms585_2, ms587_1)

controls_inter=c(0.9335,0.9245,0.73425,1.135,0.835,0.98625,0.9635,0.9635,0.77375,1.00225,1.1065,0.8395,
                 1.252,0.87025, 0.55925,0.80675)    
mean(controls_inter)
se=(sd(controls_inter))/16
patients_inter=c(1.23375,1.3015, 0.91,0.955,0.955,0.99275, 1.02525,1.11725,0.844,0.761,1.06225,1.4935,0.89275,
                 1.31225,1.84325,0.9745,0.845,0.86275,0.9845,1.328,1.19575,0.975,1.50775,0.91325,0.907,
                 0.8905, 1.33,1.533,1.45,1.97625,0.86575,1.174,1.1335,0.87225)
mean(patients_inter)
se=(sd(patients_inter))/34



tot2=c(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6],data[,7],data[,8],data[,9],
       data[,10],data[,11],data[,12],data[,13],data[,14],data[,15],data[,16],data[,17],data[,18],
       data[,19],data[,20],data[,21],data[,22],data[,23],data[,24],data[,25],
       data[,26],data[,27],data[,28],data[,29],data[,30],data[,31],data[,32],data[,33],data[,34],
       data[,35],data[,36],data[,37],data[,38],data[,39],data[,40],data[,41],data[,42],data[,43],
       data[,44],data[,45],data[,46],data[,47],data[,48],data[,49],data[,50])



type2=c(rep("C48-pcg ",nrow(data)),rep("C54-cbp ",nrow(data)),rep("C54-cbp ",nrow(data)),
        rep("C72-pcg ",nrow(data)),rep("C72-cbp ",nrow(data)),rep("C74-pcg ",nrow(data)),
        rep("C74-cbp",nrow(data)),rep("C75-pcg",nrow(data)),rep("C75-cbp",nrow(data)),
        rep("C76-pcg",nrow(data)),
        rep("C76-cbp",nrow(data)),rep("C29-pcg",nrow(data)),rep("C29-cbp",nrow(data)),
        rep("C39-cbp",nrow(data)), rep("C40_pcg",nrow(data)),rep("C40-cbp",nrow(data)),
        rep("MS404-pcg",nrow(data)),
        rep("MS404-cbp",nrow(data)),rep("MS406-pcg",nrow(data)),rep("MS406-cbp",nrow(data)),
        rep("MS411-cbp",nrow(data)),
        rep("MS422-cbp",nrow(data)),rep("MS444-pcg",nrow(data)),rep("MS461-pcg",nrow(data)),
        rep("MS461-cbp",nrow(data)),rep("MS466-pcg",nrow(data)),rep("MS466-cbp",nrow(data)),
        rep("MS478-pcg",nrow(data)),
        rep("MS478-cbp",nrow(data)),rep("MS500-pcg",nrow(data)),rep("MS500-cbp",nrow(data)),
        rep("MS510-pcg",nrow(data)),
        rep("MS510-cbp",nrow(data)),rep("MS517-pcg",nrow(data)),
        rep("MS517-cbp",nrow(data)),rep(" MS523-pcg",nrow(data)),rep("MS523-cbp",nrow(data)),
        rep("MS530-pcg",nrow(data)),rep("MS530-cbp",nrow(data)),rep("MS549-cbp",nrow(data)),
        rep("MS567-pcg",nrow(data)),rep("MS567-cbp",nrow(data)),rep("MS584-pcg",nrow(data)),
        rep("MS584-cbp",nrow(data)),
        rep("MS535-pcg",nrow(data)),rep("MS535-cbp",nrow(data)),rep("MS542-pcg",nrow(data)),
        rep("MS585-pcg",nrow(data)),rep("MS585-cbp",nrow(data)),
        rep("MS587-pcg",nrow(data)))




data2=data.frame("Class"=type2,"Length"=tot2)
data2 = data2[!is.na(data2$Length),]
data2$Class<- factor(data2$Class,levels=unique(data2$Class))


# hacer el mean orand median de cada columna del data frame para hacer la correlacion con SMI32 y con HLA. 
write.csv(data2, file="allcases.csv")

pdf("paranodal_distributions.pdf",width=12, height=9)
p <- ggplot(data2, aes(x=Class, y=Length)) + 
  geom_jitter(aes(fill=Class),width = 0.2, color="gray",alpha=0.5)+
  geom_boxplot(aes(fill=Class),alpha=0.5,notch=TRUE)+
  coord_flip()+
  scale_fill_manual(values=c("darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray",
                             "darkgray","darkgray","darkgray",
                             "darkgray","darkgray","darkgray","darkgray","darkgray","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue",
                             "cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue"))+
  
  ylab("Length(µm)")+theme_bw()+
  xlab("Case")+
  theme_bw()+
  theme(text = element_text(size=20))+theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="azure4",size=14) )+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  ggtitle("Paranodal Length Distributions")
print(p)
dev.off()


n <- plot_ly(data2, x = ~Length, color = ~Class, type = "box")
n
api_create(n)




boxplot(lengths, horizontal=TRUE, xlab="Paranodal length(um)",las=1 ,col="slategray3",
        whisklty=1, staplelty=0, outpch=16, outcol="slategray3", main="Paranodal lengths")
boxplot(total_lengths_controls, horizontal=TRUE, xlab="Paranodal length(um)",
        las=1 ,col="slategray3", whisklty=1, staplelty=0, outpch=16, 
        outcol="slategray3", main="Paranodal length control")
boxplot(total_lengths_patients, horizontal=TRUE, xlab="Paranodal length(um)",
        las=1 ,col="slategray3", whisklty=1, staplelty=0, outpch=16, outcol="slategray3",
        main="Paranodal length patient")

#In order to know if the difference is statistically significant, we will do a t-test and compare the
#two means, the null hypothesis is that the two distributions are equal, while the alternative hypothesis 
#is thta they are different. We want to reject the null hypothesis. 
hist(total_lengths[,2], freq = F)
hist(total_lengths[,1], col = "red", add =T, freq = F)
t.test(Controls,MSPatients)


#see if the degree of overlapping correlates with the ages and duration of the disease
interquartile_controls=c(0.751,0.702,0.93825,0.83525,0.6075)
interquartile_patients=c(0.6545,1.08275,0.823,1, 0.7585,1.117,1.123,1.396,0.72625,1.073)
t.test(interquartile_patients,interquartile_controls)

#finding correlations between paranodal length and microglia per block 
paranodal_means=c(3.09, 2.65,2.98,3.28, 2.98,2.29, 2.82,2.42,2.77, 3.46,3.07,3.13,
                  3.18,3.03,3.33, 2.25, 2.47,2.63,2.51,2.47,2.14,2.44,2.68,2.55,2.69,
                  2.62,2.53,2.56,2.58,2.59,2.75,2.86,3.63,2.43,2.69,3.19)
microglia_means=c(1.15,1.53,1.49,2.03,2.7,1.62,0.9,2.08,3.76,1.811,1.28,1.53,5.21,9.14,1.54,
                  3.76, 1.89, 1.36, 0.95,2.05,2.94, 2.41,2.49, 3.11,4.15, 2.26,4.12,1.32,
                  6.21,3.22,2.21,3.4, 1.44,2.87,3.83,2.94)
paranodal_percentile=c(3.36,
                       3.05,
                       3.51,
                       3.97,
                       3.36,
                       2.73,
                       3.36,
                       2.82,
                       3.34,
                       4.2,
                       3.54,
                       3.7,
                       3.7,
                       3.54,
                       3.93,
                       2.55,
                       2.9,
                       2.98,
                       2.98,
                       2.91,
                       2.56,
                       2.78,
                       3.08,
                       2.9,
                       3.03,
                       2.97,
                       2.95,
                       2.88,
                       2.97,
                       3,
                       3.4,
                       3.11,
                       4.3,
                       2.82,
                       3.2,
                       3.69
)
myframe=data.frame("Paranodal_length"=paranodal_means,"Microglia_area"=microglia_means)

n <- ggplot(myframe, aes(x=Microglia_area, y=Paranodal_length)) + 
  geom_point()+theme_bw()+ylab("%area occupied by microglia")+
  xlab(" Paranodal Length Mean(µm)")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  ggtitle("Paranodal Length versus Microglial Inflammation")

print(n)
cor.test(paranodal_means,microglial_means,method="pearson")



#finding correlations between paranodal length and microglia per case
paranodal_means2=c(2.86, 3.13, 2.61, 2.82, 2.59,3.26,3.15, 3.18,2.36,2.63,2.51,2.47,2.29,2.61,2.65,2.54,2.58,
                   2.80,2.97,2.93)
age=c(43, 50,38,49,63,65,55,42,65,50,58,61,58,45,76,48,62,63,42,53)
pmd=c(13,32,19,18,24,12,17,26,25,8,25,24,36,48,12,12,23,20,15,27)
duration=c(21,29,22,20,39,40,34,12,36,29,NA,29,20,23,35,25,42,32,21,27)
progressive=c(12,29,15,18,7,25,19,3,12,20,13,15,6,16,35,25,20,7,15,NA)
progressive2=c(12,12,29,29,15,15,18,7,7,25,25,19,19,3,3,12,12,20,13,15,6,6,16,16,35,35,25,25,20,20,7,7,15,15,NA,NA)
microglial_means2=c(10,
                    7.17,
                    20.1,
                    7.98,
                    11.65,
                    10.45,
                    6.73,
                    10.62,
                    9.13,
                    7.98,
                    5.07,
                    8.64,
                    11.8,
                    33.9,
                    6.236,
                    25.46,
                    13.03,
                    7.62,
                    7.39,
                    9.42,
                    7.12,
                    6.19,
                    7.51,
                    7.41,
                    14.58,
                    9.06,
                    11.84,
                    6.082,
                    13.97,
                    10.38,
                    13.14,
                    11.63,
                    8.8,
                    7,
                    9.95,
                    10.39
                    
)
myframe2=data.frame("Paranodal_length"=paranodal_means2,"Microglia_area"=microglial_means2)
myframe3=data.frame("Paranodal_length"=paranodal_means2,"age"=age)
myframe4=data.frame("Paranodal_length"=paranodal_means2,"pmd"=pmd)
myframe5=data.frame("Paranodal_length"=paranodal_means2,"duration"=duration)
myframe6=data.frame("Paranodal_length"=paranodal_means2,"progressive"=progressive)
myframe7=data.frame("Paranodal_length"=paranodal_means,"progressive"=progressive2)
myframe8=data.frame("Paranodal_length"=paranodal_percentile,"Microglia_area"=microglia_means)
myframe9=data.frame("Paranodal_length"=paranodal_percentile,"progressive"=progressive2)

n <- ggplot(myframe2, aes(x=Paranodal_length, y=Microglia_area)) + 
  geom_point()+theme_bw()+ylab("%area occupied by microglia")+
  xlab(" Paranodal Length Mean(µm)")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Paranodal Length versus Microglial Inflammation")

print(n)
cor.test(microglial_means2,paranodal_means2,method="spearman")    

m <- ggplot(myframe3, aes(x=Paranodal_length, y=age)) + 
  geom_point()+theme_bw()+ylab("Age(years)")+
  xlab("Paranodal Length Mean(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+ 
  ggtitle("Age versus Paranodal length")

print(m)     
cor.test(age,paranodal_means2,method="pearson")  

s <- ggplot(myframe4, aes(x=Paranodal_length, y=pmd)) + 
  geom_point()+theme_bw()+ylab("Post-mortem delay(hours)")+
  xlab("Paranodal Length Mean(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Post-mortem delay versus Paranodal length")

print(s)                  
cor.test(paranodal_means2,pmd,method="spearman") 

t <- ggplot(myframe4, aes(x=Paranodal_length, y=duration)) + 
  geom_point()+theme_bw()+ylab("Duration of disease(years)")+
  xlab("Paranodal Length Mean(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Duration of the disease versus paranodal length")

print(t)                  
cor.test(paranodal_means2,duration,method="spearman")


k <- ggplot(myframe7, aes(x=Paranodal_length, y=progressive2)) + 
  geom_point()+theme_bw()+ylab("Progressive phase(years)")+
  xlab("Paranodal Length Mean(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Progressive phase versus paranodal length")

print(k)                  
cor.test(paranodal_means,progressive2,method="spearman")

o <- ggplot(myframe8, aes(x=Paranodal_length, y=Microglia_area)) + 
  geom_point()+theme_bw()+ylab("%occupied by microglia")+
  xlab("Paranodal Length 75% percentile(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Progressive phase versus paranodal length")

print(o)                  
cor.test(paranodal_percentile,microglia_means,method="spearman")

l <- ggplot(myframe9, aes(x=Paranodal_length, y=progressive)) + 
  geom_point()+theme_bw()+ylab("Progressive phase")+
  xlab("Paranodal Length 75% percentile(µm) ")+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="gray")+
  ggtitle("Progressive phase versus paranodal length")

print(l)                  
cor.test(paranodal_percentile,progressive2,method="spearman")                 














