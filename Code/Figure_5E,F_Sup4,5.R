rm(list =ls())
gc()
library(ggplot2)

setwd("C:/Users/pg2015/Desktop/Measurements/glutamate")





#100 TNF-IFN
glutamate_100_raw=c(74.279, 140.3706, 51.4089616,
                    101.4771,77.09886, 66.2880512, 
                    85.1785206, 87.79587, 78.12427, 
                    29.3825180,	36.3753007,	34.72454,
                    81.3700000,	91.5357560,	65.2754980, 60.1100000,	52.9894490,	45.767878,
72.0000000,	92.1922630,	63.8686990, 23.4893600,	17.7021300,	42.0033300
)



mean(c(85.1785206, 87.79587, 78.12427))
mean(c(29.3825180,	36.3753007,	34.72454))

mean(23.4893600,	17.7021300,	42.0033300)
                    
cytokine_raw=c("TNF","TNF","TNF", "IFN" , "IFN", "IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control",
               "TNF","TNF","TNF", "IFN" , "IFN", "IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control")  


cytokine_raw_timing=c("24hTNF","24hTNF","24hTNF", "24hIFN", "24hIFN","24hIFN",
                      "24hTNF-IFN","24hTNF-IFN","24hTNF-IFN", "24hControl",
                      "24hControl", "24hControl","48hTNF","48hTNF","48hTNF", "48hIFN", "48hIFN","48hIFN",
                      "48hTNF-IFN","48hTNF-IFN","48hTNF-IFN", "48hControl",
                      "48hControl", "48hControl")



timing=c("24h", "24h","24h","24h","24h","24h","24h","24h","24h","24h" ,"24h","24h",
         "48h", "48h","48h","48h","48h","48h","48h","48h","48h","48h" ,"48h","48h")
                   

glutamate_raw=data.frame("glutamate" =glutamate_100_raw, "cytokine"=cytokine_raw,
                         "timing"=timing )

glutamate_raw_timing=data.frame("glutamate" =glutamate_100_raw, "cytokine"=cytokine_raw_timing
                          )

Sample<-matrix(c(glutamate_100_raw,cytokine_raw,timing),ncol=3)



friedman.test(Sample)


pairwise.wilcox.test(glutamate_raw$glutamate,glutamate_raw$cytokine, p.adjust.method="bonferroni" )
pairwise.wilcox.test(glutamate_raw$glutamate, glutamate_raw$timing, p.adjust.method="bonferroni")


glutamate_100_raw=c(74.279, 140.3706, 51.4089616,
                    101.4771, 77.09886, 66.2880512, 
                    85.1785206, 87.79587, 78.12427, 
                    29.3825180,	36.3753007,	34.72454,
                    81.3700000,	91.5357560,	65.2754980, 60.1100000,	52.9894490,	45.767878,
                    72.0000000,	92.1922630,	63.8686990, 23.4893600,	17.7021300,	42.0033300
)



glutamate_100_mean=c(88.6861872, 81.6213371,83.6995535,33.4941196,
                79.3937513,52.9557757, 
                76.0203207, 27.7316067)

glutamate_100_se=c(46.19760339/sqrt(3),18.02517373/sqrt(3),5.002546284/sqrt(3),3.655161/sqrt(3),
                   13.24120299/sqrt(3),7.171120295/sqrt(3),14.5834942/sqrt(3),12.69388/sqrt(3))


cytokine=c("TNF", "IFN" ,"TNF-IFN","Control","TNF" ,"IFN", "TNF-IFN","Control")

timing=c("24h","24h","24h","24h", "48h","48h","48h","48h")

dose_100=c("100","100","100","100","100","100","100","100")

frame_100=data.frame( "glutamate_100_mean"=glutamate_100_mean, 
                      "glutamate_100_se"=glutamate_100_se, 
                      "cytokine"=cytokine, "timing"=timing,"dose_100"=dose_100 )


pdf("glutamate_100_plots.pdf", width = 9, height = 6)
s <- ggplot(frame_100, aes(x=cytokine, y=glutamate_100_mean, 
                           fill=timing, alpha=0.5))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=glutamate_100_mean-glutamate_100_se, ymax=glutamate_100_mean+glutamate_100_se), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values=c("#f39b7fb2", "#3c5488b2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0), limits=c(0, 120))+
  xlab("Cytokine treatment")+ ylab("Glutamate (100uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(s)
dev.off()


con=c(79.94, 87.96, 62.94, 61.83,63.78, 73.90, 75.289, 52.214,52.987, 56.039, 51.4214, 55.2165)
cien=c(104.655, 99.48, 127.89, 112.058, 93.06, 120.61, 122.72, 70.038, 99.031, 82.58,101.17, 87.66)
doscientos=c(89, 72.644,74.7965, 85.637, 60.023, 81.999, 122.3665, 86.304, 95.0866, 107.0141, 87.96344, 107.6538)
wilcox.test(con, cien)
wilcox.test(con, doscientos)


#######astrocytes  glutamate release###########

cytokine_astrocytes=c("Control", "TNF-IFN(100)", "TNF-IFN(200)")

glutamate_100_mean_astro=c(64.4608, 101.7477,89.207)
glutamate_100_se_astro=c(3.499, 4.9648,4.9235)
timing=c("24h","24h","24h")


astrocytes_frame=data.frame("cytokine"=cytokine_astrocytes, "glutamate_100_mean"=glutamate_100_mean_astro, 
                            "glutamate_100_se"=glutamate_100_se_astro)

pdf("astrocytes_100_plots.pdf", width = 9, height = 6)
a <- ggplot(astrocytes_frame, aes(x=cytokine, y=glutamate_100_mean, fill="#f39b7fb2"))+
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  geom_errorbar(aes(ymin=glutamate_100_mean-glutamate_100_se, ymax=glutamate_100_mean+glutamate_100_se), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values=c("grey", "#f39b7fb2", "#3c5488b2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0), limits=c(0, 120))+
  xlab("Cytokine treatment")+ ylab("Glutamate(uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(a)
dev.off()

#######astrocytes  glutamate uptake###########
glutamate_100_mean_uptake=c(67.44, 22.76, 6.15)
glutamate_100_se_uptake=c(4.23,4.96,15.08)

astrocytes_uptake=data.frame("cytokine"=cytokine_astrocytes, "glutamate_100_mean"=glutamate_100_mean_uptake, 
                              "glutamate_100_se"=glutamate_100_se_uptake)

pdf("astrocytes_100_plots_uptake.pdf", width = 9, height = 6)
u <- ggplot(astrocytes_uptake, aes(x=cytokine, y=glutamate_100_mean, fill="#3c5488b2"))+
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  geom_errorbar(aes(ymin=glutamate_100_mean-glutamate_100_se, ymax=glutamate_100_mean+glutamate_100_se), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values=c("grey", "#f39b7fb2", "#3c5488b2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0), limits=c(0, 80))+
  xlab("Cytokine treatment")+ ylab("Glutamate(uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(u)
dev.off()

controlglu=c(50.415, 75.749, 60.491, 69.819, 78.348, 69.832)
cienglu=c(12.553, 15.95882,43.085, 14.752, 32.204, 18.0244)
doscientosglu=c(-18.1711, 50.228, 15.912, 29.58, -53.81, 13.187)
wilcox.test(controlglu,cienglu )
wilcox.test(controlglu, doscientosglu)



##############################200 TNF-IFN########################################### 

glutamate_200_raw=c( 75.1231000,	59.9153600,	39.3179232,
                     89.7538000,	53.0874822,	63.1017100,
                     85.1583000,	98.0376956,	67.3122300,
                     29.3825180,	36.3753007,	34.72454,
                     53.7841400,	50.0390100,	58.5900000,
                     56.4889500,	28.5045500,	61.8400000,
                     63.4590400,	50.5591700,	71.7800000,
                     23.4893600,	17.7021300,	42.0033300
)
                     
                    
cytokine_raw=c("TNF","TNF","TNF", "IFN" , "IFN", "IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control",
               "TNF","TNF","TNF", "IFN" , "IFN", "IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control")  

timing=c("24h", "24h","24h","24h","24h","24h","24h","24h","24h","24h" ,"24h","24h",
         "48h", "48h","48h","48h","48h","48h","48h","48h","48h","48h" ,"48h","48h")

glutamate_raw_200=data.frame("glutamate" =glutamate_200_raw, "cytokine"=cytokine_raw,
                         "timing"=timing )

Sample2<-matrix(c(glutamate_200_raw,cytokine_raw,timing),ncol=3)

friedman.test(Sample2)

pairwise.wilcox.test(glutamate_raw_200$glutamate,glutamate_raw_200$cytokine, p.adjust.method="bonferroni" )
pairwise.wilcox.test(glutamate_raw_200$glutamate, glutamate_raw_200$timing, p.adjust.method="bonferroni")




glutamate_200_mean=c(58.1187944,68.6476641,83.5027419, 33.4941196,
                     54.1377167,48.9445000, 61.9327367,27.7316067 )



glutamate_200_se=c(17.97006977/sqrt(3),18.95185796/sqrt(3), 15.42949168/sqrt(3), 3.655161/sqrt(3),
                   5.84027/sqrt(3),
                   4.2864461/sqrt(3),17.9025725/sqrt(3),12.69388/sqrt(3) )


cytokine=c("TNF", "IFN" ,"TNF-IFN","Control","TNF" ,"IFN", "TNF-IFN","Control")

timing=c("24h","24h","24h","24h", "48h","48h","48h","48h")

dose_200=c("200","200","200","200","200","200","200","200")

frame_200=data.frame( "glutamate_200_mean"=glutamate_200_mean, 
                      "glutamate_200_se"=glutamate_200_se, 
                      "cytokine"=cytokine, "timing"=timing,"dose_200"=dose_200 )


pdf("glutamate_200_plots.pdf", width = 9, height = 6)
o <- ggplot(frame_200, aes(x=cytokine, y=glutamate_200_mean, 
                           fill=timing, alpha=0.5))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=glutamate_200_mean-glutamate_200_se, ymax=glutamate_200_mean+glutamate_200_se), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values=c("#f39b7fb2", "#3c5488b2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0), limits=c(0, 100))+
  xlab("Cytokine treatment")+ ylab("Glutamate (200uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(o)
dev.off()




##############################100 TNF-IFN two doses

glutamate_two_raw=c(74.2790000,	140.3706000,	51.4089616,
                    101.4771000,	77.0988600,	66.2880512,
                    85.1785206,	87.7958700,	78.1242700,
                    29.3825180,	36.3753007,	34.72454,
                    49.83095,	77.60728,	43.58908,	19.76593,
                    47.3342,	66.26788,	26.52796,	92.27568,
                    83.329,	77.08713,	102.9909,	120.7802,
                    23.4893600,	17.7021300,	42.0033300
                    
)

mean(c(83.329,	77.08713,	102.9909,	120.7802))
sd(c(83.329,	77.08713,	102.9909,	120.7802))
            
cytokine_two_raw=c("TNF","TNF","TNF", "IFN" , "IFN", "IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control",
               "TNF","TNF","TNF", "TNF", "IFN" , "IFN", "IFN","IFN",
               "TNF-IFN","TNF-IFN","TNF-IFN","TNF-IFN", "Control","Control","Control")  

blocker=c(35.733761, 	52.15718)
tnf=c(83.329,	77.08713,	102.9909,	120.7802)

wilcox.test(blocker,tnf )





timing_two=c("24h", "24h","24h",
         "24h","24h","24h",
         "24h","24h","24h","24h" ,"24h","24h",
         
         "48h", "48h","48h","48h","48h","48h","48h","48h","48h","48h" ,"48h","48h", 
         "48h","48h","48h")

glutamate_raw_two=data.frame("glutamate" =glutamate_two_raw, "cytokine"=cytokine_two_raw,
                             "timing"=timing_two )

Sampletwo<-matrix(c(glutamate_two_raw,cytokine_two_raw,timing_two),ncol=3)

friedman.test(Sampletwo)


pairwise.wilcox.test(glutamate_raw_two$glutamate,glutamate_raw_two$cytokine,p.adjust.method="bonferroni")
pairwise.wilcox.test(glutamate_raw_two$glutamate, glutamate_raw_two$timing, p.adjust.method="bonferroni")


glutamate_two_raw=c(74.2790000,	140.3706000,	51.4089616,
                    101.4771000,	77.0988600,	66.2880512,
                    85.1785206,	87.7958700,	78.1242700,
                    29.3825180,	36.3753007,	34.72454,
                    49.83095,	77.60728,	43.58908,	19.76593,
                    47.3342,	66.26788,	26.52796,	92.27568,
                    83.329,	77.08713,	102.9909,	120.7802,
                    23.4893600,	17.7021300,	42.0033300
                    
)



glutamate_two_mean=c(88.6861872,81.6213371,83.6995535, 33.49412,
                     47.6983100,58.1014300, 
                     96.0468075, 27.73161)


glutamate_two_se=c(46.19760339/sqrt(3),18.02517373/sqrt(3),5.002546284/sqrt(3),
                   3.655161/sqrt(3),
                   23.7781291/sqrt(4),27.9725313/sqrt(4),19.8424891/sqrt(4),
                   12.69388/sqrt(3) )

cytokine=c("TNF", "IFN" ,"TNF-IFN","Control","TNF" ,"IFN", "TNF-IFN","Control")

timing=c("24h","24h","24h","24h", "48h","48h","48h","48h")

dose_two=c("two","two","two","two","two","two","two","two")

frame_two=data.frame( "glutamate_two_mean"=glutamate_two_mean, 
                      "glutamate_two_se"=glutamate_two_se, 
                      "cytokine"=cytokine, "timing"=timing,"dose_two"=dose_two )



pdf("glutamate_two_plots.pdf", width = 9, height = 6)
n <- ggplot(frame_two, aes(x=cytokine, y=glutamate_two_mean, 
                           fill=timing, alpha=0.5))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=glutamate_two_mean-glutamate_two_se, ymax=glutamate_two_mean+glutamate_two_se), width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  scale_fill_manual(values=c("#f39b7fb2", "#3c5488b2"))+
  scale_y_continuous(expand=c(0,0))+
  xlab("Cytokine treatment")+ ylab("Glutamate (two doses 100uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(n)
dev.off()



#####lymphotoxin-interferon####################




glutamate_100_lympho= c( 50.8115221,	46.9000000,	76.5000000,
101.4771000,	77.0988600,	66.2880512,
73.6849218,	53.7000000,	41.9047600, 
29.3825180,	36.3753007,	34.72454,
41.7350530,	42.8604920,
60.1100000,	52.9894490,	45.767878,
47.2684640,	38.5463070,	73.6849218,
23.4893600,	17.7021300,	42.0033300
)


cytokine_raw_lympho=c("LT","LT","LT", "IFN" , "IFN", "IFN",
               "LT-IFN","LT-IFN","LT-IFN", "Control","Control","Control",
               "LT","LT", "IFN" , "IFN", "IFN",
               "LT-IFN","LT-IFN","LT-IFN", "Control","Control","Control")  

 

timing_lympho=c("24h", "24h","24h","24h","24h","24h","24h","24h","24h","24h" ,"24h","24h",
         "48h", "48h","48h","48h","48h","48h","48h","48h","48h","48h" ,"48h")



glutamate_raw_lympho=data.frame("glutamate" =glutamate_100_lympho,
                                "cytokine"=cytokine_raw_lympho,
                                 "timing"=timing_lympho )



glutamate_raw_lympho=data.frame("glutamate" =glutamate_100_lympho,
                                "cytokine"=cytokine_raw_lympho
)

Sample_lympho<-matrix(c(glutamate_100_lympho,cytokine_raw_lympho, timing_lympho),ncol=3)



friedman.test(Sample_lympho)


pairwise.wilcox.test(glutamate_raw_lympho$glutamate,glutamate_raw_lympho$cytokine, p.adjust.method="bonferroni" )
pairwise.wilcox.test(glutamate_raw_lympho$glutamate, glutamate_raw_lympho$timing, p.adjust.method="bonferroni")





glutamate_100_lympho= c( 50.8115221,	46.9000000,	76.5000000,
                         101.4771000,	77.0988600,	66.2880512,
                         73.6849218,	53.7000000,	41.9047600, 
                         29.3825180,	36.3753007,	34.72454,
                         41.7350530,	42.8604920,
                         60.1100000,	52.9894490,	45.767878,
                         47.2684640,	38.5463070,	73.6849218,
                         23.4893600,	17.7021300,	42.0033300
)



glutamate_lympho_mean=c(58.07051, 81.62134, 56.42989, 33.49412,42.29777,
                        52.95578, 53.16656,27.73161  )


glutamate_lympho_se=c(16.07979/sqrt(3),18.02517/sqrt(3), 16.06499/sqrt(3),
                      3.655161/sqrt(3),0.7958055/sqrt(2), 7.17112/sqrt(3), 
                      18.29676/sqrt(3),12.69388 /sqrt(3))


cytokine_lympho=c("LT", "IFN" ,"LT-IFN","Control","LT" ,"IFN", "LT-IFN","Control")

timing=c("24h","24h","24h","24h", "48h","48h","48h","48h")




frame_lympho=data.frame( "glutamate_lympho_mean"=glutamate_lympho_mean, 
                      "glutamate_lympho_se"=glutamate_lympho_se, 
                      "cytokine"=cytokine_lympho, "timing"=timing)



pdf("glutamate_lympho_plots.pdf", width = 9, height = 6)
n <- ggplot(frame_lympho, aes(x=cytokine, y=glutamate_lympho_mean, 
                           fill=timing, alpha=0.5))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=glutamate_lympho_mean-glutamate_lympho_se, ymax=glutamate_lympho_mean+glutamate_lympho_se), width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  scale_fill_manual(values=c("#f39b7fb2", "#3c5488b2"))+
  scale_y_continuous(expand=c(0,0))+
  xlab("Cytokine treatment")+ ylab("Glutamate (two doses 100uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(n)
dev.off()



#####lymphotoxin-interferon-two doses ####################


glutamate_two_lympho =c(50.8115221,	46.9000000,	76.5000000,101.4771000,	77.0988600,	
                        66.2880512, 73.6849218,	53.7000000,	41.9047600,
                         29.3825180,	36.3753007,	34.72454,
                         58.153450,	104.759400,	74.560000	,
                         47.334200,	66.267880,	26.527960, 92.275680,
                         51.911570,	95.604680,	109.440800,
                         23.4893600,	17.7021300,	42.0033300)


mean(51.911570,95.604680,109.440800)

cytokine_two_lympho=c("LT","LT","LT", "IFN" , "IFN", "IFN",
                      "LT-IFN","LT-IFN","LT-IFN", "Control","Control","Control",
                      "LT","LT", "LT", "IFN" , "IFN", "IFN", "IFN",
                      "LT-IFN","LT-IFN","LT-IFN", "Control","Control","Control")  



timing_two_lympho=c("24h", "24h","24h","24h","24h","24h","24h","24h","24h","24h" ,"24h","24h",
                "48h", "48h","48h","48h","48h","48h","48h","48h","48h","48h" ,
                "48h","48h","48h")



glutamate_two_lympho_frame=data.frame("glutamate" =glutamate_two_lympho,
                                "cytokine"=cytokine_two_lympho,
                                "timing"=timing_two_lympho )



Sample_lympho_two<-matrix(c(glutamate_two_lympho, cytokine_two_lympho,timing_two_lympho ),ncol=3)


friedman.test(Sample_lympho_two)


pairwise.wilcox.test(glutamate_two_lympho_frame$glutamate,glutamate_two_lympho_frame$cytokine, p.adjust.method="bonferroni" )
pairwise.wilcox.test(glutamate_two_lympho_frame$glutamate, glutamate_two_lympho_frame$timing, p.adjust.method="bonferroni")




glutamate_two_lympho =c(50.8115221,	46.9000000,	76.5000000,
                        101.4771000, 77.0988600,	66.2880512,
                        73.6849218,	53.7000000,	41.9047600,
                        29.3825180,	36.3753007,	34.72454,
                        58.153450,	104.759400,	74.560000	,
                        47.334200,	66.267880,	26.527960, 92.275680,
                        51.911570,	95.604680,	109.440800,
                        23.4893600,	17.7021300,	42.0033300)


lympho_two_mean=c(58.07051, 81.62134, 56.42989, 33.49412,  79.15762, 
                  58.10143, 85.65235, 27.73161)

lympho_two_se=c(16.07979/sqrt(3), 18.02517/sqrt(3), 16.06499/sqrt(3), 3.655161/sqrt(3), 
                23.64069/sqrt(3), 27.97253/sqrt(4), 
              30.02815/sqrt(3), 12.69388/sqrt(3))



cytokine_lympho_two=c("LT", "IFN" ,"LT-IFN","Control","LT" ,"IFN", "LT-IFN","Control")

timing=c("24h","24h","24h","24h", "48h","48h","48h","48h")




frame_lympho_two=data.frame( "glutamate_lympho_mean"=lympho_two_mean, 
                         "glutamate_lympho_se"=lympho_two_se, 
                         "cytokine"=cytokine_lympho_two, "timing"=timing)



pdf("glutamate_lympho_two_plots.pdf", width = 9, height = 6)
n <- ggplot(frame_lympho_two, aes(x=cytokine, y=glutamate_lympho_mean, 
                              fill=timing, alpha=0.5))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=glutamate_lympho_mean-glutamate_lympho_se, ymax=glutamate_lympho_mean+glutamate_lympho_se), width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  scale_fill_manual(values=c("#f39b7fb2", "#3c5488b2"))+
  scale_y_continuous(expand=c(0,0))+
  xlab("Cytokine treatment")+ ylab("Glutamate (two doses 100uM)")+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text =element_text(color="black", size=15))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))
print(n)
dev.off()

























