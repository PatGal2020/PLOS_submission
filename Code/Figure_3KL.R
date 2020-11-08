rm(list =ls())
gc()
overall.wd = "C:/Users/pg2015/Desktop/rats"  #Figure 3KL
setwd(overall.wd) 
file.list1 = list.files(path = ".") # list of all files, "." means we are here in this path 
file.len1 = length(file.list1) 
library(ggplot2)


total.data1 = c()
k=1

for(f in 1:file.len1){
  print(f)
  casename=file.list1[f]
  # get all files:
  setwd(paste0(overall.wd,sep = "/",file.list1[f])) #this just gives the path, when f=1 we are in C14
  folder.list1 = list.files(path = ".") # list of all files
  folder.len1 = length(folder.list1)
  print(folder.list1[f])
  
  for(i in 1:folder.len1){
    cat("i =", i, "\n")
    data1 = read.csv(folder.list1[i],header = TRUE)
    data1$X0 = data1$X0/max(data1$X0)
    clean.data1 = data.frame('X0' = data1$X0,'Y0'= data1$Y0,'Y1'= data1$Y1,'Y2'= data1$Y2, "class"= factor(paste(k)), "case"=paste(casename))
    total.data1 = rbind(clean.data1, total.data1)
    
    k=k+1
    print(k)
    #observations<-nrow(clean.data)
    #store=c(store,class)
  }
}



dummy.data=total.data1
##este es para darle la vuelta a los profiles es esten mal. 

for (j in levels(dummy.data$class)){
  print(j)
  mysubset<-dummy.data[dummy.data$class==j,]
  maximum<-mysubset$X0[which.max(mysubset$Y0)]
  maximum2<-mysubset$X0[which.max(mysubset$Y1)]
  if (maximum > 0.5| maximum2<0.25) {
    mysub2 = data.frame("X0" = mysubset$X0, "Y0" = rev(mysubset$Y0), "Y1" = rev(mysubset$Y1), "class"=paste(j), "case"=mysubset$case[1])
  }
  dummy.data[dummy.data$class==j,] = mysubset
}




# hacemos un BIN y cogemos el valor de en medio del bin
bin_seq = seq(0, ceiling(max(dummy.data$X0)), length.out = 100)
middle.points = c()
for(i in 2:length(bin_seq)){
  middle.points = c(middle.points, mean(c(bin_seq[i-1], bin_seq[i])))
}


# elegir un caso de total data:
out.data = data.frame()

for (n in levels(dummy.data$case)){
  case.data = total.data1[dummy.data$case==n,]
  dum = case.data$X0
  outmeanY0 = c()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
  outsdY0 = c()
  
  for(i in 2:length(bin_seq)){
    pick_data =case.data[dum >= bin_seq[i-1] & dum <= bin_seq[i],]
    outmeanY0 = c(outmeanY0, mean(pick_data$Y0,na.rm = T))
    outsdY0 = c(outsdY0, sd(pick_data$Y0, na.rm = T))
  }
  
  outmeanY1 = c()
  outsdY1 = c()
  
  for(i in 2:length(bin_seq)){
    pick_data1 = case.data[dum >= bin_seq[i-1] & dum <= bin_seq[i],]
    outmeanY1 = c(outmeanY1, mean(pick_data1$Y1, na.rm = T))
    outsdY1 = c(outsdY1, sd(pick_data1$Y1, na.rm = T))
  }
  
  statistics_data<-data.frame("meanY0"=outmeanY0,"meanY1"=outmeanY1,
                              "sdY0"=outsdY0, "sdY1"=outsdY1, "case"=n, "bin"=middle.points)
  out.data = rbind(out.data, statistics_data)
}

####quitando NAs.

row.has.na <- apply(out.data, 1, function(x){any(is.na(x))})
new.data=out.data[!row.has.na,]
new.data2=data.frame("meanY0"=new.data$meanY0,"meanY1"=new.data$meanY1, "sdY0"=new.data$sdY0,
                     "sdY1"=new.data$sdY1,"case"=new.data$case,"bin"=new.data$bin )


####the mean of the mean 


#naive_frame<-new.data2[c(1:93, 765:954), ]
#Lt_frame<-new.data2[c(955:1050, 381:764),]
#gfp_frame<-new.data2[94:380,]

lt_list=as.factor(c("rat1", "rat2", "rat3", "rat4","rat5"))
gfp_list=as.factor(c("rat6", "rat7", "rat8"))
naive_list=as.factor(c("rat9", "rat10", "rat11"))

gfp_frame<-new.data2[new.data2$case %in% gfp_list, ]
Lt_frame<-new.data2[new.data2$case %in% lt_list, ]
naive_frame<-new.data2[new.data2$case %in% naive_list, ]



naiveY0_mean = c()
naiveY1_mean = c()
naiveY0_sd= c() 
naiveY1_sd= c()


for(i in middle.points){
  d1 = naive_frame[naive_frame$bin==i, ]
  naiveY0_mean = c(naiveY0_mean, mean(d1$meanY0,na.rm = T))
  naiveY1_mean = c(naiveY1_mean, mean(d1$meanY1,na.rm = T))
  naiveY0_sd=c(naiveY0_sd,mean(d1$sdY0,na.rm = T))
  naiveY1_sd=c(naiveY1_sd,mean(d1$sdY1,na.rm = T))
}


LtY0_mean = c()
LtY1_mean = c()
LtY0_sd=c()
LtY1_sd=c()

for(g in middle.points){
  d2=Lt_frame[Lt_frame$bin==g,]
  LtY0_mean=c(LtY0_mean, mean(d2$meanY0,na.rm = T))
  LtY1_mean=c(LtY1_mean, mean(d2$meanY1, na.rm = T))
  LtY0_sd=c(LtY0_sd, mean(d2$sdY0, na.rm = T))
  LtY1_sd=c(LtY1_sd, mean(d2$sdY1, na.rm = T))
}

gfpY0_mean = c()
gfpY1_mean = c()
gfpY0_sd= c() 
gfpY1_sd= c()


for(o in middle.points){
  d3 = gfp_frame[gfp_frame$bin==o, ]
  gfpY0_mean = c(gfpY0_mean, mean(d3$meanY0,na.rm = T))
  gfpY1_mean = c(gfpY1_mean, mean(d3$meanY1,na.rm = T))
  gfpY0_sd=c(gfpY0_sd,mean(d3$sdY0,na.rm = T))
  gfpY1_sd=c(gfpY1_sd,mean(d3$sdY1,na.rm = T))
}





##Calculating the stardard error of the mean (SEM) across two populations. 

#bin_vector=statistics_data$bin


mean_data_frame_Y0=data.frame("new_naive_Y0_mean"=naiveY0_mean,
                              "new_Lt_Y0_mean"=LtY0_mean,
                              "new_gfp_Y0_mean"=gfpY0_mean,
                              "new_naive_Y0_sd"=naiveY0_sd,
                              "new_Lt_Y0_sd"=LtY0_sd,
                              "new_gfp_Y0_sd"=gfpY0_sd,
                              "bin"=middle.points)

mean_data_frame_Y1=data.frame("new_naive_Y1_mean"=naiveY1_mean,
                              "new_Lt_Y1_mean"=LtY1_mean,
                              "new_gfp_Y1_mean"=gfpY1_mean,
                              "new_naive_Y1_sd"=naiveY1_sd,
                              "new_Lt_Y1_sd"=LtY1_sd,
                              "new_gfp_Y1_sd"=gfpY1_sd,
                              "bin"=middle.points)



mean_data_frame_Y0_new=na.omit(mean_data_frame_Y0)
mean_data_frame_Y1_new=na.omit(mean_data_frame_Y1)




pdf("Caspr1.pdf",width = 9, height = 6)
t<-ggplot(aes(x=bin,y=new_naive_Y0_mean),data=mean_data_frame_Y0_new)+
  geom_errorbar(aes(ymin=new_naive_Y0_mean-(new_naive_Y0_sd/sqrt(length(new_naive_Y0_sd))),
                    ymax=new_naive_Y0_mean+(new_naive_Y0_sd/sqrt(length(new_naive_Y0_sd))),
                    color="naive"))+
  geom_line(aes(color="naive"),size=1)+
  scale_colour_manual("Cases",values=c("naive"="darkgray", "Lt"="cornflowerblue", "gfp"="orange"))+
  geom_line(aes(x=bin,y=new_Lt_Y0_mean,color="Lt"),data=mean_data_frame_Y0_new,size=1)+
  geom_errorbar(aes(ymin=new_Lt_Y0_mean-(new_Lt_Y0_sd/sqrt(length(new_Lt_Y0_sd))),
                    ymax=new_Lt_Y0_mean+(new_Lt_Y0_sd/sqrt(length(new_Lt_Y0_sd))),
                    color="Lt"))+
  geom_line(aes(x=bin,y=new_gfp_Y0_mean,color="gfp"),data=mean_data_frame_Y0_new,size=1)+
  geom_errorbar(aes(ymin=new_gfp_Y0_mean-(new_gfp_Y0_sd/sqrt(length(new_gfp_Y0_sd))),
                    ymax=new_gfp_Y0_mean+(new_gfp_Y0_sd/sqrt(length(new_gfp_Y0_sd))),
                    color="gfp"))+
  theme_bw()+
  scale_x_continuous("Length Bin(um)",expand=c(0,0),limits=c(0,1))+
  theme(axis.line = element_line(size = 4))+
  scale_y_continuous(expand=c(0,0))+
  ylab("Average Intensity Caspr1")+
  theme(text = element_text(size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())
print(t)
dev.off()


pdf("Kv.pdf",width = 9, height = 6 )
s<-ggplot(aes(x=bin,y=new_naive_Y1_mean),data=mean_data_frame_Y1_new)+
  geom_line(aes(color="naive"),size=1)+
  geom_errorbar(aes(ymin=new_naive_Y1_mean-(new_naive_Y1_sd/sqrt(length(new_naive_Y1_sd))),
                    ymax=new_naive_Y1_mean+(new_naive_Y1_sd/sqrt(length(new_naive_Y1_sd))),
                    color="naive"))+
  geom_line(aes(x=bin,y=new_Lt_Y1_mean,color="Lt"),data=mean_data_frame_Y1_new,size=1)+
  geom_errorbar(aes(ymin=new_Lt_Y1_mean-(new_Lt_Y1_sd/sqrt(length(new_Lt_Y1_sd))),
                    ymax=new_Lt_Y1_mean+(new_Lt_Y1_sd/sqrt(length(new_Lt_Y1_sd))),
                    color="Lt"))+
  geom_line(aes(x=bin,y=new_gfp_Y1_mean,color="gfp"),data=mean_data_frame_Y1_new,size=1)+
  geom_errorbar(aes(ymin=new_gfp_Y1_mean-(new_gfp_Y1_sd/sqrt(length(new_gfp_Y1_sd))),
                    ymax=new_gfp_Y1_mean+(new_gfp_Y1_sd/sqrt(length(new_gfp_Y1_sd))),
                    color="gfp"))+
  scale_colour_manual("Cases",values=c("naive"="darkgray", "Lt"="cornflowerblue","gfp"="orange"))+
  theme_bw()+ylab("Average Intensity Kv")+
  xlab("Length Bin")+
  scale_x_continuous("Length Bin (um)",expand=c(0,0), limits=c(0,1))+
  theme(axis.line = element_line(size = 4))+
  scale_y_continuous(expand=c(0,0))+
  theme(text = element_text(size=20))+
  theme(axis.line=element_line(color="black",size=2,linetype="solid"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())
print(s)
dev.off()


