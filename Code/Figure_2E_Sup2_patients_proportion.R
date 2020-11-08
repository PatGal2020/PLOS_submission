rm(list =ls())
gc()
overall.wd = "C:/Users/Patricia/Desktop/Measurements/patients_K/"
setwd(overall.wd) 
file.list1 = list.files(path = ".") # list of all files, "." means we are here in this path 
file.len1 = length(file.list1) 
library(ggplot2)



#I want to create a function that for each case I have (16), gives me different thresholds 
#the degree of overlapping, so for each threshold we have different overlaps of different lengths. 
# so we will have a graph in each case, in the x the threshold and for each point, different 
#points of overlap length
total.data1 = c()

for(f in 1:file.len1){
  print(f)
  # get all files:
  setwd(paste0(overall.wd,file.list1[f])) #this just gives the path, when f=1 we are in C14
  folder.list1 = list.files(path = ".") # list of all files
  folder.len1 = length(folder.list1)
  for(i in 1:folder.len1){
    cat("i =", i, "\n")
    data1 = read.csv(folder.list1[i],header = TRUE)
    clean.data1 = data.frame('X0' = data1$X0,'Y0'= data1$Y0,'Y1'= data1$Y1,'Y2'= data1$Y2)
    total.data1 = rbind(clean.data1, total.data1)
  }
}

#total.data1=total.data1/2
#establishing the overlap method 
minus1 = abs(total.data1$Y0 - total.data1$Y1)
#minus1=abs(total.data1$Y0 - total.data1$Y1)/abs(max(total.data1$Y0)-max(total.data1$Y1))



thresholdseq1=seq(0, max(max(total.data2$Y0),max(total.data2$Y1)),2.5)
cushion1=10000
overlaps1 = c()
tmin1 = 25

for (j in thresholdseq1){
  # for each threshold do:
  pair.store1 = c() # store pairs of start/end here
  count = 1 # start looking from first entry onwards
  ovlps1 = length(minus1[minus1 <= j & total.data1$Y0 > tmin1 & total.data1$Y1 > tmin1])/length(minus1)
  overlaps1 = c(overlaps1, ovlps1)
  
}


proportion_dataframe<-data.frame("proportion_overlap_patient"=overlaps1, "proportion_overlap_control"=overlaps2,"threshold"=thresholdseq1)

jpeg("Proportion of overlapping regions.jpg", width = 9, height = 6, units = 'in', res=300)
n<-ggplot(aes(x=threshold,y=proportion_overlap_patient),data=proportion_dataframe)+
  geom_line(aes(colour="Patient"),size=1,alpha=0.7)+
  geom_line(aes(x=threshold,y=proportion_overlap_control,colour="Control"),data=proportion_dataframe,size=1,alpha=0.7)+
  theme_bw()+
  xlab("Threshold")+
  ylab("Proportion of overlapping regions")+
  ggtitle("Proportion of Na-Caspr overlap")+ 
  scale_color_manual(name="Legend", values=c(Patient="blue",Control="red"))+
  theme(axis.line = element_line(size = 4))+
  theme(text = element_text(size=18))+
  theme(axis.line=element_line(color="black",size=1,linetype="solid"),plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,1))

print(n)
dev.off() 

