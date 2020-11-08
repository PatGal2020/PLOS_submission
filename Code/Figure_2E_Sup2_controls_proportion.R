rm(list =ls())
gc()
overall.wd = ("C:/Users/pg2015/Desktop/Measurements/controls_K")
setwd(overall.wd) 
file.list2 = list.files(path = ".") # list of all files, "." means we are here in this path 
file.len2 = length(file.list2)



#I want to create a function that for each case I have (16), gives me different thresholds 
#the degree of overlapping, so for each threshold we have different overlaps of different lengths. 
# so we will have a graph in each case, in the x the threshold and for each point, different 
#points of overlap length
total.data2 = c()

for(f in 1:file.len2){
  print(f)
  # get all files:
  setwd(paste0(overall.wd,sep = "/", file.list2[16])) #this just gives the path, when f=1 we are in C14
  folder.list2 = list.files(path = ".") # list of all files
  folder.len2 = length(folder.list2) # number of files
  for(i in 1:folder.len2){
    cat("i =", i, "\n")
    data2 = read.csv(folder.list2[i],header = TRUE)
    clean.data2 = data.frame('X0' = data2$X0,'Y0'= data2$Y0,'Y1'= data2$Y1,'Y2'= data2$Y2)
    total.data2 = rbind(clean.data2, total.data2)
  }
}


total.data2=total.data2
#total.data2=total.data2/2
#total.data2=total.data2*2
#types of noralization 


#establishing the overlap method 
minus2 = abs(total.data2$Y0 - total.data2$Y2)


thresholdseq2=50
#thresholdseq2=seq(0, max(max(total.data2$Y0),max(total.data2$Y2)),1.5)
cushion2=10000
overlaps2 = c()
tmin2 = 75

for (j in thresholdseq2){
  # for each threshold do:
  pair.store2 = c() # store pairs of start/end here
  count = 1 # start looking from first entry onwards
  ovlps2 = length(minus2[minus2 <= j & total.data2$Y0 > tmin2 & total.data2$Y2 > tmin2])/length(minus2)
  overlaps2 = c(overlaps2, ovlps2)
  
}


plot(thresholdseq2, overlaps2, type = "l",col="red")