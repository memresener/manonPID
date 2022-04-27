# filename_212="Arduino_120422_Turnerlab_212"
# filename_302="Arduino_120422_Turnerlab_302"
# filename_143="Arduino_120422_Turnerlab_143"
# filename_75="Arduino_120422_Turnerlab_75"
# filename_31="Arduino_120422_Turnerlab_31"
# 
# temp_dat<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename_212,".txt"),sep = ":")
# ardf_212<-temp_dat[,3]
# 
# temp_dat<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename_302,".txt"),sep = ":")
# ardf_302<-temp_dat[,3]
# 
# temp_dat<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename_143,".txt"),sep = ":")
# ardf_143<-temp_dat[,3]
# 
# temp_dat<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename_75,".txt"),sep = ":")
# ardf_75<-temp_dat[,3]
# 
# temp_dat<-read.csv(paste0("C:/Users/memre/Documents/Easter break raw data/",filename_31,".txt"),sep = ":")
# ardf_31<-temp_dat[,3] 
# 
# cbind_dif <- function(x = list()){
#   # Find max length
#   max_length <- max(unlist(lapply(x, length)))
#   
#   # Set length of each vector as
#   res <- lapply(x, function(x){
#     length(x) <- max_length
#     return(x)
#   })
#   
#   return(as.data.frame(res))
# }
# 
# ardf<-cbind_dif(list(ardf_212,ardf_302,ardf_143,ardf_75,ardf_31))
# colnames(ardf)<-c("c212","c302","c143","c75","c31")


decim<-30 # Decimate data by
lmax=5000 # Max lag in no. points


ardf_ts<-ts(ardf)
subset<-seq(from=1, to=length(ardf_ts[,1]),by=decim)
s_ardf<-ardf_ts[subset,]



m<-matrix(nrow=((lmax*2)+1),ncol=length(c[1,]))
c<- combn(c(1:length(ardf)), 2)

for (i in c(1:length(ardf))){c<-cbind(c, c(i,i))} # Self corr

for(i in c(1:length(c[1,]))){
  ac<-ccf(s_ardf[,c[1,i]],s_ardf[,c[2,i]],lag.max=lmax,na.action=na.pass)
  acfs<-ac$acf
  m[,i]<-acfs[c(1:length(acfs))]
}

dev.off()
par(mfrow=c(3,5))


for(i in c(1:length(c[1,]))){
  text1 = as.character(colnames(ardf)[c[1,i]])
  text2 = as.character(colnames(ardf)[c[2,i]])
  
  x_s=seq(from=-((decim*lmax)/60), to=((decim*lmax)/60),by=decim/60)
  
  y_max<-m[which(max(m[,i]) == m[,i]),i]
  x_max=x_s[which(max(m[,i]) == m[,i])]
  
  if(i%%5 == 1){
    y_lab="CCF Correlation"
  } else {y_lab=""}
  
  plot(y=m[,i], x=x_s,main=paste(text1, text2),xlab=paste0("Max: ", format(round(y_max,3),nsmall=3), " Lag: ", format(round(x_max,3),nsmall=3)),ylab=y_lab)
  points(x_max, y_max, col = "red", pch = 19)
}