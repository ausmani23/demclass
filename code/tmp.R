######

#compare the main resulst
codedir<-file.path(
  "c:","users","adaner","dropbox","data","_demclass","code"
)
setwd(codedir); dir()
source('dirs.R')
setwd(outputdir); dir()
repdf<-read.csv(
  'repests.csv',
  stringsAsFactors=F
)
maindf<-read.csv(
  'mainests.csv',
  stringsAsFactors=F
)

#merge
head(repdf); head(maindf)
tmpcols<-c("iv","dv","mu")
repdf<-repdf[,tmpcols]
repdf$iv<-stringr::str_replace(repdf$iv,"L\\.","")
names(repdf)<-c("iv","dv","mu_rep")

tmprows<-maindf$ivspec=="preferred" & 
  maindf$df=="one" &
  maindf$type=="longrun"
tmpcols<-c("iv","dv","mu")
maindf<-maindf[tmprows,tmpcols]
names(maindf)<-c("iv","dv","mu_main")

#view results
compdf<-merge(repdf,maindf)
compdf<-compdf[order(compdf$dv),]
round(compdf$mu_main-compdf$mu_rep)





































