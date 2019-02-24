#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(plm)
require(tidyr)
require(rprojroot)

#set dirs
rootdir<-find_root(
  criterion=has_file('_demclass.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

setwd(datadir)
load('cowcodes.RData')
#back to homedir
setwd(homedir)

#set params
reps<-10000
set.seed(23)

#load functions
setwd(codedir)
source('demfunctions.R')

#########################################################
#########################################################

#load
setwd(datadir); dir()
demdfs<-readRDS(
  "demdfs.RDS"
)
demdf<-demdfs$one

#########################################################
########################################################

#INSPECT MISSINGNESS
#for each iv and control,
#get missingness in yrs where
#any dv

#get ivs/controls
tmp<-varsdf$type%in%c("iv")
checkvars<-varsdf$varname[tmp]

#these are the dvs; get sample where any exists
dvs<-varsdf$varname[varsdf$type=="demdv"]
tmp<-lapply(dvs,function(x) !is.na(demdf[[x]]))
dvdf<-data.frame(tmp)  
samp<-apply(dvdf,1,any) 
100 * sum(!samp)/sum(samp)

#quick check of min years
tmp<-lapply(checkvars,function(x) {
  min(demdf$year[!is.na(demdf[[x]])])
})
names(tmp)<-checkvars
tmp

#examine missingness of each
tmp.seq<-seq_along(checkvars)
tmpoutput<-lapply(tmp.seq,function(i) {
  #i<-1
  #get param
  thisvar<-checkvars[i]
  #track progress
  print(i); print(thisvar)
  print("######")
  #where there is dv but none of this iv
  tmpsamp<-samp & is.na(demdf[[thisvar]])
  #get countries and years missing
  tmpdf<-demdf[tmpsamp,]
  #browsevars<-c("countryname","ccode","year")
  #tmpdf[,browsevars]
  missdf<-by(tmpdf,tmpdf$cowcode.num,function(df) {
    data.frame(
      cowcode.num=tail(df$cowcode.num,1),
      year=sumruns(df$year)
    )
  }) %>% rbind.fill
  #also get amount missing
  N.missing<-sum(tmpsamp)
  #return
  list(
    missdf=missdf,
    N.missing=N.missing
  )
})

#name
names(tmpoutput)<-checkvars

#vars and most missingness
tmp<-sapply(tmpoutput,function(x) x$N.missing)
sort(tmp,decreasing=T)

#output dfs for vars with some missingness
tmpdir<-file.path(outputdir,"missingness")
dir.create(tmpdir,showWarnings=F)
setwd(tmpdir)
tmp<-tmpoutput[sapply(tmpoutput,function(x) x$N.missing>0)]
tmp.seq<-seq_along(tmp)
lapply(tmp.seq,function(i) {
  #i<-1
  element<-tmp[[i]]
  tmpdf<-element$missdf
  dfname<-paste0(
    names(tmp)[i],
    ".MISSING.csv"
  )
  write.csv(
    tmpdf,
    dfname,
    row.names=F
  )
})

#########################################################
#########################################################