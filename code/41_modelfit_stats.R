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
require(data.table)
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
source('getlongrun2.R')
source('predict2.R')

#########################################################
#########################################################

#LOAD
setwd(filesdir); dir()
load(
  "modelfit.RData"
)

#########################################################
#########################################################

#MERGE IN OBSERVED VAL

#get obs
tmpcols<-c(
  "cowcode.num",
  "year",
  unique(rawfitdf$dv)
)
tmprows<-demdfs$one$cowcode.num%in%
  unique(rawfitdf$cow)
tmpdf<-demdfs$one[tmprows,tmpcols]
tmpdf<-gather_(
  tmpdf,
  "dv",
  "yobs",
  unique(rawfitdf$dv)
)
names(tmpdf)[names(tmpdf)=="cowcode.num"]<-
  "cow"

intersect(
  names(tmpdf),
  names(rawfitdf)
)

mfitdf<-merge(
  rawfitdf,
  tmpdf,
  by=c(
    "cow",
    "year",
    "dv"
  ),
  all.x=T
)

#should have obs everywhere
tmp<-is.na(mfitdf$yobs)
if(sum(tmp)!=0)
  stop()

#########################################################
#########################################################

#CALCULATE FIT STATS
#RMSE and MAE

getrmse<-function(hat,obs) {
  sqdev<-(hat - obs)^2
  avg.sqdev<-sum(sqdev)/length(sqdev)
  sqrt(avg.sqdev)
}
getmae<-function(hat,obs) {
  abdev<-abs(hat - obs)
  mean(abdev)
}

#fit stats
fitsumdf<-mfitdf[
  ,
  .(
    rmse=getrmse(yhat,yobs),
    #r2=(cor(yhat,yobs))^2,
    mae=getmae(yhat,yobs)
  )
  ,
  by=c(
    "dv",
    "df",
    "perm"
  )
  ]

#put in shape for plotting
fitsumdf<-gather(
  fitsumdf,
  stat,
  val,
  rmse:mae
)

fitsumdf<-spread(
  fitsumdf,
  perm,
  val
)

#choose baselinde
fitsumdf$baseline<-fitsumdf$fe

tmpcols<-unique(permdf$perm)
fitsumdf<-gather_(
  fitsumdf,
  "perm",
  "val",
  tmpcols
)
fitsumdf$pctchg<-
  100 * (fitsumdf$val - fitsumdf$baseline) /
  fitsumdf$baseline
fitsumdf$baseline<-NULL

#########################################################
#########################################################

#save out
setwd(filesdir)
save.image(
  "modelfit_stats.RData"
)