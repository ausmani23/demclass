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
cfreps<-100
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
  "cfactuals.RData"
)

#########################################################
#########################################################

#ADD DEV/ADV AVG AND LOESS

rawpredictdf<-data.table(rawpredictdf)

loopdf<-expand.grid(
  subset=c("developing","advanced"),
  type=c("avg","loess"),
  stringsAsFactors=F
)

#loess f
tmpf<-function(yhat,year) {
  tmploess<-loess(yhat ~ year)
  list(
    yhat=predict(tmploess,unique(year)),
    year=unique(year)
  )
}

tmpseq.i<-1:nrow(loopdf)
newdfs<-lapply(tmpseq.i,function(i) {
  
  #i<-4
  print(i)
  thisrow<-loopdf[i,]
  
  if(thisrow$subset=="developing") {
    tmp<-!demdfs$one$advanced
  } else {
    tmp<-demdfs$one$advanced
  }
  mycows<-demdfs$one$cowcode.num[tmp] %>%
    unique
  tmpss<-rawpredictdf$cow%in%mycows
  
  if(thisrow$type=="avg") {
    returndf<-rawpredictdf[
      tmpss
      ,
      .(
        yhat=mean(yhat)
      )
      ,
      by=c(
        'year',
        'dv',
        'df',
        'cfactual',
        'betatype',
        'rep'
      )
      ]
  } else {
    returndf<-rawpredictdf[
      tmpss
      ,
      tmpf(yhat,year)
      ,
      by=c(
        "dv",
        "df",
        "cfactual",
        "betatype",
        "rep"
      )
      ]
  }
  cowname<-paste0(
    thisrow$subset,".",
    thisrow$type
  )
  returndf$cow<-cowname
  #return
  returndf
  
})

#put together
newdf<-rbind.fill(newdfs)
predictdf<-rbindlist(
  list(
    rawpredictdf,
    newdf
  ),
  fill=T
)

#########################################################
#########################################################

#COMPUTE DIFFS

#for each cow, compute diff w/ predicted and observed

diffdf<-predictdf
diffdf$seq<-
  diffdf$L.highcapratio<-
  diffdf$L.lngdpcap<-
  diffdf$L.landlords<-
  diffdf$L.gini.generous<-NULL
diffdf<-spread(
  diffdf,
  cfactual,
  yhat
)
diffdf<-gather(
  diffdf,
  comparison,
  baseval,
  observed:predicted
)

tmp<-!tmpcfactuals%in%c("observed","predicted")
vars<-tmpcfactuals[tmp]
for(var in vars) {
  diffdf[[var]] <- diffdf[[var]] - diffdf$baseval
}
diffdf$baseval<-NULL
diffdf<-data.table(diffdf)

#########################################################
#########################################################

#IDENTICAL..

#FROM AVG
#calc avg each scenario,
#and then calc diff
# meansdf<-predictdf[
#   ,
#   .(
#     yhat=mean(yhat)
#   )
#   ,
#   by=c(
#     "dv",
#     "df",
#     "cow",
#     "betatype",
#     "cfactual",
#     "rep"
#   )
#   ]
# tmpdf<-meansdf
# tmpdf<-spread(
#   tmpdf,
#   cfactual,
#   yhat
# )
# tmpdf

#FROM AVG AREA
#calc avg area each scenario
#and then calc diffs..
meansdf<-predictdf[
  ,
  .(
    yhat=getarea(
      year,
      yhat
    )/100
  )
  ,
  by=c(
    "dv",
    "df",
    "cow",
    "betatype",
    "cfactual",
    "rep"
  )
]
tmpdf<-meansdf
tmpdf<-spread(
  tmpdf,
  cfactual,
  yhat
)
tmpdf


#merge in dvsd
tmp<-sdsdf$var%in%unique(tmpdf$dv) &
  str_detect(sdsdf$df,"one")
tmpcols<-c("var","sd")
dvsdf<-sdsdf[tmp,tmpcols]
names(dvsdf)<-c("dv","dvsd")
tmpdf<-merge(
  tmpdf,
  dvsdf
)
vars<-vars

#do it for each var
comps<-c(
  "predicted",
  "observed"
)
tmpdf<-gather(
  tmpdf,
  comparison,
  baseval,
  observed:predicted
)
tmpdf<-data.table(tmpdf)

for(var in vars) {
  #var<-"all3"
  #get raw diff
  diffraw<-paste0(var,"_diffraw")
  tmpdf[[diffraw]] <- tmpdf[[var]] - tmpdf$baseval
  #get sd diff
  diffsd<-paste0(var,"_diffsd")
  tmpdf[[diffsd]] <- tmpdf[[diffraw]]/tmpdf$dvsd
  #get pct diff
  diffpct<-paste0(var,"_diffpct")
  tmpdf[[diffpct]] <- 100 * tmpdf[[diffraw]]/tmpdf$baseval
}

#for ranks
tmpdf<-by(tmpdf,tmpdf$comparison,function(df) {
  #df<-tmpdf[tmpdf$comparison=="observed",]
  tmpf<-ecdf(df$baseval)
  for(var in vars) {
    diffrank<-paste0(var,"_diffrank")
    df[[diffrank]] <- 100 * (
      tmpf(df[[var]]) - tmpf(df$baseval)
    )
  }
  df
}) %>% rbind.fill

tmpdf<-data.table(tmpdf)
tmp<-str_detect(names(tmpdf),paste0(vars,collapse="|")) &
  !names(tmpdf)%in%vars
gathercols<-names(tmpdf)[tmp]
dropcols<-c(
  "observed",
  "predicted",
  vars
)
tmpdf<-tmpdf[,!names(tmpdf)%in%dropcols,with=F]
gaindf<-gather_(
  tmpdf,
  "cfactual",
  "ydiff",
  gathercols
)
gaindf<-data.table(gaindf)
gaindf$stat<-str_extract(
  gaindf$cfactual,
  "\\_.*$"
) %>% str_replace(
  "\\_",""
) 
gaindf$cfactual<-str_replace(
  gaindf$cfactual,
  "\\_.*$",""
)

#summarize
gaindf<-gaindf[
  ,
  .(
    diff=mean(ydiff),
    diff.max=quantile(ydiff,0.975),
    diff.min=quantile(ydiff,0.025)
  )
  ,
  by=c(
    "dv",
    "df",
    "cow",
    "betatype",
    "cfactual",
    "comparison",
    "stat"
  )
  ]

#get

#########################################################
#########################################################

#save out
setwd(filesdir)
save.image(
  'cfactuals_stats.RData'
)