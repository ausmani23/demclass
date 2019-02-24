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
  "mainmods.RData"
)

#########################################################
#########################################################

#FIT PERMUTATIONS

perms<-c(
  'initial',
  'global',
  'regional',
  'pooled',
  'nolag',
  'fe',
  'disruptive',
  'landlords',
  'socialforces',
  'modernization',
  'inequality',
  'preferred'
)

#these are the cows in sample
tmp<-sampsdf$df=="one" &
  sampsdf$ivspec=="preferred"
mysampnames<-sampsdf$sampname[tmp]
cows<-lapply(mysampnames,function(x) {
  samps[[x]]$cowcode.num
}) %>% unlist %>% unique %>% sort
length(cows)

#generate perms
permdf<-expand.grid(
  dv=dvs,
  df="one",
  cow=cows,
  perm=perms,
  stringsAsFactors=F
)

#trim
tmp<-rep(T,nrow(permdf))
#tmp<-tmp & permdf$perm%in%c("fe","inequality")
#tmp<-tmp & permdf$cow%in%c(2,200)
permdf<-permdf[tmp,]
permdf$seq<-1:nrow(permdf)

#########################################################
#########################################################

#define outofbounds values
obvals<-lapply(unique(permdf$dv),function(dv) {
  minval<-0
  maxval<-100
  returnlist<-list(min=minval,
                   max=maxval)
  return(returnlist)
})
names(obvals)<-unique(permdf$dv)

#########################################################
#########################################################

#LOOP THROUGH AND PREDICT!

#looop through  
tmpoutput<-lapply(permdf$seq,function(i) {
  
  #i<-13
  #permdf[permdf$cow==385,]
  #i<-385
  
  #######################################
  #SETUP
  
  #track progress
  print("#####")
  print(i)
  print(
    paste0(
      round(100*i/nrow(permdf),2),
      "%"
    )
  )
  #print("SETUP")
  
  #get params
  #for model selection
  thisdv<-permdf$dv[i]
  thisdfname<-permdf$df[i]
  thiscow<-permdf$cow[i]
  thisperm<-permdf$perm[i]
  
  #get sample
  tmp<-finaldf$dv==thisdv & 
    finaldf$df==thisdfname &
    finaldf$ivspec=="preferred"
  this.sampname<-finaldf$sampname[tmp] %>%
    unique
  thisdf<-samps[[this.sampname]]
  
  #get model, based on perm
  modnames<-c(
    "pooled",
    "nolag",
    "fe",
    'disruptive',
    'landlords',
    'socialforces',
    'modernization',
    'inequality',
    'preferred'
  )
  if(!thisperm%in%modnames) {
    thismod<-NULL
  } else if(thisperm=="pooled") {
    tmp<-finaldf$dv==thisdv &
      finaldf$df==thisdfname & 
      str_detect(finaldf$mname,"preferred")
    thismodname<-unique(finaldf$mname[tmp])
    if(length(thismodname)>1)
      stop("Duplicate models.")
    thismod<-mods[[thismodname]]
    thisform<-thismod$formula
    thismod<-plm(
      data=thisdf,
      formula=thisform,
      model="pooling",
      index=c(
        "cowcode.num",
        "year"
      )
    )
  } else if(thisperm=="nolag") {
    tmp<-finaldf$dv==thisdv &
      finaldf$df==thisdfname & 
      str_detect(finaldf$mname,"preferred")
    thismodname<-unique(finaldf$mname[tmp])
    if(length(thismodname)>1)
      stop("Duplicate models.")
    thismod<-mods[[thismodname]]
    rawform<-thismod$formula %>%
      deparse %>% paste0(collapse="")
    thisform<-str_replace(
      rawform,
      paste0("L\\.",thisdv,"\\s\\+"),
      ""
    ) %>% as.formula
    thismod<-plm(
      data=thisdf,
      formula=thisform,
      model="within",
      index=c(
        "cowcode.num",
        "year"
      )
    )
  } else if(thisperm=="fe") {
    #estimate it
    thisform<-paste0(
      thisdv,
      " ~ ",
      " factor(year)"
    ) %>% as.formula
    thismod<-plm(
      data=thisdf,
      formula=thisform,
      model="within",
      index=c(
        "cowcode.num",
        "year"
      )
    )
  } else {
    #re-estimate the model!
    tmp<-finaldf$dv==thisdv &
      finaldf$df==thisdfname & 
      str_detect(finaldf$mname,thisperm)
    thismodname<-unique(finaldf$mname[tmp])
    if(length(thismodname)>1)
      stop("Duplicate models.")
    #re-estimate on thisdf
    oldmod<-mods[[thismodname]]
    thismod<-plm(
      data=thisdf,
      formula=oldmod$formula,
      model="within",
      index=c(
        "cowcode.num",
        "year"
      )
    )
  }
  
  #finalize beta for prediction
  if(!is.null(thismod)) {
    mu<-thismod$coefficients
    mybetas<-matrix(
      mu,
      nrow=1
    )
    colnames(mybetas)<-names(mu)
    if( nrow(mybetas) == 1 ) {
      #need to add an intercept (i.e., the fixef)
      #if there isn't one (in the non-RE models)
      #this gives me the intercept for this sector
      tmp<-str_detect(colnames(mybetas),"Intercept")
      if(sum(tmp)==0) { #for FE model
        this.intercept<-plm::fixef(thismod)[thiscow]
        oldnames<-colnames(mybetas)
        mybetas<-cbind(this.intercept,mybetas)
        colnames(mybetas)<-c("(Intercept)",oldnames)
        row.names(mybetas)<-NULL
      }
    } else if( nrow(mybetas)>1 ) {
      tmp<-str_detect(colnames(mybetas),"Intercept")
      if(sum(tmp)==0) { #for FE model
        this.intercept<-plm::fixef(thismod)[thiscow]
        oldnames<-colnames(mybetas)
        mybetas<-cbind(this.intercept,mybetas)
        colnames(mybetas)<-c("(Intercept)",oldnames)
        row.names(mybetas)<-NULL    
      }
    } else {
      stop("not implemented")
    }
  }
  
  #trim df to this cow
  mydf<-thisdf[thisdf$cowcode.num==thiscow,]
  
  #######################################
  
  #MISSING OBS
  if(nrow(mydf)>0) {
    ####################
    ####################
    #which have gaps
    tmp<-complete.cases(mydf) 
    tmpdiffs<-diff(mydf$year[tmp])
    if(sum(tmpdiffs>1)>0) {
      tmp<-apply(mydf,2,function(x) sum(is.na(x)))
      tmp[tmp>0]
      warn<-paste(
        thiscow,"has gap of",max(tmpdiffs)
      )
      warning(warn)
    }
    ####################
    ####################
  } else {
    stop('No obs.')
  }
  
  #######################################
  
  #is this a prediction, w/ mod? 
  isprediction<-thisperm%in%modnames
  if(!isprediction) {
    
    if(thisperm=="initial") {
      yhat<-mydf[[thisdv]][1]
    } else if(thisperm=="global") {
      yhat<-mean(demdfs$one[[thisdv]],na.rm=T)
    } else if(thisperm=="regional") {
      myregion<-getregion(thiscow) %>% getregionname
      tmp<-demdfs$one$region==myregion
      tmpdf<-demdfs$one[tmp,]
      if(nrow(tmpdf)==0) stop()
      yhat<-mean(tmpdf[[thisdv]],na.rm=T)
    }
    #repeat N times
    yhat<-rep(yhat,nrow(mydf))
    
  } else {

    yhat<-rep(NA,nrow(mydf)) 
    for(k in 1:nrow(mydf)) {
      #k<-1
      yhat[k]<-predict2(
        thismod,
        mydf[k,],
        mybeta=mybetas[1,]
      )
      if(!is.na(yhat[k])) { 
        minval<-obvals[[thisdv]]$min
        maxval<-obvals[[thisdv]]$max
        if(!is.na(minval))
          if(yhat[k]<minval) 
            yhat[k]<-minval
        if(!is.na(maxval))
          if(yhat[k]>maxval) 
            yhat[k]<-maxval
      }
      limit<-length(yhat) 
      if(k+1<=length(yhat))
        mydf[[paste0("L.",thisdv)]][k+1]<-
        yhat[k]
    }
    yhat
  }
  
  ######
  
  #put together returndf
  data.frame(
    seq=i,
    year=mydf$year,
    yhat=yhat,
    stringsAsFactors=F
  )
  
})

########################################################
########################################################

#PUT TOGETHER

#big data, so:
require(data.table)

#put the predictions together
rawfitdf<-rbindlist(tmpoutput)

nadf<-rawfitdf[is.na(yhat)]
if(nrow(nadf)>0)
  stop('inspect me')

intersect(
  names(permdf),
  names(rawfitdf)
)
rawfitdf<-merge(rawfitdf,permdf)

#########################################################
#########################################################

#save out
setwd(filesdir)
save.image("modelfit.RData")
setwd(homedir)

#########################################################
#########################################################