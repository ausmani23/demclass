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

#PERMUATIONS

#these are cfactuals
tmpcfactuals<-c(
  "observed",
  "predicted", #as observed,
  #"dcap",
  #"land",
  #"all3",
  "gdp",
  "ed",
  "urban",
  "modernization",
  ####
  "dcapyear",
  "landyear",
  "socialforces",
  ###
  "gini",
  ###
  "full"
)

#load trajectories
setwd(codedir)
source('trajs.R') 

#these are the cows in sample
tmp<-sampsdf$df=="one" & 
  sampsdf$ivspec=="preferred"
mysampnames<-sampsdf$sampname[tmp]
cows<-lapply(mysampnames,function(x) {
  samps[[x]]$cowcode.num
}) %>% unlist %>% unique %>% sort
length(cows)

#generate perms
cfdf<-expand.grid(
  dv=dvs,
  df="one",
  cow=cows,
  cfactual=tmpcfactuals,
  betatype=c("fixed","mixed","full"),
  stringsAsFactors=F
)

#trim
tmp<-rep(T,nrow(cfdf))
cfreps<-50
#tmp<-tmp & cfdf$cow%in%c(2,140)
tmp<-tmp & cfdf$betatype=="fixed"
#tmp<-tmp & cfdf$betatype!="full"
cfdf<-cfdf[tmp,]
cfdf$seq<-1:nrow(cfdf)

#########################################################
#########################################################

#PREP THE MODELDRAWS
drawdf<-expand.grid(
  dv=unique(cfdf$dv),
  df=unique(cfdf$df)
)
tmpseq.i<-1:nrow(drawdf)
drawdf$drawname<-apply(
  drawdf,1,paste0,collapse="."
)
modeldraws<-lapply(tmpseq.i,function(i) {
  #i<-5
  #get model
  tmp<-finaldf$dv==drawdf$dv[i] & 
    finaldf$df==drawdf$df[i] &
    str_detect(finaldf$mname,"preferred")
  thismodname<-unique(finaldf$mname[tmp])
  if(length(thismodname)>1)
    stop("Duplicate models.")
  thismod<-mods[[thismodname]]
  #get coefs and draw
  mu<-thismod$coefficients
  robvcov<-vcovHC(
    thismod,
    type="HC0",
    cluster="group"
  )
  #robvcov<-thismod$vcov
  betas<-MASS::mvrnorm(
    n=cfreps,
    mu=mu,
    Sigma=robvcov
  )
  list(
    mu=mu,
    betas=betas
  )
})
names(modeldraws)<-drawdf$drawname

#define outofbounds values
obvals<-lapply(unique(cfdf$dv),function(dv) {
  minval<-0
  maxval<-100
  returnlist<-list(min=minval,
                   max=maxval)
  return(returnlist)
})
names(obvals)<-unique(cfdf$dv)

#########################################################
#########################################################

#LOOP THROUGH AND PREDICT!

#looop through  
tmpoutput<-lapply(cfdf$seq,function(i) {
  
  #i<-5
  #cfdf[cfdf$cow==385,]
  #i<-59
  
  #######################################
  #SETUP
  
  #track progress
  print("#####")
  print(i)
  print(
    paste0(
      round(100*i/nrow(cfdf),2),
      "%"
    )
  )
  #print("SETUP")
  
  #get params
  #for model selection
  thisdv<-cfdf$dv[i]
  thisdf<-cfdf$df[i]
  thiscow<-cfdf$cow[i]
  thiscf<-cfdf$cfactual[i]
  thisbetatype<-cfdf$betatype[i]
  
  #get model
  tmp<-finaldf$dv==thisdv &
    finaldf$df==thisdf & 
    str_detect(finaldf$mname,"preferred")
  thismodname<-unique(finaldf$mname[tmp])
  if(length(thismodname)>1)
    stop("Duplicate models.")
  thismod<-mods[[thismodname]]
  
  #also get this sampname
  this.sampname<-finaldf$sampname[tmp] %>%
    unique
  
  #get modeldraws
  mdrawname<-paste0(
    thisdv,".",thisdf
  )
  thismod.draws<-modeldraws[[mdrawname]]
  fixbetas<-thismod.draws$mu #fixed
  fullbetas<-thismod.draws$betas #full uncert.
  #mixed betas retain uncertainty
  #only in the keyvars
  mixbetas<-matrix(
    rep(fixbetas,cfreps),
    ncol=length(fixbetas),
    byrow=T
  )
  colnames(mixbetas)<-names(fixbetas)
  keyvars<-c(
    "highcapratio",
    "landlords",
    "lngdpcap"
  )
  tmp1<-str_detect(
    colnames(fullbetas),
    paste0(keyvars,collapse="|")
  )
  tmp2<-str_detect(
    colnames(mixbetas),
    paste0(keyvars,collapse="|")
  )
  mixbetas[,tmp2]<-fullbetas[,tmp1] #mixed uncert.
  
  #select the right one
  if(thisbetatype=="fixed") {
    mybetas<-matrix(
      fixbetas,
      nrow=1
    )
    colnames(mybetas)<-names(fixbetas)
  } else if(thisbetatype=="mixed") {
    mybetas<-mixbetas
  } else if(thisbetatype=="full") {
    mybetas<-fullbetas
  }
  
  #finalize betas
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
  
  #get df/sample
  thisdf<-samps[[this.sampname]]
  thisdf<-thisdf[thisdf$cowcode.num==thiscow,]
  
  #######################################
  
  #MISSING OBS
  if(nrow(thisdf)>0) {
    ####################
    ####################
    #which have gaps
    tmp<-complete.cases(thisdf) 
    tmpdiffs<-diff(thisdf$year[tmp])
    if(sum(tmpdiffs>1)>0) {
      tmp<-apply(thisdf,2,function(x) sum(is.na(x)))
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
  
  #CFACTUAL
  #print("DEFINE CFACTUAL")
  
  if( thiscf=="predicted" ) {
    #nothing doing
  } else if ( thiscf=="gdp" ) {
    
    thisdf$L.lngdpcap<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    
  } else if ( thiscf=="dcap" ) {
    
    L.gdp<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    thisdf$L.highcapratio<-
      psmooth(
        L.gdp,
        dv="highcapratio",
        iv="lngdpcap",
        subset="advanced"
      )
    
  } else if ( thiscf=="land" ) {
    
    L.gdp<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    thisdf$L.landlords<-
      psmooth(
        L.gdp,
        dv="landlords",
        iv="lngdpcap",
        subset="advanced"
      )
    
  } else if ( thiscf=="gini" ) {
    
    # L.gdp<-psmooth(
    #   thisdf$year - 1, #lagged
    #   dv="lngdpcap",
    #   iv="year",
    #   subset="advanced"
    # )
    thisdf$L.gini.generous<-
      psmooth(
        thisdf$year - 1,
        dv="gini.generous",
        iv="year",
        subset="advanced"
      )
    
  } else if (thiscf=="all3") {
    
    thisdf$L.lngdpcap<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    thisdf$L.highcapratio<-
      psmooth(
        thisdf$L.lngdpcap,
        dv="highcapratio",
        iv="lngdpcap",
        subset="advanced"
      )
    thisdf$L.landlords<-
      psmooth(
        thisdf$L.lngdpcap,
        dv="landlords",
        iv="lngdpcap",
        subset="advanced"
      )
    
  } else if (thiscf=="dcapyear") {
    
    thisdf$L.highcapratio<-
      psmooth(
        thisdf$year - 1,
        dv="highcapratio",
        iv="year",
        subset="advanced"
      )
    
  } else if (thiscf=="landyear") {
    
    thisdf$L.landlords<-
      psmooth(
        thisdf$year - 1,
        dv="landlords",
        iv="year",
        subset="advanced"
      )
    
    
  } else if (thiscf=="socialforces") {
    
    thisdf$L.highcapratio<-
      psmooth(
        thisdf$year - 1,
        dv="highcapratio",
        iv="year",
        subset="advanced"
      )
    thisdf$L.landlords<-
      psmooth(
        thisdf$year - 1,
        dv="landlords",
        iv="year",
        subset="advanced"
      )
    
  } else if (thiscf=="ed") {
    
    thisdf$L.years.all<-
      psmooth(
        thisdf$year - 1,
        dv="years.all",
        iv="year",
        subset="advanced"
      )

    
  } else if (thiscf=="urban") {
    
    thisdf$L.urban02<-
      psmooth(
        thisdf$year - 1,
        dv="urban02",
        iv="year",
        subset="advanced"
      )
    
  } else if (thiscf=="modernization") {
    
    thisdf$L.lngdpcap<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    thisdf$L.years.all<-
      psmooth(
        thisdf$year - 1,
        dv="years.all",
        iv="year",
        subset="advanced"
      )
    thisdf$L.urban02<-
      psmooth(
        thisdf$year - 1,
        dv="urban02",
        iv="year",
        subset="advanced"
      )
    
  } else if (thiscf=="full") {
    
    thisdf$L.lngdpcap<-psmooth(
      thisdf$year - 1, #lagged
      dv="lngdpcap",
      iv="year",
      subset="advanced"
    )
    thisdf$L.years.all<-
      psmooth(
        thisdf$year - 1,
        dv="years.all",
        iv="year",
        subset="advanced"
      )
    thisdf$L.urban02<-
      psmooth(
        thisdf$year - 1,
        dv="urban02",
        iv="year",
        subset="advanced"
      )
    
    thisdf$L.highcapratio<-
      psmooth(
        thisdf$year - 1,
        dv="highcapratio",
        iv="year",
        subset="advanced"
      )
    thisdf$L.landlords<-
      psmooth(
        thisdf$year - 1,
        dv="landlords",
        iv="year",
        subset="advanced"
      )
    
    thisdf$L.gini.generous<-
      psmooth(
        thisdf$year - 1,
        dv="gini.generous",
        iv="year",
        subset="advanced"
      )
    
  }
  
  #######################################
  
  #PREDICITON
  
  #print("PREDICTION") 
  #output prediction if not observed
  tmpseq.j<-1:nrow(mybetas)
  returndf<-lapply(tmpseq.j,function(j) {
    
    if(thiscf!="observed") {
      
      #j<-1
      if(j%%10==0) {
        print(
          paste(
            j,"/",length(tmpseq.j)
          )
        )
      }
      thisbeta<-mybetas[j,]
      ####
      yhat<-rep(NA,nrow(thisdf)) 
      for(k in 1:nrow(thisdf)) {
        #k<-1
        yhat[k]<-predict2(
          thismod,
          thisdf[k,],
          mybeta=thisbeta
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
          thisdf[[paste0("L.",thisdv)]][k+1]<-
          yhat[k]
      }
      returndf<-data.frame(
        seq=i,
        rep=j,
        year=thisdf$year,
        yhat=yhat,
        stringsAsFactors=F
      )
    } else {
      returndf<-data.frame(
        seq=i,
        rep=j, #for each rep
        year=thisdf$year,
        yhat=thisdf[[thisdv]],
        stringsAsFactors=F
      )
    }
    returndf
  }) %>% rbind.fill
  
  #ADD COVARIATES
  mergevars<-c(
    "year",
    "L.lngdpcap",
    "L.highcapratio",
    "L.landlords",
    "L.gini.generous"
  )
  returndf<-merge(
    returndf,
    thisdf[,mergevars]
  )
  
  #######################################
  #RETURN
  list(
    returndf=returndf
  )
  
})

#########################################################
#########################################################

#PUT TOGETHER

#big data, so:
require(data.table)

#put the predictions together
rawpredictdf<-rbindlist(
  lapply(tmpoutput,function(x) x$returndf)
)

nadf<-rawpredictdf[is.na(yhat)]
if(nrow(nadf)>0)
  stop('inspect me')

intersect(
  names(cfdf),
  names(rawpredictdf)
)
rawpredictdf<-merge(rawpredictdf,cfdf)

#########################################################
#########################################################

#save out
setwd(filesdir)
 save.image("cfactuals.RData")
# #save.image('cfactuals_tmp.RData')
setwd(homedir)

#########################################################
#########################################################