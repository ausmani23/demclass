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

#set reps
reps<-10000
#seed is model-specific
#i load it during the runs
#(see below)

#########################################################
#########################################################

#function used below to summarize distribution of ests

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  se<-NA 
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

#########################################################
#########################################################

#LOAD
setwd(datadir); dir()
demdfs<-readRDS(
  "demdfs.RDS"
)

#########################################################
#########################################################

#this code replicates main results shown in 
#tables 3 and 4; i.e. democratization models 1yr panel

#########################################################
#########################################################

#PREP

#gather the dataset
thisdf<-demdfs$one

#gather the vars
dvs<-c("polity2","v2x_polyarchy")
ivs<-c(
  "L.highcapratio",
  "L.landlords",
  "L.lngdpcap",
  "L.gdpgr",
  "L.years.all",
  "L.urban02",
  "L.gini.generous"
)

#gen forms
keyforms<-lapply(dvs,function(thisdv) {
  paste(
    thisdv,
    "~",
    paste0("L.",thisdv),
    "+",
    paste0("L.",thisdv,"_regavg"),
    "+",
    "factor(year) +",
    paste0(
      ivs,
      collapse=" + "
    )
  ) %>% as.formula
})
names(keyforms)<-dvs

#gen samples (restrict to >20yrs avail)
keysamples<-lapply(dvs,function(thisdv) {
  f<-keyforms[[thisdv]]
  tmprows<-by(
    thisdf[,c("cowcode.num",all.vars(f))],
    thisdf$cowcode.num,
    function(df) {
      tmpcases<-complete.cases(df)
      if(sum(tmpcases)>=20) {
        returnrows<-tmpcases
      } else {
        returnrows<-rep(F,nrow(df))
      }
      #return
      returnrows
    }) %>% unlist
})
names(keysamples)<-dvs

#get sds in the sample
sdsdf<-lapply(dvs,function(thisdv) {
  this.samp<-keysamples[[thisdv]]
  tmpdf<-thisdf[this.samp,]
  sdvars<-c(thisdv,ivs)
  returndf<-lapply(sdvars,function(myvar) {
    mysd<-tapply(
      tmpdf[[myvar]],
      tmpdf$cowcode.num,
      sd,na.rm=T
    ) %>% mean(na.rm=T)
    data.frame(
      sd=mysd,
      iv=myvar,
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  returndf$dv<-thisdv
  returndf
}) %>% rbind.fill

#trim cols
idvars<-c(
  "cowcode.num",
  "year"
)
keyvars<-sapply(keyforms,all.vars) %>%
  unique
keepvars<-c(idvars,keyvars)
thisdf<-thisdf[,keepvars]

#########################################################
#########################################################

#RUN MODS AND GET ESTS

#run mods
keymods<-lapply(dvs,function(v) {
  thisform<-keyforms[[v]]
  this.sample<-keysamples[[v]]
  tmpdf<-thisdf[this.sample,keepvars]
  plm(
    data=tmpdf,
    formula=thisform,
    index=c(
      "cowcode.num",
      "year"
    ),
    model="within"
  )
})
names(keymods)<-dvs

setwd(outputdir)
saveRDS(
  keymods,
  "repmods.RDS"
)

#get lr ests
require(lmtest)
tmpoutput<-lapply(dvs,function(thisdv) {
  #thisdv<-dvs[[1]]
  m<-keymods[[thisdv]]
  coefs<-m$coefficients
  thisvcov<-vcovHC(
    m,
    type="HC1",
    cluster="group"
  )
  coefs.tested<-coeftest(m,thisvcov)

  #LONG-RUN ESTS
  lroutput<-lapply(ivs,function(thisiv) {
    #get sd of iv, dv
    #thisiv<-"L.highcapratio"
    tmp<-sdsdf$dv==thisdv & 
      sdsdf$iv==thisiv
    thisiv.sd<-sdsdf$sd[tmp]
    tmp<-sdsdf$dv==thisdv & 
      sdsdf$iv==thisdv
    thisdv.sd<-sdsdf$sd[tmp]
    if(length(thisiv.sd)!=1) stop("SD missing.")
    if(length(thisdv.sd)!=1) stop("SD missing")
    #did we detect var?
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    tmprows<-str_detect(names(coefs),thisiv)
    if(sum(tmprows)==0) {
      returnrow<-data.frame(
        mu=NA
      )
    } else {
      #all ests involving this var
      iv.terms<-names(coefs)[tmprows]
      #all ests with dv
      thisdv.regex<-paste0(
        "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
        thisdv,
        "$"
      )
      tmprows<-str_detect(names(coefs),thisdv.regex)
      lagdv.terms<-names(coefs)[tmprows]
      #get the longrun estimate
      means<-c(
        coefs[lagdv.terms],
        coefs[iv.terms]
      )
      #get the vcov matrix
      new.vcov<-thisvcov #from above
      rows<-row.names(new.vcov)%in%c(lagdv.terms,iv.terms)
      cols<-colnames(new.vcov)%in%c(lagdv.terms,iv.terms)
      vcov.useme<-new.vcov[rows,cols]
      #vcov needs to be ordered in the same way as the means
      new.order<-match(names(means),row.names(vcov.useme))
      vcov.useme<-vcov.useme[new.order,new.order]
      #load seeds, for replication of main results
      setwd(filesdir); dir()
      thisiv.root<-str_replace(thisiv,"L\\.","")
      seed.fname<-paste0(
        thisdv,
        ".one.preferred_",
        thisiv.root,
        "_seed.RDS"
      )
      #assign the seed to the global environment
      #which is where it lives (not in function environment)
      assign(
        ".Random.seed",
        readRDS(seed.fname),
        envir=.GlobalEnv
      )
      #sample from the multivariate distribution defined here
      draws<-MASS::mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
      numerator<-apply(draws[,iv.terms] %>% as.matrix,1,sum) #sum iv terms
      denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
      lrm.distribution<-numerator/denominator
      head(lrm.distribution); tail(lrm.distribution)
      #put this distribution in meaningful units
      lrm.distribution<-lrm.distribution * thisiv.sd
      returnrow<-summarize.distribution2(lrm.distribution)
      #return this info (but not w/ lag)
      returnrow$iv<-thisiv
    }
    return(returnrow)
  })
  
  ###
  
  returndf<-rbind.fill(lroutput)
  returndf$dv<-thisdv #to identify model
  returndf
  
})

keyestsdf<-rbind.fill(tmpoutput)

#########################################################
#########################################################

#print key ests
keyestsdf

#save out
setwd(outputdir)
write.csv(
  keyestsdf,
  'repests.csv',
  row.names=F
)
