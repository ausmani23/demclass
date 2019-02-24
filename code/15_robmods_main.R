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
source('getlongrun2.R')

#########################################################
#########################################################

#LOAD
setwd(filesdir); dir()
load(
  "mainmods.RData"
)

#this file shows folly of 
#estimating the model w/o lagdv
setwd(codedir)
source('lagdv.R')

#########################################################
#########################################################

#SET UP ROBMODS LOOP

#models are named externally
setwd(metadir); dir()
rawrobdf<-read.csv(
  'robustness.csv',
  stringsAsFactors=F
)

#those which are NA, are deprecated
rawrobdf<-rawrobdf[!is.na(rawrobdf$order),]

#make a table, for output
rawrobdf$letter<-
  paste0(
    "(",LETTERS[1:nrow(rawrobdf)],")"
  )
setwd(outputdir)
write.csv(
  rawrobdf[,c("letter","propername")],
  "tab_robmods.csv",
  row.names=F
)

#to be used below
getmodname<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$propername[rawrobdf$mname==mname]
}

getmodorder<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$order[rawrobdf$mname==mname]
}

#to loop through
robdf<-expand.grid(
  mod=rawrobdf$mname,
  dv=c(
    "polity2",
    "v2x_polyarchy"
  ),
  df="one",
  stringsAsFactors=F
)

#########################################################
#########################################################

#LOOP THROUGH

#trim?
tmp<-rep(T,nrow(robdf))
robdf<-robdf[tmp,]
robdf$seq<-1:nrow(robdf)

#LOOP!

tmp.seq<-1:nrow(robdf)
tmpoutput<-lapply(tmp.seq,function(i) {
  
  #i<-1
  #i<-which(robdf$mod=="nolag")[1]
  #get param
  thisdv<-robdf$dv[i]
  thisdfname<-robdf$df[i]
  thismod<-robdf$mod[i]
  thisgroup<-rawrobdf$group[rawrobdf$mname==thismod]
  #track progress
  print("####")
  print(
    paste(
      "Estimating model",
      i,"of",max(tmp.seq)
    )
  )
  print(thismod)
  
  #############
  
  #PREPATORY
  
  #get sample and formula from pref
  tmp<-finaldf$dv==thisdv &
    finaldf$df==thisdfname
  tmp<-tmp & str_detect(finaldf$mname,"preferred")
  this.sampname<-finaldf$sampname[tmp] %>%
    unique
  if(length(this.sampname)>1)
    stop()
  this.mname<-unique(finaldf$mname[tmp])
  if(length(this.mname)>1)
    stop()
  #identify sample w/ logicals,
  #which makes robustness process easier,
  #since we are adding new vars etc.c
  thisdf<-demdfs[[thisdfname]]
  this.samp<-paste0(
    thisdf$cowcode.num,
    thisdf$year
  ) %in% paste0(
    samps[[this.sampname]]$cowcode.num,
    samps[[this.sampname]]$year
  )
  this.mname<-unique(finaldf$mname[tmp])
  thismodel.arg<-"within" #default pref.
  thisform<-forms[[this.mname]]
  
  #these are the estimates we extract
  keyvars<-c(
    "lngdpcap",
    "highcapratio",
    "landlords",
    "gdpgr",
    "polity2_regavg",
    "v2x_polyarchy_regavg",
    "landlords",
    "gini.generous",
    "years.all",
    "urban02"
  )
  names(keyvars)<-
    keyvars
  
  #############
  
  #which model?
  #redefine samp/form as necessary
  #and then re-estimate below
  
  #############
  
  #SET UP FORMULA/SAMPLE
  
  if(thismod=="pref") {
    
    #don't change anything
    
  } else if (thismod=="re") {
    
    thismodel.arg<-"random"
    
  } else if (thismod=="pooled") {
    
    thismodel.arg<-"pooling"
    
  } else if (thismod=="liberal") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdv,
      "v2x_liberal"
    ) %>% as.formula
    thisdv<-"v2x_liberal"
    this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="deliberative") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdv,
      "v2xdl_delib"
    ) %>% as.formula
    thisdv<-"v2xdl_delib"
    this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="participatory") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdv,
      "v2x_partip"
    ) %>% as.formula
    thisdv<-"v2x_partip"
    this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="egalitarian") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdv,
      "v2x_egal"
    ) %>% as.formula
    thisdv<-"v2x_egal"
    this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="hcaplf") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      "highcapratio",
      "highcaplf"
    ) %>% as.formula
    keyvars['highcapratio']<-"highcaplf"
    
  } else if (thismod=="hcappcap") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      "highcapratio",
      "highcap_pcap"
    ) %>% as.formula
    keyvars['highcapratio']<-"highcap_pcap"
    
  } else if (thismod=="hcapgov") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      "highcapratio",
      "highcapratio.gov"
    ) %>% as.formula
    keyvars['highcapratio']<-"highcapratio.gov"
    
  } else if (thismod=="twolags") {
    
    thisdvlag<-paste0("L.",thisdv)
    thisdvlags<-paste0(
      c("L.","L2."),
      thisdv,
      collapse=" + "
    )
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdvlag,
      thisdvlags
    ) %>% as.formula
    
  } else if (thismod=="threelags") {
    
    thisdvlag<-paste0("L.",thisdv)
    thisdvlags<-paste0(
      c("L.","L2.","L3."),
      thisdv,
      collapse=" + "
    )
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdvlag,
      thisdvlags
    ) %>% as.formula
    
  } else if (thismod=="fourlags") {
    
    thisdvlag<-paste0("L.",thisdv)
    thisdvlags<-paste0(
      c("L.","L2.","L3.","L4."),
      thisdv,
      collapse=" + "
    )
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdvlag,
      thisdvlags
    ) %>% as.formula
    
  } else if (thismod=="nolag") {
    
    thisdvlag<-paste0(
      paste0("L.",thisdv),"\\s+\\+"
    )
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      thisdvlag,
      ""
    ) %>% as.formula
    
  } else if (thismod=="istatewar") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.gini\\.generous",
      "L.gini.generous + L.wars_interstate"
    ) %>% as.formula
    
  } else if (thismod == "allwar") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.gini\\.generous",
      "L.gini.generous + L.war"
    ) %>% as.formula
    
  } else if (thismod == "wardeaths") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.gini\\.generous",
      "L.gini.generous + L.deaths_interstate"
    ) %>% as.formula
    
  } else if (thismod=="modernization_pref") {
    
    this.mname<-paste0(thisdv,".one.modernization")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="disruptive_pref") {
    
    
    this.mname<-paste0(thisdv,".one.disruptive")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="landlords_pref") {
    
    this.mname<-paste0(thisdv,".one.landlords")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="socialforces_pref") {
    
    this.mname<-paste0(thisdv,".one.socialforces")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="inequality_pref") {
    
    this.mname<-paste0(thisdv,".one.inequality")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (thismod=="preferred_pref") {
    
    this.mname<-paste0(thisdv,".one.preferred")
    thisform<-forms[[this.mname]]
    #this.samp<-rep(T,nrow(thisdf))
    
  } else if (str_detect(thismod,"thresh[0-9]{1,2}$")) {
    
    this.minN<-str_extract(
      thismod,
      "[0-9]{1,2}$"
    ) %>% as.numeric
    allvars<-all.vars(thisform)
    tmp<-by(
      thisdf[,allvars],
      thisdf$cowcode.num,
      function(df) {
        sum(complete.cases(df))>=this.minN
      }
    )
    goodcows<-names(which(tmp))
    this.samp<-complete.cases(thisdf[,allvars]) &
      thisdf$cowcode.num%in%goodcows
    
  } else if (thismod == "hcap2") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.highcapratio",
      "L.highcapratio + L.highcapratio2"
    ) %>% as.formula
    
  } else if (thismod == "land2") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.landlords",
      "L.landlords + L.landlords2"
    ) %>% as.formula
    
  } else if (thismod == "gdp2") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "L\\.lngdpcap",
      "L.lngdpcap + L.lngdpcap2"
    ) %>% as.formula
    
  } else if (thismod == "hcapXland") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.highcapratio:L.landlords"
    ) %>% as.formula
    
  } else if (thismod == "hcapXgdp") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.highcapratio:L.lngdpcap"
    ) %>% as.formula
    
  } else if (thismod == "landXgdp") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.landlords:L.lngdpcap"
    ) %>% as.formula
    
  }else if (thismod == "hcapXgini") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.highcapratio:L.gini.generous"
    ) %>% as.formula
    
  } else if (thismod == "hcapXwar") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.wars_interstate + L.highcapratio:L.wars_interstate"
    ) %>% as.formula
    
  } else if (thismod == "landXgini") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.landlords:L.gini.generous"
    ) %>% as.formula
    
  } else if (thismod == "landXwar") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.wars_interstate + L.landlords:L.wars_interstate"
    ) %>% as.formula
    
    
  } else if (thismod == "gdpXgini") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.lngdpcap:L.gini.generous"
    ) %>% as.formula
    
  } else if (thismod == "gdpXwar") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      "+ L.wars_interstate + L.lngdpcap:L.wars_interstate"
    ) %>% as.formula
    
  } else {
    
    stop(
      print(paste(thismod,"not implemented."))
    )
    
  }
  
  #############
  
  #ESTIMATION
  
  #get the df
  tmpdf<-thisdf[this.samp,]
  regvars<-all.vars(thisform)
  regvars<-c("cowcode.num","year",regvars)
  prez<-complete.cases(tmpdf[,regvars])
  tmpdf<-tmpdf[prez,regvars]
  
  #restimate
  m.tmp<-plm(
    data=tmpdf,
    form=thisform,
    model=thismodel.arg
  )
  vcov.tmp<-vcovHC(
    m.tmp,
    type="HC1",
    cluster="group"
  )
  
  ###
  #loop through keyvars, defined earlier
  returndf<-lapply(keyvars,function(thisvar) {
    #thisvar<-keyvars[2]
    #IVSD
    tmp<-sdsdf$sampname==this.sampname &
      sdsdf$var==thisvar
    if(sum(tmp)==0) {
      #if we don't have SD, calculate it
      thisvar.sd<-tapply(
        thisdf[[thisvar]],
        thisdf$cowcode.num,
        sd,na.rm=T
      ) %>% mean(na.rm=T)
    } else {
      thisvar.sd<-sdsdf$sd[tmp]
    }
    if(length(thisvar.sd)>1)
      stop()
    ####
    tmp<-str_detect(
      all.vars(thisform),
      thisvar
    )
    if(sum(tmp)>0) {
      
      #estimate w/ diff error structures
      
      #if long run
      if(thismod!="nolag") {
        returndf<-getlongrun2(
          m=m.tmp,
          vcov=vcov.tmp,
          dv=thisdv,
          iv=thisvar,
          ivsd=thisvar.sd
        )
      } else {
        #short-run
        tmpcoefs<-lmtest::coeftest(
          m.tmp,
          vcov.tmp
        )
        tmp<-str_detect(
          row.names(tmpcoefs),
          thisvar
        )
        mu<-tmpcoefs[tmp,1]
        se<-tmpcoefs[tmp,2]
        pval<-tmpcoefs[tmp,4]
        returndf<-data.frame(
          mu=mu,
          mu.min=mu-1.96*se,
          mu.max=mu+1.96*se,
          pval=pval,
          pval.class=get.pvals.class(pval),
          stringsAsFactors=F
        )
      }
    } else {
      returndf<-data.frame(
        mu=NA
      )
    }
    returndf
  }) %>% rbind.fill
  
  
  #ADD DVSD
  tmp<-sdsdf$sampname==this.sampname &
    sdsdf$var==thisdv
  if(sum(tmp)==0) {
    #if we don't have SD, calculate it
    thisdv.sd<-tapply(
      thisdf[[thisdv]],
      thisdf$cowcode.num,
      sd,na.rm=T
    ) %>% mean(na.rm=T)
  } else {
    thisdv.sd<-sdsdf$sd[tmp]
  }
  if(length(thisdv.sd)>1)
    stop()
  returndf$dvsd<-thisdv.sd
  
  #####
  
  returndf$iv<-names(keyvars)
  returndf$N<-nrow(m.tmp$model)
  returndf$N.countries<-length(unique(tmpdf$cowcode.num))
  returndf$seq<-i
  
  #####
  
  #return 
  returnlist<-list(
    returndf=returndf,
    modlist=list(
      m=m.tmp,
      robvcov=vcov.tmp
    )
  )
  return(returnlist)
})

#make estsdf
robestsdf<-lapply(tmpoutput,function(x) x$returndf) %>%
  rbind.fill
intersect(
  names(robdf),
  names(robestsdf)
)
robestsdf<-merge(
  robdf,
  robestsdf,
  by="seq"
)

#remove anything that wasn't estimated
tmp<-is.na(robestsdf$mu)
missing<-unique(robestsdf$mod[tmp])
print(missing)
robestsdf<-robestsdf[!tmp,]

# #remove the int mods, those will be elsewhere
# tmp<-rawrobdf$group=="mainint" & 
#   !is.na(rawrobdf$group)
# intmods<-rawrobdf$mname[tmp]
# robestsdf<-robestsdf[!robestsdf$mod%in%intmods,]

#standardized estimates
robestsdf$musd<-robestsdf$mu/robestsdf$dvsd
robestsdf$musd.max<-robestsdf$mu.max/robestsdf$dvsd
robestsdf$musd.min<-robestsdf$mu.min/robestsdf$dvsd

#########################################################
#########################################################

#MISC ESTS

#check alt war specs
tmp<-rawrobdf$mname%in%c(
  "istatewar",
  "allwar",
  "wardeaths"
)
waroutput<-tmpoutput[tmp]
tmpseq.i<-seq_along(waroutput)
warsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(i)
  thisoutput<-waroutput[i][[1]]
  thism<-thisoutput$modlist$m
  thisrobvcov<-thisoutput$modlist$robvcov
  #war coef?
  tmpcoefs<-lmtest::coeftest(
    thism,
    thisrobvcov
  )
  tmp<-str_detect(
    row.names(tmpcoefs),
    "war|death"
  )
  mu<-tmpcoefs[tmp,1]
  se<-tmpcoefs[tmp,2]
  pval<-tmpcoefs[tmp,4]
  returndf<-data.frame(
    mu=mu,
    mu.min=mu-1.96*se,
    mu.max=mu+1.96*se,
    pval=pval,
    pval.class=get.pvals.class(pval),
    stringsAsFactors=F
  )
  returndf$var<-row.names(tmpcoefs)[tmp]
  returndf
}) %>% rbind.fill

#########################################################
#########################################################

#INTERACTIONS

#set up key vars
thisdf<-samps$polity2.one.preferred
keyvars<-c(
  "L.lngdpcap",
  "L.highcapratio",
  "L.landlords"
)
intvars<-c(
  keyvars,
  "L.gini.generous",
  "L.wars_interstate"
)
intvars.vals<-lapply(intvars,function(thisvar) {
  #print(thisvar)
  #thisvar<-"L.gini_generous"
  if(thisvar!="L.wars_interstate") {
    vals<-seq(
      quantile(thisdf[[thisvar]],0.10),
      quantile(thisdf[[thisvar]],0.90),
      length.out=10
    )
    names(vals)<-seq(0.10,0.90,length.out=10)
    vals
  } else {
    vals<-c(
      0,1
    )
    names(vals)<-c(0,100)
  }
  vals
})
names(intvars.vals)<-intvars

#loop through
loopdf <- expand.grid(
  dv=unique(robdf$dv),
  mainvar=keyvars,
  intvar=intvars,
  stringsAsFactors=F
)

#assign each to a model, above
loopdf$mod<-rep(
  c(
    "gdp2",
    "hcapXgdp",
    "landXgdp",
    "hcapXgdp",
    "hcap2",
    "hcapXland",
    "landXgdp",
    "hcapXland",
    "land2",
    "gdpXgini",
    "hcapXgini",
    "landXgini",
    "gdpXwar",
    "hcapXwar",
    "landXwar"
  ),
  each=2
)

#loop through rows
tmpseq.i<-1:nrow(loopdf)
intestsdf<-lapply(tmpseq.i,function(i) {
  
  #i <- 13
  print(i)
  
  #get params
  thisdv<-loopdf$dv[i]
  thismname<-loopdf$mod[i]
  thismainvar<-loopdf$mainvar[i]
  thisintvar<-loopdf$intvar[i]
  sqmod<-str_detect(thismname,"2$")
  
  #####
  #get mod
  tmp<-robdf$mod==thismname & 
    robdf$dv==thisdv
  thisoutput<-tmpoutput[tmp][[1]]
  m<-thisoutput$modlist$m
  vcov<-thisoutput$modlist$robvcov
  coefs<-m$coefficients
  #get dv terms
  tmpregex<-paste0(thisdv,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  lagdv.terms<-names(coefs)[tmprows]
  #get iv terms
  tmpregex<-paste0("^",thismainvar,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  iv.terms<-names(coefs)[tmprows]
  
  #get int term
  if(sqmod) {
    tmpregex<-paste0(thisintvar,"2$")
  } else {
    tmpregex<-paste0("(:",thisintvar,")|(",thisintvar,":)")
  }
  tmprows<-str_detect(names(coefs),tmpregex)
  int.terms<-names(coefs)[tmprows]
  means<-c(
    coefs[lagdv.terms],
    coefs[iv.terms],
    coefs[int.terms]
  )
  #vcov
  rows<-row.names(vcov)%in%c(lagdv.terms,iv.terms,int.terms)
  cols<-colnames(vcov)%in%c(lagdv.terms,iv.terms,int.terms)
  vcov.useme<-vcov[rows,cols]
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  #get draws
  draws<-MASS::mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  #####
  #get lrm at diff values
  intvals<-intvars.vals[[thisintvar]]
  estsdf<-lapply(intvals,function(x) {
    if(sqmod) {
      numerator<-draws[,iv.terms] + 2 * draws[,int.terms] * x
    } else {
      numerator<-draws[,iv.terms] + draws[,int.terms] * x
    }
    denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
    lrm.distribution<-numerator/denominator
    #put this distribution in meaningful units
    #lrm.distribution<-lrm.distribution * ivsd
    returnrow<-summarize.distribution2(lrm.distribution)
  }) %>% rbind.fill
  estsdf$atval<-intvals
  estsdf$atpctile<-names(intvals)
  estsdf$mainvar<-thismainvar
  estsdf$intvar<-thisintvar
  estsdf$dv<-thisdv
  #####
  #return
  estsdf
  
}) %>% rbind.fill

#########################################################
#########################################################

#JACKNIFE

loopdf=expand.grid(
  dv=unique(modsdf$dv),
  omitter=c("cowcode.num"),
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(loopdf)

jkdf<-lapply(tmpseq.i,function(i) {
  
  #i<-3
  print("####")
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  #get params
  thisrow<-loopdf[i,]
  thisdv<-thisrow$dv
  this.omitter<-thisrow$omitter
  
  #get prep info
  tmp<-modsdf$df=="one" & 
    modsdf$dv==thisrow$dv &
    modsdf$ivspec=="preferred"
  prefmod<-unique(modsdf$mname[tmp])
  thismod<-mods[[prefmod]]
  thismod.arg<-thismod$args$model
  thisform<-thismod$formula
  tmp<-sampsdf$dv==thisrow$dv & 
    sampsdf$df=="one" &
    sampsdf$ivspec=="preferred"
  this.sampname<-sampsdf$sampname[tmp]
  thisdf<-samps[[this.sampname]]
  
  #merge in region info
  tmpvars<-c("cowcode.num","region")
  tmpdf<-demdfs$one[,tmpvars] %>% unique
  thisdf<-merge(thisdf,tmpdf,all.x=T)
  tmp<-sum(is.na(thisdf$region))
  if(tmp!=0)
    stop()
  
  #loop through and estimate,
  #each time omitting a country or region
  omits<-thisdf[[this.omitter]] %>%
    unique %>% sort
  omits<-c("none",omits)
  #omits<-c("none",omits[1])
  tmpseq.j<-seq_along(omits)
  omitmods<-lapply(tmpseq.j,function(j) {
    #j<-7
    print(
      paste(
        j,"of",length(tmpseq.j)
      )
    )
    thisomit<-omits[j]
    if(thisomit!="none") {
      tmp<-thisdf[[this.omitter]]!=thisomit
      newdf<-thisdf[tmp,]
    } else {
      newdf<-thisdf
    }
    m<-plm(
      form=thisform,
      data=newdf,
      model=thismod.arg
    )     
    
    
    vcov.tmp<-vcovHC(
      m,
      type="HC1",
      cluster="group"
    )
    list(
      m=m,
      vcov=vcov.tmp
    )
  })
  keyvars<-c(
    "lngdpcap",
    "highcapratio",
    "landlords"
  )
  omitdf<-lapply(tmpseq.j,function(j) {
    #get param
    #j<-1
    print(
      paste(j,"of",length(tmpseq.j))
    )
    mlist<-omitmods[[j]]
    returndf<-lapply(keyvars,function(thisvar) {
      #thisvar<-keyvars[1]
      tmp<-sdsdf$df=="one" &
        sdsdf$dv==thisdv &
        sdsdf$var==thisvar
      if(sum(tmp)>1) stop()
      thisvar.sd<-sdsdf$sd[tmp]
      tmp<-str_detect(
        all.vars(thisform),
        thisvar
      )
      if(sum(tmp)>0) {
        returndf<-getlongrun2(
          m=mlist$m,
          vcov=mlist$vcov,
          dv=thisdv,
          iv=thisvar,
          ivsd=thisvar.sd
        )
      } else {
        returndf<-data.frame(
          mu=NA
        )
      }
      returndf
    }) %>% rbind.fill
    returndf$iv<-keyvars
    returndf$seq<-j
    returndf$omitted<-omits[j]
    returndf$N<-mlist$m$model %>% nrow
    returndf
    
  }) %>% rbind.fill
  
  #some more info
  omitdf$dv<-thisdv
  omitdf$omitter<-this.omitter
  
  #return
  omitdf
  
}) %>% rbind.fill

#########################################################
#########################################################

#ADD DVSDS

#ROBESTS
#(already added)

#JKDF
#get dv sds
tmprows<-sdsdf$df%in%c(
  "polity2.one",
  "v2x_polyarchy.one"
) & sdsdf$var%in%c(
  "polity2",
  "v2x_polyarchy"
)
tmprows<-sdsdf$df=="one" &
  sdsdf$var%in%c(
    "polity2",
    "v2x_polyarchy"
  )
tmpnames<-c("var","sd")
tmpdf<-sdsdf[tmprows,tmpnames]
names(tmpdf)<-c("dv","dvsd")
intersect(
  names(jkdf),
  names(tmpdf)
)
jkdf<-merge(
  jkdf,
  tmpdf
)

#standardized estimates
jkdf$musd<-jkdf$mu/jkdf$dvsd
jkdf$musd.max<-jkdf$mu.max/jkdf$dvsd
jkdf$musd.min<-jkdf$mu.min/jkdf$dvsd

#########################################################
#########################################################

#FINALIZE/SAVE OUT

#don't need to save the mods
rm(tmpoutput)

#save out
setwd(filesdir)
save.image(
  "robmods.RData"
)

#########################################################
#########################################################
