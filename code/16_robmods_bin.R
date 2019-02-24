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

#########################################################
#########################################################

#any vars to add? 
demdfs<-lapply(demdfs,function(df) {
  df$ukcolony<-F
  tmp<-df$colruler=="United Kingdom"
  df$ukcolony[tmp]<-T
  df
})

#########################################################
#########################################################

#SET UP BINMODS LOOP

#add extra spec, w/hcap^2

extraspec<-c(
  specslist$preferred,
  "highcapratio2"
)
specslist[['extra']]<-extraspec

modsdf<-expand.grid(
  dv=c("tta","ttd"),
  df=c("one"),
  ivspec=names(specslist),
  stringsAsFactors=F
)
modsdf$mname<-apply(
  modsdf,1,paste0,collapse="."
)
modsdf$seq<-1:nrow(modsdf)

#########################################################
#########################################################

#GET FORMS

tmpseq.i<-1:nrow(modsdf)
forms<-lapply(tmpseq.i,function(i) {
  #i<-1
  #get params
  thisrow<-modsdf[i,]
  thisdv<-modsdf$dv[i]
  ####FORMULA
  ##LHS
  lhs<-thisdv
  ##RHS
  #region control
  thishet<-"factor(region) + factor(ukcolony) + L.breakdowns"
  #year cubic
  thisyr<-"factor(decade) + year"
  #duration dependence
  if(thisdv=="tta") {
    thisdurdep<-"t_dem + t_dem2 + t_dem3"
  } else if(thisdv=="ttd") {
    thisdurdep<-"t_dic + t_dic2 + t_dic3"
  } else {
    stop()
  }
  #ivs
  ivs<-
    paste0("L.",specslist[[thisrow$ivspec]]) %>%
    paste0(collapse=" + ")
  #TOGETHER
  rhs<-paste(
    thishet,
    thisyr,
    thisdurdep,
    ivs,
    sep=" + "
  )
  #get rid of extra + signs
  rhs<-str_replace_all(rhs,"\\+\\s+\\+","+ ") %>%
    str_replace("\\s+\\+\\s+$","") %>%
    str_replace("^\\s+\\+\\s+","")
  #RETURN
  #put the formula togeehter
  thisform<-paste(
    lhs,"~",rhs
  ) %>% as.formula
  thisform
})
names(forms)<-modsdf$mname

#########################################################
#########################################################

#GET SAMPS
#each dv/df has a sample

sampsdf<-expand.grid(
  dv=unique(modsdf$dv),
  df=c("one"),
  stringsAsFactors=F
)
sampsdf$sampname<-apply(
  sampsdf,1,paste0,collapse="."
)

tmpseq.i<-1:nrow(sampsdf)
samps<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisdv<-sampsdf$dv[i]
  thisdf<-sampsdf$df[i]
  #this.minN<-sampsdf$minN[i]
  #this is the df
  fulldf<-demdfs[[sampsdf$df[i]]]
  #these are the cols we want
  allforms<-forms[modsdf$dv==thisdv]
  allvars<-lapply(allforms,all.vars) %>%
    unlist
  #we want a df w/ these rows 
  tmp<-allvars%in%names(fulldf)
  if(sum(!tmp)>0)
    stop()
  tmprows<-complete.cases(fulldf[,allvars])
  #subset transitions/consolidations
  if(thisdv=="ttd") {
    tmprows<-tmprows & (
      fulldf$democracy==0 | fulldf$ttd==1
    )
  } else {
    tmprows<-tmprows & (
      fulldf$democracy==1 | fulldf$tta==1
    )
  }
  #these are extra vars
  idvars<-c(
    "cowcode.num",
    "year"
  )
  extravars<-c(
    #####
  )
  tmpcols<-c(
    idvars,
    extravars,
    allvars
  ) %>% unique
  tmpcols<-tmpcols[tmpcols%in%names(fulldf)]
  #so this is the df
  thisdf<-fulldf[tmprows,tmpcols]
  #return
  thisdf
})
names(samps)<-sampsdf$sampname

sampinfodf<-lapply(tmpseq.i,function(i) {
  #i<-1
  tmpdf<-samps[[i]]
  data.frame(
    N=nrow(tmpdf),
    N.states=length(unique(tmpdf$cowcode.num)),
    range=paste0(
      min(tmpdf$year),"-",max(tmpdf$year)
    )
  )
}) %>% rbind.fill

#sampsdf
sampsdf<-cbind(
  sampsdf,
  sampinfodf
)
sampsdf

#########################################################
#########################################################

#RUN REGS
this.sequence<-seq_along(forms)
mods<-lapply(this.sequence,function(i) {
  #i<-11
  #progress
  print(
    paste(
      "Estimating model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  thisform<-forms[[i]]
  thisdv<-modsdf$dv[i]
  thisdf<-modsdf$df[i]
  tmp<-sampsdf$dv==thisdv & 
    sampsdf$df==thisdf
  this.sampname<-sampsdf$sampname[tmp]
  thisdf<-samps[[this.sampname]]
  #estimate
  m.tmp<-glm(
    data=thisdf,
    formula=thisform,
    family='binomial'
  )
})
names(mods)<-modsdf$mname

#########################################################
#########################################################

#GET KEY ESTS
tmpseq.i<-seq_along(mods) 
estsdf<-lapply(tmpseq.i,function(i) {
  #i<-11
  thismod<-mods[[i]]
  coefests<-lmtest::coeftest(thismod)
  coefs<-row.names(coefests)
  keyvars<-c(
    "lngdpcap",
    "highcapratio",
    "highcapratio2",
    "landlords"
  )
  returndf<-lapply(keyvars,function(thisvar) {
    #thisvar<-keyvars[3]
    tmpregex<-paste0(thisvar,"$")
    tmp<-str_detect(
      coefs,
      tmpregex
    )
    if(sum(tmp)==1) {
      returnrow<-as.data.frame(coefests[tmp,] %>% t)
      names(returnrow)<-c(
        "mu",
        "se",
        "z",
        "pval"
      )
    } else {
      returnrow<-data.frame(mu=NA)
    }
    returnrow
  }) %>% rbind.fill
  returndf$iv<-keyvars
  returndf$seq<-i
  #remove non-estimates
  tmp<-!is.na(returndf$mu)
  returndf<-returndf[tmp,]
  #return
  returndf
}) %>% rbind.fill

#########################################################
#########################################################

#GET MODEL FIT
tmpseq.i<-seq_along(mods) 
fitdf<-lapply(tmpseq.i,function(i) {
  thismod<-mods[[i]]
  data.frame(
    seq=i,
    aic=thismod$aic
  )
}) %>% rbind.fill
fitdf<-merge(modsdf,fitdf,by='seq')

#merge mod info
#identify prefererred models
fitdf<-by(fitdf,fitdf$dv,function(df) {
  #df<-finaldf[finaldf$dv=="tta",]
  df<-df[order(df$aic),]
  df$rank<-1:nrow(df)
  df$pref<-F
  df$pref[df$rank==1]<-T
  df
}) %>% rbind.fill

#identify models for display
dispmods<-c(
  "tta.one.preferred",
  "tta.one.extra",
  "ttd.one.preferred",
  "ttd.one.extra"
)
fitdf$disp<-F
tmp<-fitdf$mname%in%dispmods
fitdf$disp[tmp]<-T

#merge est info
binfinaldf<-merge(fitdf,estsdf,by='seq')

#add pval class
binfinaldf$pval.class<-get.pvals.class(
  binfinaldf$pval
)

#########################################################
#########################################################

#PREDICTIONS

landlords<-seq(
  80,20,-10
)
highcapratio<-seq(
  5,35,5
)

predictdf<-expand.grid(
  L.landlords=landlords,
  L.highcapratio=highcapratio,
  L.lngdpcap=mean(demdfs$one$L.lngdpcap,na.rm=T),
  L.urban02=mean(demdfs$one$L.urban02,na.rm=T),
  L.years.all=mean(demdfs$one$L.years.all,na.rm=T),
  L.gdpgr=mean(demdfs$one$L.gdpgr,na.rm=T),
  L.gini.generous=mean(demdfs$one$L.gini.generous,na.rm=T),
  region_num=2,
  region="Latin America",
  L.breakdowns=1,
  ukcolony=T,
  colrulername="United Kingdom",
  decade=1980,
  year=1980,
  t_dic=5,
  t_dic2=25,
  t_dic3=125,
  t_dem=5,
  t_dem2=25,
  t_dem3=125,
  stringsAsFactors=F
)
predictdf$L.highcapratio2 <-
  predictdf$L.highcapratio * 
  predictdf$L.highcapratio

#loop through each model
tmpseq.i<-seq_along(mods)
phatdf<-lapply(tmpseq.i,function(i) {
  #i<-7
  thismod<-mods[[i]]
  phat<-predict(
    thismod,
    newdata=predictdf,
    type='response'
  )
  tmpdf<-cbind(
    predictdf,
    phat
  )
  tmpdf$seq<-i
  tmpdf
}) %>% rbind.fill


#merge in mod/fit info
intersect(
  names(phatdf),
  names(fitdf)
)
phatdf<-merge(
  phatdf,
  fitdf,
  by='seq'
)

#we want to render phat as 
#p(trans) in next ten years
tmp<-phatdf$dv=="ttd"
phatdf$p10[tmp] <- 1 - (1-phatdf$phat[tmp])^10
#p(coup) in next ten years
tmp<-phatdf$dv=="tta"
phatdf$p10[tmp] <- 1 - (1 - phatdf$phat[tmp])^10

#########################################################
#########################################################

#SAVE OUT

#save out
setwd(filesdir)
save.image(
  "robmods_bin.RData"
)

#########################################################
#########################################################
